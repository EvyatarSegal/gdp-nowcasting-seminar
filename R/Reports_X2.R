# ==============================================================================
# 11. ENSEMBLE NOWCAST CALCULATION (XGBOOST + DFM)
# ==============================================================================
library(ggplot2)
library(dplyr)
library(openxlsx)

# 1. Define ensemble weights (conditional for Shiny or external override)
if (!exists("w_xgb")) w_xgb <- 0.60
if (!exists("w_dfm")) w_dfm <- 0.40

# 2. Extract DFM Loadings and target scaling parameters
gdp_mean <- mean(df_sub$GDP, na.rm = TRUE)
gdp_sd   <- sd(df_sub$GDP, na.rm = TRUE)

loadings_mat <- dfm_curr$C
var_names    <- setdiff(colnames(df_sub), "Date")
rownames(loadings_mat) <- var_names

# Extract how GDP loads onto the 4 factors
gdp_loadings <- loadings_mat["GDP", 1:4]

# 3. Calculate DFM Nowcast directly: X = (C * F) * sd + mean
dfm_gdp_std     <- sum(gdp_loadings * current_factors[1, 1:4])
dfm_gdp_nowcast <- (dfm_gdp_std * gdp_sd) + gdp_mean

# 4. Calculate the weighted ensemble nowcast
ensemble_gdp_nowcast <- (w_xgb * current_gdp_nowcast) + (w_dfm * dfm_gdp_nowcast)

# 5. Apply to base level
nowcast_gdp_level_ens <- base_level * (1 + ensemble_gdp_nowcast)

cat("\n========== ENSEMBLE NOWCAST ==========\n")
cat(sprintf("XGBoost Nowcast: %.4f\n", current_gdp_nowcast))
cat(sprintf("DFM Nowcast:     %.4f\n", dfm_gdp_nowcast))
cat(sprintf("Ensemble Growth: %.4f (%.2f%%)\n", ensemble_gdp_nowcast, ensemble_gdp_nowcast * 100))
cat(sprintf("Ensemble GDP Level: %.2f\n", nowcast_gdp_level_ens))
cat("======================================\n")


# ==============================================================================
# 12. CURRENT QUARTER ATTRIBUTION (EXACT LINEAR ALGEBRA PROJECTION)
# ==============================================================================

# --- A. Calculate Factor-Level Impacts ---
# 1. XGBoost SHAP values for the factors
shap_matrix <- predict(xgb_model_final, current_factors, predcontrib = TRUE)
shap_vals   <- as.numeric(shap_matrix)[1:4]

# 2. DFM Factor Impacts (Linear contribution to the unscaled target)
dfm_factor_impacts <- gdp_loadings * current_factors[1, 1:4] * gdp_sd

# 3. Blended Ensemble Factor Impact
ensemble_factor_impacts <- (w_xgb * shap_vals) + (w_dfm * dfm_factor_impacts)

# --- B. Distribute Factor Impacts to Variables (GLS Projection) ---
loadings_drivers <- loadings_mat[rownames(loadings_mat) != "GDP", ]

# 1. Standardize raw drivers (mean = 0, sd = 1)
raw_drivers    <- df_sub[, rownames(loadings_drivers)]
scaled_drivers <- scale(raw_drivers)
current_X      <- as.numeric(scaled_drivers[nrow(scaled_drivers), ])
names(current_X) <- rownames(loadings_drivers)

# 2. Handle Ragged Edges (NAs): Impute missing current data using DFM state
dfm_implied_X <- as.numeric(loadings_drivers %*% t(current_factors))
current_X[is.na(current_X)] <- dfm_implied_X[is.na(current_X)]

# 3. Calculate Projection Matrix W = (C'C)^(-1) C' (4 factors x N variables)
C_mat <- loadings_drivers
W_mat <- solve(t(C_mat) %*% C_mat) %*% t(C_mat)

# 4. Calculate exact contributions for today's data
var_contributions <- numeric(nrow(loadings_drivers))
names(var_contributions) <- rownames(loadings_drivers)

for (k in 1:4) {
  f_impact <- ensemble_factor_impacts[k]
  raw_var_to_factor <- W_mat[k, ] * current_X
  sum_raw <- sum(raw_var_to_factor, na.rm = TRUE)
  
  rel_weights <- if (abs(sum_raw) < 1e-12) rep(0, length(current_X)) else (raw_var_to_factor / sum_raw)
  var_contributions <- var_contributions + (rel_weights * f_impact)
}

# --- C. Map Variables Back to Sectors (Blocks) ---
block_map_list <- lapply(names(blocks_shifted), function(b_name) {
  if (!b_name %in% c("adjusters", "target")) {
    data.frame(
      Sector = b_name,
      Variable = setdiff(names(blocks_shifted[[b_name]]), "Date"),
      stringsAsFactors = FALSE
    )
  }
})
var_Sector_map <- bind_rows(block_map_list)

# --- D. Build Final Current DataFrames ---
news_report <- data.frame(
  Variable = names(var_contributions),
  Ensemble_Impact = var_contributions,
  stringsAsFactors = FALSE
) %>%
  left_join(var_Sector_map, by = "Variable") %>%
  mutate(
    Impact_in_Percent = Ensemble_Impact * 100,
    Relative_Share_Pct = (abs(Ensemble_Impact) / sum(abs(Ensemble_Impact), na.rm = TRUE)) * 100
  ) %>%
  arrange(desc(abs(Ensemble_Impact)))

Sector_report <- news_report %>%
  group_by(Sector) %>%
  summarise(
    Total_Impact = sum(Ensemble_Impact, na.rm = TRUE),
    Impact_in_Percent = sum(Impact_in_Percent, na.rm = TRUE),
    Relative_Share_Pct = sum(Relative_Share_Pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(Total_Impact)))


# ==============================================================================
# 12.1 VISUALIZE CURRENT QUARTER NEWS REPORT
# ==============================================================================

# Plot 1: Macro Impact by Sector (Current Quarter)
p_Sector <- ggplot(Sector_report, aes(x = reorder(Sector, Total_Impact), y = Total_Impact, fill = Total_Impact > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#15803D", "FALSE" = "#B91C1C")) +
  theme_minimal() +
  labs(
    title = "Current GDP Nowcast Drivers by Sector",
    subtitle = "Combined XGBoost SHAP + DFM Linear Projection",
    x = "Sector", y = "Contribution to GDP Growth Rate"
  ) +
  theme(legend.position = "none")

print(p_Sector)

# Plot 2: Top 15 Individual Variable Impacts (Current Quarter)
top_vars <- news_report %>% 
  arrange(desc(abs(Ensemble_Impact))) %>% 
  head(15)

p_vars <- ggplot(top_vars, aes(x = reorder(Variable, Ensemble_Impact), y = Ensemble_Impact, fill = Ensemble_Impact > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#15803D", "FALSE" = "#B91C1C")) +
  theme_minimal() +
  labs(
    title = "Top 15 Variable Drivers (Current GDP Nowcast)",
    subtitle = "Apportioned via DFM Linear Projection Matrix W",
    x = "Variable Name", y = "Contribution to GDP Growth Rate"
  ) +
  theme(legend.position = "none")

print(p_vars)


# ==============================================================================
# 12.5 PREVIOUS QUARTER ATTRIBUTION (ROBUST HISTORICAL EXTRACTION)
# ==============================================================================
cat("\nCalculating previous quarter attribution for historical comparison...\n")

# Offset: 3 months back for monthly dataset (change to 1 if dataset is quarterly)
row_offset <- 3

# 1. Extract historical factor matrix using exact slot names from dfm_curr
factors_hist <- if (!is.null(dfm_curr$F_2s)) {
  dfm_curr$F_2s
} else if (!is.null(dfm_curr$F_qml)) {
  dfm_curr$F_qml
} else if (!is.null(dfm_curr$F_pca)) {
  dfm_curr$F_pca
} else if (!is.null(dfm_curr$F)) {
  dfm_curr$F
} else {
  stop("Factors matrix not found in dfm_curr object.")
}

factors_hist <- as.matrix(factors_hist)

# 2. Extract row for previous quarter as a 1x4 matrix
prev_factors <- matrix(factors_hist[nrow(factors_hist) - row_offset, 1:4], nrow = 1)

# 3. Align column names with current_factors for XGBoost prediction
if (exists("current_factors") && !is.null(colnames(current_factors))) {
  colnames(prev_factors) <- colnames(current_factors)
}

# 4. Predict factor impacts for previous quarter
shap_matrix_prev             <- predict(xgb_model_final, prev_factors, predcontrib = TRUE)
shap_vals_prev               <- as.numeric(shap_matrix_prev)[1:4]
dfm_factor_impacts_prev      <- gdp_loadings * as.numeric(prev_factors) * gdp_sd
ensemble_factor_impacts_prev <- (w_xgb * shap_vals_prev) + (w_dfm * dfm_factor_impacts_prev)

# 5. Extract and scale drivers for previous quarter
prev_X <- as.numeric(scaled_drivers[nrow(scaled_drivers) - row_offset, ])
names(prev_X) <- rownames(loadings_drivers)

# Impute missing values for previous quarter if any
dfm_implied_X_prev <- as.numeric(loadings_drivers %*% t(prev_factors))
prev_X[is.na(prev_X)] <- dfm_implied_X_prev[is.na(prev_X)]

# 6. Apportion factor impacts to variables for previous quarter
prev_var_contributions <- numeric(nrow(loadings_drivers))
names(prev_var_contributions) <- rownames(loadings_drivers)

for (k in 1:4) {
  raw_var_to_factor_prev <- W_mat[k, ] * prev_X
  sum_raw_prev           <- sum(raw_var_to_factor_prev, na.rm = TRUE)
  rel_weights_prev       <- if (abs(sum_raw_prev) < 1e-12) rep(0, length(prev_X)) else (raw_var_to_factor_prev / sum_raw_prev)
  prev_var_contributions <- prev_var_contributions + (rel_weights_prev * ensemble_factor_impacts_prev[k])
}

# 7. Build summary dataframes for previous quarter
prev_news_report <- data.frame(
  Variable = names(prev_var_contributions),
  Prev_Impact = prev_var_contributions,
  stringsAsFactors = FALSE
) %>% 
  left_join(var_Sector_map, by = "Variable") %>%
  mutate(
    Impact_in_Percent = Prev_Impact * 100,
    Relative_Share_Pct = (abs(Prev_Impact) / sum(abs(Prev_Impact), na.rm = TRUE)) * 100
  ) %>%
  arrange(desc(abs(Prev_Impact)))

prev_Sector_report <- prev_news_report %>%
  group_by(Sector) %>%
  summarise(
    Prev_Total_Impact = sum(Prev_Impact, na.rm = TRUE),
    Impact_in_Percent = sum(Impact_in_Percent, na.rm = TRUE),
    Relative_Share_Pct = sum(Relative_Share_Pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(Prev_Total_Impact)))


# ==============================================================================
# 12.6 VISUALIZE PREVIOUS QUARTER FORECAST DRIVERS (NEW)
# ==============================================================================

# Plot 3: Sector Impact for Previous Quarter
p_Sector_prev <- ggplot(prev_Sector_report, aes(x = reorder(Sector, Prev_Total_Impact), y = Prev_Total_Impact, fill = Prev_Total_Impact > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#15803D", "FALSE" = "#B91C1C")) +
  theme_minimal() +
  labs(
    title = "Previous Quarter GDP Nowcast Drivers by Sector",
    subtitle = "Historical Sector Attribution Baseline",
    x = "Sector", y = "Contribution to GDP Growth Rate"
  ) +
  theme(legend.position = "none")

print(p_Sector_prev)

# Plot 4: Top 15 Individual Variable Impacts for Previous Quarter
top_vars_prev <- prev_news_report %>% 
  arrange(desc(abs(Prev_Impact))) %>% 
  head(15)

p_vars_prev <- ggplot(top_vars_prev, aes(x = reorder(Variable, Prev_Impact), y = Prev_Impact, fill = Prev_Impact > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#15803D", "FALSE" = "#B91C1C")) +
  theme_minimal() +
  labs(
    title = "Top 15 Variable Drivers (Previous GDP Nowcast)",
    subtitle = "Historical Variable Attribution Baseline",
    x = "Variable Name", y = "Contribution to GDP Growth Rate"
  ) +
  theme(legend.position = "none")

print(p_vars_prev)


# ==============================================================================
# 13. EXECUTIVE EXCEL REPORT (WITH PREVIOUS QUARTER CHARTS & FORMATTING)
# ==============================================================================
cat("Generating comprehensive Excel report with current & historical charts...\n")

# Prepare export objects
news_export <- news_report %>%
  mutate(
    `Impact (%)` = Ensemble_Impact,
    `Relative Share (%)` = abs(Ensemble_Impact) / sum(abs(Ensemble_Impact), na.rm = TRUE)
  ) %>%
  dplyr::select(Variable, Sector, `Total Impact` = Ensemble_Impact, `Impact (%)`, `Relative Share (%)`)

Sector_export <- Sector_report %>%
  mutate(
    `Impact (%)` = Total_Impact,
    `Relative Share (%)` = abs(Total_Impact) / sum(abs(Total_Impact), na.rm = TRUE)
  ) %>%
  dplyr::select(Sector, `Total Impact` = Total_Impact, `Impact (%)`, `Relative Share (%)`)

prev_Sector_export <- prev_Sector_report %>%
  mutate(
    `Impact (%)` = Prev_Total_Impact,
    `Relative Share (%)` = abs(Prev_Total_Impact) / sum(abs(Prev_Total_Impact), na.rm = TRUE)
  ) %>%
  dplyr::select(Sector, `Total Impact` = Prev_Total_Impact, `Impact (%)`, `Relative Share (%)`)

prev_news_export <- prev_news_report %>%
  mutate(
    `Total Impact` = Prev_Impact,
    `Impact (%)` = Prev_Impact,
    `Relative Share (%)` = abs(Prev_Impact) / sum(abs(Prev_Impact), na.rm = TRUE)
  ) %>%
  dplyr::select(Variable, Sector, `Total Impact`, `Impact (%)`, `Relative Share (%)`)

# Initialize Workbook
wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 11, fontName = "Calibri")

# Reusable Styles
header_style <- createStyle(fontSize = 12, fontColour = "#FFFFFF", halign = "center", valign = "center", fgFill = "#1E3A8A", textDecoration = "bold", border = "Bottom")
pct_style    <- createStyle(numFmt = "0.00%", halign = "right")
float_style  <- createStyle(numFmt = "0.00000", halign = "right")
large_num    <- createStyle(numFmt = "#,##0.00", halign = "right")
pos_style    <- createStyle(fontColour = "#15803D") # Emerald Green
neg_style    <- createStyle(fontColour = "#B91C1C") # Crimson Red

# --- TAB 1: EXECUTIVE DASHBOARD ---
addWorksheet(wb, "Dashboard")
showGridLines(wb, "Dashboard", showGridLines = FALSE)
setColWidths(wb, "Dashboard", cols = 1, widths = 3)

summary_df <- data.frame(
  Metric = c("XGBoost Nowcast", "DFM Nowcast", "Ensemble Growth", "Ensemble GDP Level"),
  Value  = c(current_gdp_nowcast, dfm_gdp_nowcast, ensemble_gdp_nowcast, nowcast_gdp_level_ens)
)

writeData(wb, "Dashboard", summary_df, startRow = 2, startCol = 2, headerStyle = header_style, borders = "rows")
addStyle(wb, "Dashboard", style = pct_style, rows = 3:5, cols = 3, gridExpand = TRUE)
addStyle(wb, "Dashboard", style = large_num, rows = 6, cols = 3, gridExpand = TRUE)
setColWidths(wb, "Dashboard", cols = 2, widths = 25)
setColWidths(wb, "Dashboard", cols = 3, widths = 20)

# Insert Current Quarter Plots
print(p_Sector) 
insertPlot(wb, "Dashboard", width = 8, height = 4.5, xy = c("E", 2)) 
print(p_vars) 
insertPlot(wb, "Dashboard", width = 8, height = 5.5, xy = c("E", 26)) 

# Insert Previous Quarter Plots into Dashboard
print(p_Sector_prev)
insertPlot(wb, "Dashboard", width = 8, height = 4.5, xy = c("N", 2))
print(p_vars_prev)
insertPlot(wb, "Dashboard", width = 8, height = 5.5, xy = c("N", 26))

# --- TAB 2: CURRENT SECTOR IMPACT ---
addWorksheet(wb, "Sector Impact")
showGridLines(wb, "Sector Impact", showGridLines = FALSE)
writeData(wb, "Sector Impact", Sector_export, startRow = 1, startCol = 1, headerStyle = header_style, borders = "rows")
setColWidths(wb, "Sector Impact", cols = c(1, 2, 3, 4), widths = c(35, 20, 20, 20))
addStyle(wb, "Sector Impact", style = float_style, rows = 2:(nrow(Sector_export) + 1), cols = 2, gridExpand = TRUE)
addStyle(wb, "Sector Impact", style = pct_style, rows = 2:(nrow(Sector_export) + 1), cols = 3:4, gridExpand = TRUE)
conditionalFormatting(wb, "Sector Impact", cols = 2:3, rows = 2:(nrow(Sector_export) + 1), rule = ">0", style = pos_style)
conditionalFormatting(wb, "Sector Impact", cols = 2:3, rows = 2:(nrow(Sector_export) + 1), rule = "<0", style = neg_style)

# --- TAB 3: CURRENT VARIABLE IMPACT ---
addWorksheet(wb, "Variable Impact")
showGridLines(wb, "Variable Impact", showGridLines = FALSE)
writeData(wb, "Variable Impact", news_export, startRow = 1, startCol = 1, headerStyle = header_style, borders = "rows")
setColWidths(wb, "Variable Impact", cols = c(1, 2, 3, 4, 5), widths = c(45, 25, 20, 20, 20))
addStyle(wb, "Variable Impact", style = float_style, rows = 2:(nrow(news_export) + 1), cols = 3, gridExpand = TRUE)
addStyle(wb, "Variable Impact", style = pct_style, rows = 2:(nrow(news_export) + 1), cols = 4:5, gridExpand = TRUE)
conditionalFormatting(wb, "Variable Impact", cols = 3:4, rows = 2:(nrow(news_export) + 1), rule = ">0", style = pos_style)
conditionalFormatting(wb, "Variable Impact", cols = 3:4, rows = 2:(nrow(news_export) + 1), rule = "<0", style = neg_style)

# --- TAB 4: SECTOR DELTA VS PREVIOUS QUARTER ---
addWorksheet(wb, "Sector Delta vs Prev")
showGridLines(wb, "Sector Delta vs Prev", showGridLines = FALSE)

comp_df <- Sector_export %>%
  left_join(prev_Sector_report, by = "Sector") %>%
  mutate(
    `Prev Impact`    = ifelse(is.na(Prev_Total_Impact), 0, Prev_Total_Impact),
    `Change (Delta)` = `Total Impact` - `Prev Impact`
  ) %>%
  dplyr::select(Sector, `Current Impact` = `Total Impact`, `Prev Impact`, `Change (Delta)`) %>%
  arrange(desc(abs(`Change (Delta)`)))

writeData(wb, "Sector Delta vs Prev", comp_df, startRow = 1, startCol = 1, headerStyle = header_style, borders = "rows")
setColWidths(wb, "Sector Delta vs Prev", cols = c(1, 2, 3, 4), widths = c(35, 20, 20, 20))
addStyle(wb, "Sector Delta vs Prev", style = float_style, rows = 2:(nrow(comp_df) + 1), cols = 2:4, gridExpand = TRUE)
conditionalFormatting(wb, "Sector Delta vs Prev", cols = 2:4, rows = 2:(nrow(comp_df) + 1), rule = ">0", style = pos_style)
conditionalFormatting(wb, "Sector Delta vs Prev", cols = 2:4, rows = 2:(nrow(comp_df) + 1), rule = "<0", style = neg_style)

# --- TAB 5: PREVIOUS QUARTER SECTOR IMPACT ---
addWorksheet(wb, "Prev Qtr Sector")
showGridLines(wb, "Prev Qtr Sector", showGridLines = FALSE)
writeData(wb, "Prev Qtr Sector", prev_Sector_export, startRow = 1, startCol = 1, headerStyle = header_style, borders = "rows")
setColWidths(wb, "Prev Qtr Sector", cols = c(1, 2, 3, 4), widths = c(35, 20, 20, 20))
addStyle(wb, "Prev Qtr Sector", style = float_style, rows = 2:(nrow(prev_Sector_export) + 1), cols = 2, gridExpand = TRUE)
addStyle(wb, "Prev Qtr Sector", style = pct_style, rows = 2:(nrow(prev_Sector_export) + 1), cols = 3:4, gridExpand = TRUE)
conditionalFormatting(wb, "Prev Qtr Sector", cols = 2:3, rows = 2:(nrow(prev_Sector_export) + 1), rule = ">0", style = pos_style)
conditionalFormatting(wb, "Prev Qtr Sector", cols = 2:3, rows = 2:(nrow(prev_Sector_export) + 1), rule = "<0", style = neg_style)

# --- TAB 6: PREVIOUS QUARTER VARIABLE IMPACT ---
addWorksheet(wb, "Prev Qtr Variable")
showGridLines(wb, "Prev Qtr Variable", showGridLines = FALSE)
writeData(wb, "Prev Qtr Variable", prev_news_export, startRow = 1, startCol = 1, headerStyle = header_style, borders = "rows")
setColWidths(wb, "Prev Qtr Variable", cols = c(1, 2, 3, 4, 5), widths = c(45, 25, 20, 20, 20))
addStyle(wb, "Prev Qtr Variable", style = float_style, rows = 2:(nrow(prev_news_export) + 1), cols = 3, gridExpand = TRUE)
addStyle(wb, "Prev Qtr Variable", style = pct_style, rows = 2:(nrow(prev_news_export) + 1), cols = 4:5, gridExpand = TRUE)
conditionalFormatting(wb, "Prev Qtr Variable", cols = 3:4, rows = 2:(nrow(prev_news_export) + 1), rule = ">0", style = pos_style)
conditionalFormatting(wb, "Prev Qtr Variable", cols = 3:4, rows = 2:(nrow(prev_news_export) + 1), rule = "<0", style = neg_style)

# Save Workbook
output_file <- "data/clean/Nowcast_Executive_Report_X2.xlsx"
saveWorkbook(wb, output_file, overwrite = TRUE)

cat(sprintf("\n✓ Executive Excel Report generated successfully: %s\n", output_file))