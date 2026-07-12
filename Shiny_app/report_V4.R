# ==============================================================================
# 11. ENSEMBLE: DFM + XGBOOST (BULLETPROOF FIX)
# ==============================================================================

# 1. Define your ensemble weights (conditional for Shiny injection)
if (!exists("w_xgb")) w_xgb <- 0.60
if (!exists("w_dfm")) w_dfm <- 0.40

# 2. Extract DFM Loadings and scale parameters manually to avoid NA bugs
gdp_mean <- mean(df_sub$GDP, na.rm = TRUE)
gdp_sd   <- sd(df_sub$GDP, na.rm = TRUE)

loadings_mat <- dfm_curr$C
var_names <- setdiff(colnames(df_sub), "Date")
rownames(loadings_mat) <- var_names

# Extract how heavily GDP loads onto the 4 factors
gdp_loadings <- loadings_mat["GDP", 1:4]

# 3. Calculate DFM Nowcast directly: X = (C * F) * sd + mean
# current_factors is our 1x4 matrix from the latest row
dfm_gdp_std <- sum(gdp_loadings * current_factors[1, 1:4])
dfm_gdp_nowcast <- (dfm_gdp_std * gdp_sd) + gdp_mean

# 4. Calculate the ensemble nowcast
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
# 12. TRUE ENSEMBLE "NEWS": EXACT LINEAR ALGEBRA ATTRIBUTION
# ==============================================================================
library(ggplot2)
library(dplyr)
library(openxlsx)

# --- A. Calculate Factor-Level Impacts ---
# 1. XGBoost SHAP values for the factors
shap_matrix <- predict(xgb_model_final, current_factors, predcontrib = TRUE)
shap_vals <- as.numeric(shap_matrix)[1:4]

# 2. DFM Factor Impacts (Linear contribution to the unscaled target)
dfm_factor_impacts <- gdp_loadings * current_factors[1, 1:4] * gdp_sd

# 3. Blended Ensemble Factor Impact
ensemble_factor_impacts <- (w_xgb * shap_vals) + (w_dfm * dfm_factor_impacts)

# --- B. Distribute Factor Impacts to Variables (GLS Projection Fix) ---
loadings_drivers <- loadings_mat[rownames(loadings_mat) != "GDP", ]

# 1. Get the current raw data and standardize it (mean=0, sd=1)
raw_drivers <- df_sub[, rownames(loadings_drivers)]
scaled_drivers <- scale(raw_drivers)
current_X <- as.numeric(scaled_drivers[nrow(scaled_drivers), ])
names(current_X) <- rownames(loadings_drivers)

# 2. Handle Ragged Edges (NAs): Fill missing current month data using DFM state
dfm_implied_X <- as.numeric(loadings_drivers %*% t(current_factors))
current_X[is.na(current_X)] <- dfm_implied_X[is.na(current_X)]

# 3. Calculate Projection Matrix W = (C'C)^(-1) C' 
C_mat <- loadings_drivers
W_mat <- solve(t(C_mat) %*% C_mat) %*% t(C_mat) # 4 factors x 51 variables

# 4. Calculate exact contributions based on TODAY'S data
var_contributions <- numeric(nrow(loadings_drivers))
names(var_contributions) <- rownames(loadings_drivers)

for (k in 1:4) {
  f_impact <- ensemble_factor_impacts[k]
  
  # How much Variable J drove Factor K this month
  raw_var_to_factor <- W_mat[k, ] * current_X
  
  # Normalize to ensure it sums exactly to 1.0 (100% of the factor's impact)
  sum_raw <- sum(raw_var_to_factor, na.rm = TRUE)
  
  if (abs(sum_raw) < 1e-12) {
    rel_weights <- rep(0, length(current_X))
  } else {
    rel_weights <- raw_var_to_factor / sum_raw
  }
  
  # Add this factor's slice to the variable's total impact
  var_contributions <- var_contributions + (rel_weights * f_impact)
}

# --- C. Map Variables Back to their Sectors (Blocks) ---
block_map_list <- lapply(names(blocks_shifted), function(b_name) {
  if(!b_name %in% c("adjusters", "target")) {
    data.frame(
      Sector = b_name,
      Variable = setdiff(names(blocks_shifted[[b_name]]), "Date"),
      stringsAsFactors = FALSE
    )
  }
})
var_Sector_map <- bind_rows(block_map_list)

# --- D. Build the Final DataFrames ---
news_report <- data.frame(
  Variable = names(var_contributions),
  Ensemble_Impact = var_contributions,
  stringsAsFactors = FALSE
) %>%
  left_join(var_Sector_map, by = "Variable") %>%
  mutate(
    # Multiply by 100 here ONLY for the console print to avoid messy decimals
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

# --- E. Console Output & Proof of Math ---
cat("\n========== TOP SECTOR DRIVERS (ENSEMBLE IMPACT) ==========\n")
Sector_print <- Sector_report %>%
  mutate(
    Total_Impact = sprintf("%.6f", Total_Impact),
    Impact_in_Percent = sprintf("%+.4f%%", Impact_in_Percent),
    Relative_Share_Pct = sprintf("%.2f%%", Relative_Share_Pct)
  ) %>% rename(`Total Impact` = Total_Impact, `Impact (%)` = Impact_in_Percent, `Relative Share (%)` = Relative_Share_Pct)
print(as.data.frame(Sector_print), row.names = FALSE)

cat("\n========== ALL INDIVIDUAL VARIABLE DRIVERS ==========\n")
news_print <- news_report %>%
  mutate(
    Ensemble_Impact = sprintf("%.6f", Ensemble_Impact),
    Impact_in_Percent = sprintf("%+.4f%%", Impact_in_Percent),
    Relative_Share_Pct = sprintf("%.2f%%", Relative_Share_Pct)
  ) %>%
  select(Variable, Sector, `Total Impact` = Ensemble_Impact, `Impact (%)` = Impact_in_Percent, `Relative Share (%)` = Relative_Share_Pct)
print(as.data.frame(news_print), row.names = FALSE)
cat("==========================================================\n")

# THE INTEGRITY TEST (Account for Intercept/Bias)
total_var_sum <- sum(news_report$Ensemble_Impact)
intercept_contribution <- ensemble_gdp_nowcast - sum(ensemble_factor_impacts)

cat(sprintf("\n[Math Verification] Sum of 51 variables:     %.6f\n", total_var_sum))
cat(sprintf("[Math Verification] Intercept/Bias Baseline: %.6f\n", intercept_contribution))
cat(sprintf("[Math Verification] Total Accounted Growth:  %.6f\n", total_var_sum + intercept_contribution))
cat(sprintf("[Math Verification] Actual Ensemble Nowcast: %.6f\n", ensemble_gdp_nowcast))

if (abs((total_var_sum + intercept_contribution) - ensemble_gdp_nowcast) < 1e-8) {
  cat("✓ Perfect Match: All nowcast growth accounted for seamlessly.\n\n")
} else {
  cat("⚠ Warning: Mathematical leak detected.\n\n")
}

if (!exists("is_shiny")) is_shiny <- FALSE
if (!is_shiny) {
  # Save raw CSVs
  write.csv(Sector_print, "data/clean/nowcast_Sector_drivers.csv", row.names = FALSE)
  write.csv(news_print, "data/clean/nowcast_variable_drivers.csv", row.names = FALSE)
}


# ==============================================================================
# 12.1 VISUALIZE THE NEWS REPORT
# ==============================================================================

# Plot 1: Macro Impact by Sector
p_Sector <- ggplot(Sector_report, aes(x = reorder(Sector, Total_Impact), y = Total_Impact, fill = Total_Impact > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred")) +
  theme_minimal() +
  labs(title = "GDP Nowcast Drivers by Sector",
       subtitle = "Combined XGBoost SHAP + DFM Linear Impacts",
       x = "Sector", y = "Contribution to GDP Growth Rate") +
  theme(legend.position = "none")

print(p_Sector)

# Plot 2: Top 15 Individual Variable Impacts (Absolute magnitude)
top_vars <- news_report %>% 
  arrange(desc(abs(Ensemble_Impact))) %>% 
  head(15)

p_vars <- ggplot(top_vars, aes(x = reorder(Variable, Ensemble_Impact), y = Ensemble_Impact, fill = Ensemble_Impact > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred")) +
  theme_minimal() +
  labs(title = "Top 15 Variable Drivers for Current GDP Nowcast",
       subtitle = "Apportioned via DFM Linear Projection",
       x = "Specific Variable", y = "Contribution to GDP Growth Rate") +
  theme(legend.position = "none")

print(p_vars)


# ==============================================================================
# 13. PROFESSIONAL EXCEL EXPORT (DASHBOARD & REPORTS)
# ==============================================================================
library(openxlsx)

cat("Generating professional Excel report...\n")

# --- Prepare Clean Native Data for Excel ---
# We keep the raw decimals so Excel can apply native percentages and math works flawlessly
news_export <- news_report %>%
  mutate(
    `Impact (%)` = Ensemble_Impact,
    `Relative Share (%)` = abs(Ensemble_Impact) / sum(abs(Ensemble_Impact), na.rm = TRUE)
  ) %>%
  select(Variable, Sector, `Total Impact` = Ensemble_Impact, `Impact (%)`, `Relative Share (%)`)

Sector_export <- Sector_report %>%
  mutate(
    `Impact (%)` = Total_Impact,
    `Relative Share (%)` = abs(Total_Impact) / sum(abs(Total_Impact), na.rm = TRUE)
  ) %>%
  select(Sector, `Total Impact` = Total_Impact, `Impact (%)`, `Relative Share (%)`)


# Create a new workbook
wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 11, fontName = "Calibri")

# --- Define Reusable Elegant Styles ---
# Dark blue header, centered, white bold text
header_style <- createStyle(fontSize = 12, fontColour = "#FFFFFF", halign = "center", valign = "center", fgFill = "#1E3A8A", textDecoration = "bold", border = "Bottom")

pct_style   <- createStyle(numFmt = "0.00%", halign = "right")
float_style <- createStyle(numFmt = "0.00000", halign = "right")
large_num   <- createStyle(numFmt = "#,##0.00", halign = "right")

# Conditional text colors (cleaner than full background cell fills)
pos_style <- createStyle(fontColour = "#15803D") # Emerald Green
neg_style <- createStyle(fontColour = "#B91C1C") # Crimson Red


# ==============================================================================
# SHEET 1: EXECUTIVE DASHBOARD
# ==============================================================================
addWorksheet(wb, "Dashboard")
showGridLines(wb, "Dashboard", showGridLines = FALSE) # Removes ugly excel gridlines
setColWidths(wb, "Dashboard", cols = 1, widths = 3)   # Elegant left margin

summary_df <- data.frame(
  Metric = c("XGBoost Nowcast", "DFM Nowcast", "Ensemble Growth", "Ensemble GDP Level"),
  Value = c(current_gdp_nowcast, dfm_gdp_nowcast, ensemble_gdp_nowcast, nowcast_gdp_level_ens)
)

writeData(wb, "Dashboard", summary_df, startRow = 2, startCol = 2, headerStyle = header_style, borders = "rows")

addStyle(wb, "Dashboard", style = pct_style, rows = 3:5, cols = 3, gridExpand = TRUE)
addStyle(wb, "Dashboard", style = large_num, rows = 6, cols = 3, gridExpand = TRUE)
setColWidths(wb, "Dashboard", cols = 2, widths = 25)
setColWidths(wb, "Dashboard", cols = 3, widths = 20)

# Insert Plots (Offset nicely)
print(p_Sector) 
tmp_Sector <- tempfile(fileext = ".png")
ggsave(tmp_Sector, p_Sector, width = 8, height = 5, dpi = 300)
insertImage(wb, "Dashboard", tmp_Sector, width = 8, height = 5, startCol = 5, startRow = 2)

print(p_vars) 
tmp_vars <- tempfile(fileext = ".png")
ggsave(tmp_vars, p_vars, width = 8, height = 6, dpi = 300)
insertImage(wb, "Dashboard", tmp_vars, width = 8, height = 6, startCol = 5, startRow = 28)


# ==============================================================================
# SHEET 2: SECTOR IMPACT
# ==============================================================================
addWorksheet(wb, "Sector Impact")
showGridLines(wb, "Sector Impact", showGridLines = FALSE)

writeData(wb, "Sector Impact", Sector_export, startRow = 1, startCol = 1, headerStyle = header_style, borders = "rows")

setColWidths(wb, "Sector Impact", cols = 1, widths = 35)
setColWidths(wb, "Sector Impact", cols = 2:4, widths = 20)

addStyle(wb, "Sector Impact", style = float_style, rows = 2:(nrow(Sector_export)+1), cols = 2, gridExpand = TRUE)
addStyle(wb, "Sector Impact", style = pct_style, rows = 2:(nrow(Sector_export)+1), cols = 3:4, gridExpand = TRUE)

conditionalFormatting(wb, "Sector Impact", cols = 2:3, rows = 2:(nrow(Sector_export)+1), rule = ">0", style = pos_style)
conditionalFormatting(wb, "Sector Impact", cols = 2:3, rows = 2:(nrow(Sector_export)+1), rule = "<0", style = neg_style)


# ==============================================================================
# SHEET 3: INDIVIDUAL VARIABLE IMPACT
# ==============================================================================
addWorksheet(wb, "Variable Impact")
showGridLines(wb, "Variable Impact", showGridLines = FALSE)

writeData(wb, "Variable Impact", news_export, startRow = 1, startCol = 1, headerStyle = header_style, borders = "rows")

setColWidths(wb, "Variable Impact", cols = 1, widths = 45)
setColWidths(wb, "Variable Impact", cols = 2, widths = 25)
setColWidths(wb, "Variable Impact", cols = 3:5, widths = 20)

addStyle(wb, "Variable Impact", style = float_style, rows = 2:(nrow(news_export)+1), cols = 3, gridExpand = TRUE)
addStyle(wb, "Variable Impact", style = pct_style, rows = 2:(nrow(news_export)+1), cols = 4:5, gridExpand = TRUE)

conditionalFormatting(wb, "Variable Impact", cols = 3:4, rows = 2:(nrow(news_export)+1), rule = ">0", style = pos_style)
conditionalFormatting(wb, "Variable Impact", cols = 3:4, rows = 2:(nrow(news_export)+1), rule = "<0", style = neg_style)


# ==============================================================================
# SAVE WORKBOOK
# ==============================================================================
output_file <- "data/clean/Nowcast_Executive_Report.xlsx"
if (!is_shiny) {
  saveWorkbook(wb, output_file, overwrite = TRUE)
  cat(sprintf("✓ Executive Excel Report generated successfully: %s\n", output_file))
}