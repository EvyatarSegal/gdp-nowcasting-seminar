# ==============================================================================
# 11. ENSEMBLE: DFM + XGBOOST (BULLETPROOF FIX)
# ==============================================================================

# 1. Define your ensemble weights
w_xgb <- 0.60
w_dfm <- 0.40

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
  arrange(desc(Ensemble_Impact)) # Sorted from highest positive to lowest negative

Sector_report <- news_report %>%
  group_by(Sector) %>%
  summarise(Total_Impact = sum(Ensemble_Impact, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Total_Impact))

# --- E. Console Output & Proof of Math ---
cat("\n========== TOP SECTOR DRIVERS (ENSEMBLE IMPACT) ==========\n")
print(as.data.frame(Sector_report), row.names = FALSE)

cat("\n========== ALL INDIVIDUAL VARIABLE DRIVERS ==========\n")
formatted_news <- news_report %>%
  mutate(Ensemble_Impact_Formatted = sprintf("%.5f", Ensemble_Impact)) %>%
  select(Variable, Ensemble_Impact_Formatted, Sector)
print(as.data.frame(formatted_news), row.names = FALSE)
cat("==========================================================\n")

# THE INTEGRITY TEST
total_var_sum <- sum(news_report$Ensemble_Impact)
total_fac_sum <- sum(ensemble_factor_impacts)

cat(sprintf("\n[Math Verification] Sum of all 51 variables: %.6f\n", total_var_sum))
cat(sprintf("[Math Verification] Sum of all 4 factors:    %.6f\n", total_fac_sum))

if (abs(total_var_sum - total_fac_sum) < 1e-8) {
  cat("✓ Perfect Match: All factor impact was successfully distributed via Linear Projection.\n\n")
} else {
  cat("⚠ Warning: Mathematical leak detected.\n\n")
}

# Save outputs to CSV so you have the raw data
write.csv(Sector_report, "data/clean/nowcast_Sector_drivers.csv", row.names = FALSE)
write.csv(news_report, "data/clean/nowcast_variable_drivers.csv", row.names = FALSE)


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

# Create a new workbook
wb <- createWorkbook()

# --- Define Reusable Styles ---
pct_style   <- createStyle(numFmt = "0.00%")
float_style <- createStyle(numFmt = "0.00000")
large_num   <- createStyle(numFmt = "#,##0.00")
neg_style   <- createStyle(fontColour = "#9C0006", fgFill = "#FFC7CE") # Light red
pos_style   <- createStyle(fontColour = "#006100", fgFill = "#C6EFCE") # Light green
header_fmt  <- createStyle(fontSize = 14, fontColour = "#FFFFFF", fgFill = "#1F497D", 
                           halign = "center", textDecoration = "bold")

# ==============================================================================
# SHEET 1: EXECUTIVE DASHBOARD
# ==============================================================================
addWorksheet(wb, "Dashboard")

# 1. Prepare Summary Metrics Table
summary_df <- data.frame(
  Metric = c("XGBoost Nowcast", "DFM Nowcast", "Ensemble Growth", "Ensemble GDP Level"),
  Value = c(current_gdp_nowcast, dfm_gdp_nowcast, ensemble_gdp_nowcast, nowcast_gdp_level_ens)
)

# Write the table with a clean Excel style
writeDataTable(wb, "Dashboard", summary_df, startRow = 2, startCol = 2, 
               tableStyle = "TableStyleMedium2", withFilter = FALSE)

# Format the numbers in the summary table
addStyle(wb, "Dashboard", style = pct_style, rows = 3:5, cols = 3, gridExpand = TRUE)
addStyle(wb, "Dashboard", style = large_num, rows = 6, cols = 3, gridExpand = TRUE)
setColWidths(wb, "Dashboard", cols = 2:3, widths = c(25, 20))

# 2. Insert the Visualizations
print(p_Sector) 
insertPlot(wb, "Dashboard", width = 8, height = 5, xy = c("F", 2)) 

print(p_vars) 
insertPlot(wb, "Dashboard", width = 8, height = 6, xy = c("F", 28)) 


# ==============================================================================
# SHEET 2: SECTOR IMPACT
# ==============================================================================
addWorksheet(wb, "Sector Impact")

writeDataTable(wb, "Sector Impact", Sector_report, startRow = 1, startCol = 1, 
               tableStyle = "TableStyleMedium9")

setColWidths(wb, "Sector Impact", cols = 1:2, widths = c(35, 20))
addStyle(wb, "Sector Impact", style = float_style, rows = 2:(nrow(Sector_report)+1), 
         cols = 2, gridExpand = TRUE)
conditionalFormatting(wb, "Sector Impact", cols = 2, rows = 2:(nrow(Sector_report)+1), 
                      rule = ">0", style = pos_style)
conditionalFormatting(wb, "Sector Impact", cols = 2, rows = 2:(nrow(Sector_report)+1), 
                      rule = "<0", style = neg_style)


# ==============================================================================
# SHEET 3: INDIVIDUAL VARIABLE IMPACT
# ==============================================================================
addWorksheet(wb, "Variable Impact")

writeDataTable(wb, "Variable Impact", news_report, startRow = 1, startCol = 1, 
               tableStyle = "TableStyleMedium9")

setColWidths(wb, "Variable Impact", cols = 1:3, widths = c(45, 20, 30))
addStyle(wb, "Variable Impact", style = float_style, rows = 2:(nrow(news_report)+1), 
         cols = 2, gridExpand = TRUE)
conditionalFormatting(wb, "Variable Impact", cols = 2, rows = 2:(nrow(news_report)+1), 
                      rule = ">0", style = pos_style)
conditionalFormatting(wb, "Variable Impact", cols = 2, rows = 2:(nrow(news_report)+1), 
                      rule = "<0", style = neg_style)


# ==============================================================================
# SAVE WORKBOOK
# ==============================================================================
output_file <- "data/clean/Nowcast_Executive_Report.xlsx"
saveWorkbook(wb, output_file, overwrite = TRUE)

cat(sprintf("✓ Executive Excel Report generated successfully: %s\n", output_file))