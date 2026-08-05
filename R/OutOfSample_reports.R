# ==============================================================================
# 11. ENSEMBLE: DFM + XGBOOST (BULLETPROOF FIX FOR H0 VINTAGE)
# ==============================================================================

# 1. Define your ensemble weights
w_xgb <- 0.60
w_dfm <- 0.40

# 2. Hardcode the official Q4 2025 raw GDP level for H0 validation
base_level <- 437142  

# 3. Extract DFM Loadings and scale parameters manually to avoid NA bugs
gdp_mean <- mean(df_sub$GDP, na.rm = TRUE)
gdp_sd   <- sd(df_sub$GDP, na.rm = TRUE)

loadings_mat <- dfm_curr$C
var_names <- setdiff(colnames(df_sub), "Date")
rownames(loadings_mat) <- var_names

# Extract how heavily GDP loads onto the 4 factors
gdp_loadings <- loadings_mat["GDP", 1:4]

# 4. Calculate DFM Nowcast directly: X = (C * F) * sd + mean
# current_factors is our 1x4 matrix from the latest row
dfm_gdp_std <- sum(gdp_loadings * current_factors[1, 1:4])
dfm_gdp_nowcast <- (dfm_gdp_std * gdp_sd) + gdp_mean

# 5. Calculate the ensemble nowcast growth rate
ensemble_gdp_nowcast <- (w_xgb * current_gdp_nowcast) + (w_dfm * dfm_gdp_nowcast)

# 6. Apply growth rate to our real H0 base level
nowcast_gdp_level_ens <- base_level * (1 + ensemble_gdp_nowcast)

# Pre-format values for error-free printing
ens_level_formatted <- format(nowcast_gdp_level_ens, big.mark = ",", nsmall = 2, scientific = FALSE)

cat("\n========== H0 ENSEMBLE NOWCAST ==========\n")
cat(sprintf("XGBoost Nowcast Growth: %.4f (%.2f%%)\n", current_gdp_nowcast, current_gdp_nowcast * 100))
cat(sprintf("DFM Nowcast Growth:     %.4f (%.2f%%)\n", dfm_gdp_nowcast, dfm_gdp_nowcast * 100))
cat(sprintf("Ensemble Growth Rate:   %.4f (%.2f%%)\n", ensemble_gdp_nowcast, ensemble_gdp_nowcast * 100))
cat(sprintf("Annualized Growth Rate: %.2f%%\n", ((1 + ensemble_gdp_nowcast)^4 - 1) * 100))
cat(sprintf("Ensemble GDP Level (H3): %s million NIS\n", ens_level_formatted))
cat("=========================================\n")


# ==============================================================================
# 12. TRUE ENSEMBLE "NEWS": SHAP + DFM FACTOR ATTRIBUTION
# ==============================================================================
library(ggplot2)
library(dplyr)

# --- A. Calculate Factor-Level Impacts ---
# 1. XGBoost SHAP values for the factors
shap_matrix <- predict(xgb_model_final, current_factors, predcontrib = TRUE)
shap_vals <- as.numeric(shap_matrix)[1:4] # Safely extract the 4 factor SHAPs

# 2. DFM Factor Impacts (Linear contribution to the unscaled target)
dfm_factor_impacts <- gdp_loadings * current_factors[1, 1:4] * gdp_sd

# 3. Blended Ensemble Factor Impact
ensemble_factor_impacts <- (w_xgb * shap_vals) + (w_dfm * dfm_factor_impacts)

# --- B. Distribute Factor Impacts to Original Variables ---
loadings_drivers <- loadings_mat[rownames(loadings_mat) != "GDP", ]
var_contributions <- numeric(nrow(loadings_drivers))
names(var_contributions) <- rownames(loadings_drivers)

for (k in 1:4) {
  f_impact <- ensemble_factor_impacts[k]
  
  # Distribute based on the absolute weight of the variable inside the factor
  abs_loadings <- abs(loadings_drivers[, k])
  rel_weights <- abs_loadings / sum(abs_loadings, na.rm = TRUE)
  
  # INTEGRITY TEST: Ensure the relative weights sum to exactly 100%
  sum_weights <- sum(rel_weights, na.rm = TRUE)
  if (abs(sum_weights - 1) > 1e-8) {
    stop(sprintf("CRITICAL ERROR: Distribution weights for Factor %d sum to %f, expected 1.0", k, sum_weights))
  }
  
  # Multiply by the sign so inverse relationships pull in the correct direction
  var_contributions <- var_contributions + (rel_weights * sign(loadings_drivers[, k]) * f_impact)
}

cat("✓ Integrity Test Passed: All factor distribution weights sum to exactly 100%.\n")

# --- C. Map Variables Back to their Families (Blocks) ---
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
  arrange(desc(abs(Ensemble_Impact)))

Sector_report <- news_report %>%
  group_by(Sector) %>%
  summarise(Total_Impact = sum(Ensemble_Impact, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(abs(Total_Impact)))

# --- E. Console Output & CSV Export ---
cat("\n========== TOP SECTOR DRIVERS (ENSEMBLE IMPACT) ==========\n")
print(as.data.frame(Sector_report), row.names = FALSE)

cat("\n========== ALL INDIVIDUAL VARIABLE DRIVERS ==========\n")
formatted_news <- news_report %>%
  mutate(Ensemble_Impact_Formatted = sprintf("%.5f", Ensemble_Impact)) %>%
  select(Variable, Ensemble_Impact_Formatted, Sector)

print(as.data.frame(formatted_news), row.names = FALSE)
cat("==========================================================\n")

# --- PROOF OF MATH VERIFICATION ---
total_variable_sum <- sum(news_report$Ensemble_Impact)
total_factor_sum <- sum(ensemble_factor_impacts)

cat(sprintf("\n[Math Verification] Sum of all variables: %.6f\n", total_variable_sum))
cat(sprintf("[Math Verification] Sum of all 4 factors:  %.6f\n", total_factor_sum))
if(abs(total_variable_sum - total_factor_sum) < 1e-8) {
  cat("✓ Perfect Match: All factor impact was successfully distributed.\n\n")
}

# Save H0 specific raw data CSVs
write.csv(Sector_report, "data/clean/nowcast_Sector_drivers_h3.csv", row.names = FALSE)
write.csv(news_report, "data/clean/nowcast_variable_drivers_h3.csv", row.names = FALSE)


# ==============================================================================
# 12.1 VISUALIZE THE H0 NEWS REPORT
# ==============================================================================

# Plot 1: Macro Impact by Sector
p_Sector <- ggplot(Sector_report, aes(x = reorder(Sector, Total_Impact), y = Total_Impact, fill = Total_Impact > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred")) +
  theme_minimal() +
  labs(title = "H3 GDP Nowcast Drivers by Sector (Block)",
       subtitle = "Combined XGBoost SHAP + DFM Linear Impacts",
       x = "Variable Sector", y = "Contribution to GDP Growth Rate") +
  theme(legend.position = "none")

print(p_Sector)

# Plot 2: Top 15 Individual Variable Impacts
top_vars <- head(news_report, 15)

p_vars <- ggplot(top_vars, aes(x = reorder(Variable, Ensemble_Impact), y = Ensemble_Impact, fill = Ensemble_Impact > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred")) +
  theme_minimal() +
  labs(title = "Top 15 Variable Drivers for H3 GDP Nowcast",
       subtitle = "Apportioned via DFM Loadings (\u039b)",
       x = "Specific Variable", y = "Contribution to GDP Growth Rate") +
  theme(legend.position = "none")

print(p_vars)


# ==============================================================================
# 13. PROFESSIONAL EXCEL EXPORT (H0 EXECUTIVE REPORT)
# ==============================================================================
library(openxlsx)

cat("Generating professional Excel report for H3...\n")

wb <- createWorkbook()

# --- Define Reusable Styles ---
pct_style   <- createStyle(numFmt = "0.00%")
float_style <- createStyle(numFmt = "0.00000")
large_num   <- createStyle(numFmt = "#,##0.00")
neg_style   <- createStyle(fontColour = "#9C0006", fgFill = "#FFC7CE") 
pos_style   <- createStyle(fontColour = "#006100", fgFill = "#C6EFCE") 
header_fmt  <- createStyle(fontSize = 14, fontColour = "#FFFFFF", fgFill = "#1F497D", 
                           halign = "center", textDecoration = "bold")

# --- SHEET 1: EXECUTIVE DASHBOARD ---
addWorksheet(wb, "Dashboard")

# Prepare H0 Summary Metrics Table
summary_df <- data.frame(
  Metric = c("XGBoost Nowcast", "DFM Nowcast", "Ensemble Growth", "Ensemble GDP Level"),
  Value = c(current_gdp_nowcast, dfm_gdp_nowcast, ensemble_gdp_nowcast, nowcast_gdp_level_ens)
)

writeDataTable(wb, "Dashboard", summary_df, startRow = 2, startCol = 2, 
               tableStyle = "TableStyleMedium2", withFilter = FALSE)

addStyle(wb, "Dashboard", style = pct_style, rows = 3:5, cols = 3, gridExpand = TRUE)
addStyle(wb, "Dashboard", style = large_num, rows = 6, cols = 3, gridExpand = TRUE)
setColWidths(wb, "Dashboard", cols = 2:3, widths = c(25, 20))

# Insert Visualizations into Dashboard
print(p_Sector) 
insertPlot(wb, "Dashboard", width = 8, height = 5, xy = c("F", 2)) 

print(p_vars) 
insertPlot(wb, "Dashboard", width = 8, height = 6, xy = c("F", 28)) 

# --- SHEET 2: SECTOR IMPACT ---
addWorksheet(wb, "Sector Impact")
writeDataTable(wb, "Sector Impact", Sector_report, startRow = 1, startCol = 1, tableStyle = "TableStyleMedium9")
setColWidths(wb, "Sector Impact", cols = 1:2, widths = c(35, 20))
addStyle(wb, "Sector Impact", style = float_style, rows = 2:(nrow(Sector_report)+1), cols = 2, gridExpand = TRUE)
conditionalFormatting(wb, "Sector Impact", cols = 2, rows = 2:(nrow(Sector_report)+1), rule = ">0", style = pos_style)
conditionalFormatting(wb, "Sector Impact", cols = 2, rows = 2:(nrow(Sector_report)+1), rule = "<0", style = neg_style)

# --- SHEET 3: INDIVIDUAL VARIABLE IMPACT ---
addWorksheet(wb, "Variable Impact")
writeDataTable(wb, "Variable Impact", news_report, startRow = 1, startCol = 1, tableStyle = "TableStyleMedium9")
setColWidths(wb, "Variable Impact", cols = 1:3, widths = c(45, 20, 30))
addStyle(wb, "Variable Impact", style = float_style, rows = 2:(nrow(news_report)+1), cols = 2, gridExpand = TRUE)
conditionalFormatting(wb, "Variable Impact", cols = 2, rows = 2:(nrow(news_report)+1), rule = ">0", style = pos_style)
conditionalFormatting(wb, "Variable Impact", cols = 2, rows = 2:(nrow(news_report)+1), rule = "<0", style = neg_style)

# --- SAVE WORKBOOK ---
output_file <- "data/clean/Nowcast_Executive_Report_h3.xlsx"
saveWorkbook(wb, output_file, overwrite = TRUE)

cat(sprintf("✓ Executive Excel Report for H3 generated successfully: %s\n", output_file))