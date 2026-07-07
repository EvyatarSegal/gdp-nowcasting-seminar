# ==============================================================================
# 11. ENSEMBLE CONFIGURATION & BASELINE EXTRACTION
# ==============================================================================
cat("\nConfiguring ensemble and preparing baseline matrices for historical attribution...\n")

# Define ensemble weights for the historical decomposition
if (!exists("w_xgb")) w_xgb <- 0.60
if (!exists("w_dfm")) w_dfm <- 0.40

# Extract parameters from the final full-sample DFM state
loadings_mat <- dfm_curr$C
var_names <- colnames(xts_Q)  # Contains all variables including GDP
rownames(loadings_mat) <- var_names

# Isolate target loadings vs driver loadings
gdp_loadings <- loadings_mat["GDP", 1:4]
loadings_drivers <- loadings_mat[rownames(loadings_mat) != "GDP", 1:4]

# Calculate target scale metrics from clean dataset
gdp_mean <- mean(df_clean$GDP, na.rm = TRUE)
gdp_sd   <- sd(df_clean$GDP, na.rm = TRUE)

# Compute the Generalized Least Squares (GLS) Projection Matrix: W = (C'C)^(-1) C'
C_mat <- loadings_drivers
W_mat <- solve(t(C_mat) %*% C_mat) %*% t(C_mat) # Matrix dimensions: 4 factors x 51 variables

# Standardize the driver variables across the entire historical timeline
raw_drivers <- df_clean[, rownames(loadings_drivers)]
scaled_drivers <- scale(raw_drivers)

# --- Sector Mapping Fallback ---
if (exists("blocks_shifted")) {
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
} else {
  cat("Notice: 'blocks_shifted' not found. Dynamically grouping variables into analytical sectors...\n")
  var_Sector_map <- data.frame(
    Variable = rownames(loadings_drivers),
    Sector = paste0("Sector_", substr(rownames(loadings_drivers), 1, 3)),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# 12. HISTORICAL DYNAMIC ATTRIBUTION SYSTEM (TIME-SERIES NEWS)
# ==============================================================================
cat("\n[1/3] Extracting aligned historical factor matrices...\n")

# Isolate historical periods where factors and actual inputs exist
historical_timeline <- results %>%
  filter(!is.na(h0_f1) & !is.na(h0_f2) & !is.na(h0_f3) & !is.na(h0_f4))

# Convert features into a clean matrix matching the exact training column names
X_historical_factors <- as.matrix(historical_timeline[, c("h0_f1", "h0_f2", "h0_f3", "h0_f4")])
mode(X_historical_factors) <- "double"

# Generate the complete historical matrix of SHAP values
all_shap_matrix <- predict(xgb_model_final, X_historical_factors, predcontrib = TRUE)

# Initialize storage for panel processing
historical_news_list <- list()

cat("[2/3] Decomposing ensemble growth into underlying variable drivers across time...\n")
for (i in 1:nrow(X_historical_factors)) {
  
  current_date <- historical_timeline$Date[i]
  factors_i <- X_historical_factors[i, 1:4]
  shap_i <- as.numeric(all_shap_matrix[i, 1:4])
  
  # 1. Compute dynamic impacts at the factor level
  dfm_impacts_i <- gdp_loadings * factors_i * gdp_sd
  ensemble_impacts_i <- (w_xgb * shap_i) + (w_dfm * dfm_impacts_i)
  
  # 2. Extract corresponding standardized driver data for this specific month
  date_idx <- which(df_clean$Date == current_date)
  if (length(date_idx) == 0) next
  
  X_i <- as.numeric(scaled_drivers[date_idx, ])
  names(X_i) <- rownames(loadings_drivers)
  
  # Handle missing data dynamically using the DFM state space projection
  dfm_implied_X_i <- as.numeric(loadings_drivers %*% factors_i)
  X_i[is.na(X_i)] <- dfm_implied_X_i[is.na(X_i)]
  
  # 3. Distribute factor impacts to specific variables via the projection matrix
  var_contributions_i <- numeric(nrow(loadings_drivers))
  names(var_contributions_i) <- rownames(loadings_drivers)
  
  for (k in 1:4) {
    f_impact <- ensemble_impacts_i[k]
    raw_var_to_factor <- W_mat[k, ] * X_i
    sum_raw <- sum(raw_var_to_factor, na.rm = TRUE)
    
    if (abs(sum_raw) < 1e-12) {
      rel_weights <- rep(0, length(X_i))
    } else {
      rel_weights <- raw_var_to_factor / sum_raw
    }
    
    var_contributions_i <- var_contributions_i + (rel_weights * f_impact)
  }
  
  # 4. Store current period breakdown
  historical_news_list[[i]] <- data.frame(
    Date = current_date,
    Variable = names(var_contributions_i),
    Impact = var_contributions_i,
    stringsAsFactors = FALSE
  )
}

# Consolidate and bind historical panel dataframe
historical_news_df <- bind_rows(historical_news_list) %>%
  left_join(var_Sector_map, by = "Variable")

# Aggregate variable records to broader Sector levels
historical_sector_df <- historical_news_df %>%
  group_by(Date, Sector) %>%
  summarise(Sector_Impact = sum(Impact, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# 13. TIME-SERIES VISUALIZATIONS & REPORTING (EXCEL WITH EMBEDDED CHARTS)
# ==============================================================================
cat("[3/3] Generating visual tracking dashboards and embedding into Excel...\n")

# 1. Generate Plot 1: Heatmap of Variable Impacts over Time
p_historical_heatmap <- ggplot(historical_news_df, aes(x = Date, y = Variable, fill = Impact)) +
  geom_tile() +
  scale_fill_gradient2(low = "#B91C1C", mid = "white", high = "#15803D", midpoint = 0, 
                       name = "GDP Growth Impact") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    title = "Macro Feature Importance Trajectory",
    subtitle = "Tracking which specific variables drive the GDP nowcast during different economic regimes",
    x = "Timeline", y = "Predictor Variables"
  )

# 2. Generate Plot 2: Stacked Bar Chart of Structural Sector Contributions
p_historical_bar <- ggplot(historical_sector_df, aes(x = Date, y = Sector_Impact, fill = Sector)) +
  geom_col(width = 20, alpha = 0.85) + # Fixed: removed color=NA to protect Legend visibility
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.6) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray30")
  ) +
  labs(
    title = "Historical GDP Growth Structural Breakdown",
    subtitle = "Net structural contribution of macro sectors (Positives stack up, Negatives stack down)",
    x = "Timeline", y = "Contribution to Quarterly Growth Rate",
    fill = "Macro Sector"
  )

# --- Save temporary image files of the plots to insert into Excel ---
temp_img1 <- tempfile(fileext = ".png")
temp_img2 <- tempfile(fileext = ".png")

# Set consistent dimensions for output images
ggsave(temp_img1, plot = p_historical_heatmap, width = 10, height = 7, dpi = 300)
ggsave(temp_img2, plot = p_historical_bar, width = 10, height = 6, dpi = 300)

# --- Create Advanced Excel Workbook with openxlsx ---
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
library(openxlsx)

# Initialize workbook
wb <- createWorkbook()

# Sheet 1: Variable Impacts (Data + Heatmap)
addWorksheet(wb, "Variable_Impacts")
writeData(wb, "Variable_Impacts", historical_news_df)
# Fixed: Adjusted width/height to perfectly match ggsave aspect ratio
insertImage(wb, "Variable_Impacts", temp_img1, startCol = 6, startRow = 2, width = 10, height = 7)

# Sheet 2: Sector Impacts (Data + Stacked Bar Chart)
addWorksheet(wb, "Sector_Impacts")
writeData(wb, "Sector_Impacts", historical_sector_df)
# Fixed: Adjusted width/height to perfectly match ggsave aspect ratio
insertImage(wb, "Sector_Impacts", temp_img2, startCol = 5, startRow = 2, width = 10, height = 6)

# Ensure directory exists and save workbook
dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
output_excel_path <- "data/clean/historical_gdp_decomposition.xlsx"
saveWorkbook(wb, output_excel_path, overwrite = TRUE)

# Clean up temporary image files from memory
unlink(c(temp_img1, temp_img2))

cat("======================================================================\n")
cat("✓ SUCCESS: Historical structural attribution run completed successfully.\n")
cat(sprintf("Multi-sheet Excel workbook WITH LIVE CHARTS generated at: %s\n", output_excel_path))
cat("======================================================================\n")