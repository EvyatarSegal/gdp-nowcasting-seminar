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
# 13. TIME-SERIES VISUALIZATIONS & EXECUTIVE REPORTING (EXCEL UPGRADE)
# ==============================================================================
cat("[3/3] Generating visual tracking dashboards and baking Executive Excel...\n")

# --- Prepare Enhanced Dataframes with Executive Metrics ---

# For Variables: Add Absolute Impact, Percentages, and Relative Share per Month
historical_news_enhanced <- historical_news_df %>%
  mutate(
    Impact_Pct = Impact, # GDP growth is already a decimal/rate
    Abs_Impact = abs(Impact)
  ) %>%
  group_by(Date) %>%
  mutate(
    Total_Abs_Impact = sum(Abs_Impact, na.rm = TRUE),
    Relative_Share_Pct = if_else(Total_Abs_Impact > 0, Abs_Impact / Total_Abs_Impact, 0)
  ) %>%
  ungroup() %>%
  dplyr::select(Date, Variable, Sector, Total_Impact = Impact, Impact_Pct, Relative_Share_Pct)

# For Sectors: Add Absolute Impact, Percentages, and Relative Share per Month
historical_sector_enhanced <- historical_sector_df %>%
  mutate(
    Impact_Pct = Sector_Impact,
    Abs_Impact = abs(Sector_Impact)
  ) %>%
  group_by(Date) %>%
  mutate(
    Total_Abs_Impact = sum(Abs_Impact, na.rm = TRUE),
    Relative_Share_Pct = if_else(Total_Abs_Impact > 0, Abs_Impact / Total_Abs_Impact, 0)
  ) %>%
  ungroup() %>%
  dplyr::select(Date, Sector, Total_Impact = Sector_Impact, Impact_Pct, Relative_Share_Pct)


# --- Generate GGPlot Visualizations ---

# 1. Heatmap of Variable Impacts over Time
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
    subtitle = "Tracking specific variables driving the GDP nowcast (2021-2026)",
    x = "Timeline", y = "Predictor Variables"
  )

# 2. Stacked Bar Chart of Structural Sector Contributions
p_historical_bar <- ggplot(historical_sector_df, aes(x = Date, y = Sector_Impact, fill = Sector)) +
  geom_col(width = 20, alpha = 0.85) + 
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

# Save temporary image files
temp_img1 <- tempfile(fileext = ".png")
temp_img2 <- tempfile(fileext = ".png")
ggsave(temp_img1, plot = p_historical_heatmap, width = 11, height = 7.5, dpi = 300)
ggsave(temp_img2, plot = p_historical_bar, width = 11, height = 6.5, dpi = 300)


# --- Build Executive OpenXLSX Workbook ---
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
library(openxlsx)

wb <- createWorkbook()

# Define Professional Styles (Matching Executive Report Blueprint)
header_style <- createStyle(
  fontName = "Segoe UI", fontSize = 11, fontColour = "#FFFFFF", fgFill = "#333333",
  halign = "left", valign = "center", textDecoration = "bold",
  border = "TopBottomLeftRight", borderColour = "#555555"
)

data_style_left <- createStyle(fontName = "Segoe UI", fontSize = 10, halign = "left")
num_style_4dec  <- createStyle(fontName = "Segoe UI", fontSize = 10, numFmt = "0.0000", halign = "right")
pct_style_2dec  <- createStyle(fontName = "Segoe UI", fontSize = 10, numFmt = "0.00%", halign = "right")
date_style      <- createStyle(fontName = "Segoe UI", fontSize = 10, numFmt = "yyyy-mm-dd", halign = "center")

# ------------------------------------------------------------------------------
# SHEET 1: Variable_Impact
# ------------------------------------------------------------------------------
addWorksheet(wb, "Variable_Impact")
writeData(wb, "Variable_Impact", historical_news_enhanced, startRow = 1, startCol = 1)

# Apply Styles to Sheet 1 (Fixed: using addWorksheet and addStyle for headers)
addStyle(wb, "Variable_Impact", style = header_style, rows = 1, cols = 1:6, gridExpand = TRUE)
addStyle(wb, "Variable_Impact", style = date_style, rows = 2:(nrow(historical_news_enhanced)+1), cols = 1, gridExpand = TRUE)
addStyle(wb, "Variable_Impact", style = data_style_left, rows = 2:(nrow(historical_news_enhanced)+1), cols = 2:3, gridExpand = TRUE)
addStyle(wb, "Variable_Impact", style = num_style_4dec, rows = 2:(nrow(historical_news_enhanced)+1), cols = 4, gridExpand = TRUE)
addStyle(wb, "Variable_Impact", style = pct_style_2dec, rows = 2:(nrow(historical_news_enhanced)+1), cols = 5:6, gridExpand = TRUE)

# Set Column Widths and Insert Heatmap Image (Muted to Column H to give breathing room)
setColWidths(wb, "Variable_Impact", cols = 1:6, widths = c(13, 30, 28, 15, 15, 18))
insertImage(wb, "Variable_Impact", temp_img1, startCol = 8, startRow = 2, width = 11, height = 7.5)

# ------------------------------------------------------------------------------
# SHEET 2: Sector_Impact
# ------------------------------------------------------------------------------
addWorksheet(wb, "Sector_Impact")
writeData(wb, "Sector_Impact", historical_sector_enhanced, startRow = 1, startCol = 1)

# Apply Styles to Sheet 2 (Fixed: using addStyle for headers)
addStyle(wb, "Sector_Impact", style = header_style, rows = 1, cols = 1:5, gridExpand = TRUE)
addStyle(wb, "Sector_Impact", style = date_style, rows = 2:(nrow(historical_sector_enhanced)+1), cols = 1, gridExpand = TRUE)
addStyle(wb, "Sector_Impact", style = data_style_left, rows = 2:(nrow(historical_sector_enhanced)+1), cols = 2, gridExpand = TRUE)
addStyle(wb, "Sector_Impact", style = num_style_4dec, rows = 2:(nrow(historical_sector_enhanced)+1), cols = 3, gridExpand = TRUE)
addStyle(wb, "Sector_Impact", style = pct_style_2dec, rows = 2:(nrow(historical_sector_enhanced)+1), cols = 4:5, gridExpand = TRUE)

# Set Column Widths and Insert Stacked Bar Image (Muted to Column G)
setColWidths(wb, "Sector_Impact", cols = 1:5, widths = c(13, 28, 15, 15, 18))
insertImage(wb, "Sector_Impact", temp_img2, startCol = 7, startRow = 2, width = 11, height = 6.5)

# ------------------------------------------------------------------------------
# SAVE & CLEANUP
# ------------------------------------------------------------------------------
dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
output_excel_path <- "data/clean/historical_gdp_decomposition.xlsx"
saveWorkbook(wb, output_excel_path, overwrite = TRUE)

# Remove transient images
unlink(c(temp_img1, temp_img2))

cat("======================================================================\n")
cat("✓ SUCCESS: Executive-grade historical workbook built successfully.\n")
cat(sprintf("File saved with formal formatting and live plots at: %s\n", output_excel_path))
cat("======================================================================\n")