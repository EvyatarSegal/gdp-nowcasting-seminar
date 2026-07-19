# ==============================================================================
# 1. SETUP & DATA LOADING
# ==============================================================================
library(dfms)
library(readxl)
library(dplyr)
library(lubridate)
library(xts)
library(xgboost)
library(zoo)
library(ggplot2)
library(openxlsx)

if (!exists("is_shiny")) is_shiny <- FALSE
if (!is_shiny) {
  while (!is.null(dev.list()))  dev.off()
  par(mfrow = c(1,1))
}
set.seed(2026)

# Ensure output directory exists
output_dir <- "output/reports/back"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# Load data and drop completely empty columns
if (!exists("df")) {
  df <- read_excel("data/clean/combined_monthly_panel_Q_refined.xlsx") %>%
    dplyr::select(where(~ !all(is.na(.))))
} else {
  df <- df %>% dplyr::select(where(~ !all(is.na(.))))
}
df <- df %>% dplyr::select(-any_of(c('Net import purchase tax', 'Total Income Tax Division Net',
                                     'Companies returns', 'praise tax returns', 'participation rate')))

df$Date <- as.Date(df$Date)

# 1. Create month-year column
df$month_year <- format(df$Date, "%Y-%m")

# 2. Aggregate: one row per month-year, average numeric columns
data_agg <- df %>%
  group_by(month_year) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

df <- data_agg

# Convert month_year to Date (first day of month)
df$Date <- as.Date(paste0(df$month_year, "-01"))
df <- df %>% dplyr::select(-c(month_year))

# Shift the GDP column "one up" as requested
df <- df %>% 
  dplyr::mutate(GDP = dplyr::lead(GDP, n = 1))

# Clean the dataset for later merging
df_clean <- df %>% distinct(Date, .keep_all = TRUE)

# ==============================================================================
# 2. DYNAMIC TIMELINE & PRE-ALLOCATION
# ==============================================================================
end_date   <- max(df$Date, na.rm = TRUE)
start_date <- end_date - lubridate::years(6)  + months(7)
test_start_date <- start_date

all_months <- seq(start_date, end_date, by = "month")
cat(sprintf("Running rolling forecast from %s to %s\n", start_date, end_date))

# Initialize results data frame with exactly the right dimensions
results_report <- data.frame(Date = all_months)

# Dynamically generate all 16 tracking columns
for (h in 0:3) {
  for (f in 1:4) {
    col_name <- paste0("h", h, "_f", f)
    results_report[[col_name]] <- NA_real_
  }
}

# ==============================================================================
# 3. DYNAMIC FACTOR MODEL (DFM) ROLLING LOOP
# ==============================================================================
max_idx <- nrow(results_report)

for (i in seq_along(all_months)) {
  cutoff  <- all_months[i]
  month_i <- lubridate::month(cutoff)
  
  # Dynamic mathematical horizon
  h_val <- 3 - (month_i - 1) %% 3
  
  # Subset panel
  df_sub <- df[df$Date <= cutoff, ]
  if (nrow(df_sub) == 0) next
  
  X_xts <- xts(
    as.matrix(df_sub[, !(names(df_sub) == "Date")]),
    order.by = df_sub$Date
  )
  
  # Blind the latest GDP to simulate out-of-sample
  X_xts[nrow(X_xts), "GDP"] <- NA
  
  # Fit DFM
  dfm_curr <- DFM(
    X = X_xts, r = 4, p = 3,
    quarterly.vars = "GDP", em.method = "BM"
  )
  
  # Predict and safely extract vectors
  pred <- predict(dfm_curr, h = h_val, standardized = TRUE)
  F_h  <- pred$F[h_val, 1:4]
  F_h0 <- as.vector(tail(dfm_curr$F_qml, 1))[1:4]
  
  # Safe Data Assignment
  if ((i + h_val) <= max_idx) {
    target_cols <- paste0("h", h_val, "_f", 1:4)
    results_report[i + h_val, target_cols] <- F_h
  }
  
  h0_cols <- paste0("h0_f", 1:4)
  results_report[i, h0_cols] <- F_h0
}

# ==============================================================================
# 4. XGBOOST PREPARATION (ROBUST DYNAMIC MATCHING)
# ==============================================================================
all_factors <- data.frame(dfm_curr$F_qml)
colnames(all_factors) <- c("f1", "f2", "f3", "f4")
all_factors$Date <- tail(df_sub$Date, nrow(all_factors))

y_train_prep <- df_clean %>%
  filter(!is.na(GDP)) %>%
  dplyr::select(Date, GDP) %>%
  mutate(YM = format(Date, "%Y-%m"))

x_train_prep <- all_factors %>%
  mutate(YM = format(Date, "%Y-%m")) %>%
  filter(YM %in% y_train_prep$YM) %>%
  distinct(YM, .keep_all = TRUE)

train_data <- x_train_prep %>%
  inner_join(y_train_prep, by = "YM", suffix = c("_factor", "_gdp")) %>%
  filter(Date_factor < test_start_date)

base_df <- as.data.frame(train_data)
X_mat   <- as.matrix(base_df[, c("f1", "f2", "f3", "f4")])
mode(X_mat) <- "double"
y_vec   <- as.numeric(base_df$GDP)

max_date    <- max(base_df$Date_factor)
time_diff   <- as.numeric(difftime(max_date, base_df$Date_factor, units = "days")) / 365.25
weights_vec <- as.numeric(1 / (1 + exp(3.968421 * (time_diff - 9.842105))))

dtrain <- xgb.DMatrix(data = X_mat, label = y_vec, weight = weights_vec)

# ==============================================================================
# 5. XGBOOST TRAINING & PREDICTION
# ==============================================================================
params <- list(
  objective = "reg:squarederror", eval_metric = "rmse",
  eta = 0.015, max_depth = 4, subsample = 0.8, colsample_bytree = 0.8
)
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 300)

# ==============================================================================
# 6. BUILD RESULTS DATA FRAME WITH FORECASTS FOR ALL HORIZONS
# ==============================================================================
results <- results_report %>%
  left_join(df_clean %>% dplyr::select(Date, GDP), by = "Date")

for (h in 0:3) {
  col_f <- paste0("h", h, "_f", 1:4)
  fcst_col_name <- paste0("GDP_FCST_h", h)
  results[[fcst_col_name]] <- NA_real_
  
  idx_forecast <- which(rowSums(is.na(results[, col_f])) == 0)
  if (length(idx_forecast) > 0) {
    X_test_mat <- as.matrix(results[idx_forecast, col_f])
    colnames(X_test_mat) <- c("f1", "f2", "f3", "f4")
    results[[fcst_col_name]][idx_forecast] <- predict(xgb_model, X_test_mat)
  }
}

# ==============================================================================
# 7. OUT-OF-SAMPLE ERROR METRICS & AR(1) BENCHMARKING
# ==============================================================================
oos_results <- results %>% filter(Date >= test_start_date, !is.na(GDP))

horizon_comparison <- data.frame(
  Horizon = c("h0 (Nowcast - Month of Release)", "h1 (1 Month before end of Quarter)", 
              "h2 (2 Months before end of Quarter)", "h3 (3 Months before end of Quarter)"),
  RMSE = NA_real_, MAE = NA_real_
)

for (h in 0:3) {
  fcst_col <- paste0("GDP_FCST_h", h)
  eval_sub <- oos_results %>% filter(!is.na(.data[[fcst_col]]))
  if (nrow(eval_sub) > 0) {
    horizon_comparison$RMSE[h + 1] <- sqrt(mean((eval_sub$GDP - eval_sub[[fcst_col]])^2))
    horizon_comparison$MAE[h + 1]  <- mean(abs(eval_sub$GDP - eval_sub[[fcst_col]]))
  }
}

# AR(1) Benchmark
rmse_h0 <- horizon_comparison$RMSE[1]
benchmark_data <- oos_results %>%
  dplyr::select(Date, GDP, GDP_FCST_h0) %>%
  filter(!is.na(GDP)) %>% mutate(GDP_lag1 = lag(GDP)) %>% filter(!is.na(GDP_lag1))

ar1_model <- lm(GDP ~ GDP_lag1, data = benchmark_data)
benchmark_data$GDP_AR1 <- predict(ar1_model, newdata = benchmark_data)
rmse_ar1 <- sqrt(mean((benchmark_data$GDP - benchmark_data$GDP_AR1)^2, na.rm = TRUE))

cat("\n========== Model Benchmarking (Theil's U) ==========\n")
cat(sprintf("Your Nowcast (h0) RMSE : %.4f\n", rmse_h0))
cat(sprintf("AR(1) Model RMSE       : %.4f\n", rmse_ar1))
cat(sprintf("Theil's U Ratio        : %.4f\n", rmse_h0 / rmse_ar1))
cat("=====================================================\n")

# ==============================================================================
# 8. TRAIN FINAL XGBOOST ON ALL HISTORICAL DATA
# ==============================================================================
final_train <- results %>%
  filter(!is.na(GDP) & !is.na(h0_f1) & !is.na(h0_f2) & !is.na(h0_f3) & !is.na(h0_f4))

X_all <- as.matrix(final_train[, c("h0_f1", "h0_f2", "h0_f3", "h0_f4")])
y_all <- final_train$GDP
max_date_all <- max(final_train$Date)
time_diff_all <- as.numeric(difftime(max_date_all, final_train$Date, units = "days")) / 365.25
weights_all   <- as.numeric(1 / (1 + exp(3.968421 * (time_diff_all - 9.842105))))
dtrain_final <- xgb.DMatrix(data = X_all, label = y_all, weight = weights_all)

xgb_model_final <- xgb.train(params = params, data = dtrain_final, nrounds = 300)

# Target Loading Data
if (!exists("target_df")) {
  target_df <- read_excel("data/raw/nowcasting_data_raw_new.xlsx", sheet = "target")
}
target_df$Date <- as.Date(target_df$Date)
last_gdp <- target_df %>% filter(!is.na(GDP)) %>% slice_tail(n = 1)
if (nrow(last_gdp) == 0) stop("No GDP level found in the 'target' sheet.")
base_level <- last_gdp$GDP

# Ensemble Weights setup
if (!exists("w_xgb")) w_xgb <- 0.60
if (!exists("w_dfm")) w_dfm <- 0.40

# Base parameters from Final DFM
loadings_mat <- dfm_curr$C
var_names <- setdiff(colnames(df_sub), "Date")
rownames(loadings_mat) <- var_names
gdp_loadings <- loadings_mat["GDP", 1:4]
loadings_drivers <- loadings_mat[rownames(loadings_mat) != "GDP", ]

# Setup for Block Mapping
block_map_list <- lapply(names(blocks_shifted), function(b_name) {
  if(!b_name %in% c("adjusters", "target")) {
    data.frame(Sector = b_name, Variable = setdiff(names(blocks_shifted[[b_name]]), "Date"), stringsAsFactors = FALSE)
  }
})
var_Sector_map <- bind_rows(block_map_list)


# ==============================================================================
# 9. GENERATE 4 EXECUTIVE REPORTS (FOR THE LAST 4 MONTHS)
# ==============================================================================
cat("\nGenerating Executive Reports for the last 4 periods...\n")
report_indices <- (nrow(results) - 3):nrow(results)

for (target_idx in report_indices) {
  
  current_date <- results$Date[target_idx]
  cat(sprintf("\n--- Processing Report for Month: %s ---\n", current_date))
  
  # Get inputs for this specific month
  current_factors <- as.matrix(results[target_idx, c("h0_f1", "h0_f2", "h0_f3", "h0_f4")])
  colnames(current_factors) <- c("f1", "f2", "f3", "f4")
  
  # A. XGBoost & DFM Predictions
  current_gdp_nowcast <- predict(xgb_model_final, current_factors)
  
  # Recalculate GDP mean/sd up to this month to maintain accurate scaling
  df_loop <- df[df$Date <= current_date, ]
  gdp_mean <- mean(df_loop$GDP, na.rm = TRUE)
  gdp_sd   <- sd(df_loop$GDP, na.rm = TRUE)
  
  dfm_gdp_std <- sum(gdp_loadings * current_factors[1, 1:4])
  dfm_gdp_nowcast <- (dfm_gdp_std * gdp_sd) + gdp_mean
  
  ensemble_gdp_nowcast <- (w_xgb * current_gdp_nowcast) + (w_dfm * dfm_gdp_nowcast)
  nowcast_gdp_level_ens <- base_level * (1 + ensemble_gdp_nowcast)
  
  # B. Attribution (News & Impacts)
  shap_matrix <- predict(xgb_model_final, current_factors, predcontrib = TRUE)
  shap_vals <- as.numeric(shap_matrix)[1:4]
  dfm_factor_impacts <- gdp_loadings * current_factors[1, 1:4] * gdp_sd
  ensemble_factor_impacts <- (w_xgb * shap_vals) + (w_dfm * dfm_factor_impacts)
  
  # C. Map back to Variables
  raw_drivers <- df_loop[, rownames(loadings_drivers)]
  scaled_drivers <- scale(raw_drivers)
  current_X <- as.numeric(scaled_drivers[nrow(scaled_drivers), ])
  names(current_X) <- rownames(loadings_drivers)
  
  dfm_implied_X <- as.numeric(loadings_drivers %*% t(current_factors))
  current_X[is.na(current_X)] <- dfm_implied_X[is.na(current_X)]
  
  C_mat <- loadings_drivers
  W_mat <- solve(t(C_mat) %*% C_mat) %*% t(C_mat)
  
  var_contributions <- numeric(nrow(loadings_drivers))
  names(var_contributions) <- rownames(loadings_drivers)
  
  for (k in 1:4) {
    f_impact <- ensemble_factor_impacts[k]
    raw_var_to_factor <- W_mat[k, ] * current_X
    sum_raw <- sum(raw_var_to_factor, na.rm = TRUE)
    
    if (abs(sum_raw) < 1e-12) { rel_weights <- rep(0, length(current_X)) } 
    else { rel_weights <- raw_var_to_factor / sum_raw }
    var_contributions <- var_contributions + (rel_weights * f_impact)
  }
  
  # D. Build Reporting DataFrames
  news_report <- data.frame(
    Variable = names(var_contributions), Ensemble_Impact = var_contributions, stringsAsFactors = FALSE
  ) %>%
    left_join(var_Sector_map, by = "Variable") %>%
    mutate(Relative_Share_Pct = (abs(Ensemble_Impact) / sum(abs(Ensemble_Impact), na.rm = TRUE))) %>%
    arrange(desc(abs(Ensemble_Impact)))
  
  Sector_report <- news_report %>%
    group_by(Sector) %>%
    summarise(
      Total_Impact = sum(Ensemble_Impact, na.rm = TRUE),
      Relative_Share_Pct = sum(Relative_Share_Pct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(abs(Total_Impact)))
  
  # E. Create Visuals
  p_Sector <- ggplot(Sector_report, aes(x = reorder(Sector, Total_Impact), y = Total_Impact, fill = Total_Impact > 0)) +
    geom_col() + coord_flip() +
    scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred")) +
    theme_minimal() +
    labs(title = sprintf("GDP Nowcast Drivers by Sector (%s)", format(current_date, "%b %Y")),
         subtitle = "Combined XGBoost SHAP + DFM Linear Impacts",
         x = "Sector", y = "Contribution to GDP Growth Rate") +
    theme(legend.position = "none")
  
  top_vars <- head(news_report, 15)
  p_vars <- ggplot(top_vars, aes(x = reorder(Variable, Ensemble_Impact), y = Ensemble_Impact, fill = Ensemble_Impact > 0)) +
    geom_col() + coord_flip() +
    scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred")) +
    theme_minimal() +
    labs(title = sprintf("Top 15 Variable Drivers for Current GDP Nowcast (%s)", format(current_date, "%b %Y")),
         x = "Specific Variable", y = "Contribution to GDP Growth Rate") +
    theme(legend.position = "none")
  
  # F. Build Excel Workbook
  news_export <- news_report %>%
    select(Variable, Sector, `Total Impact` = Ensemble_Impact, `Relative Share (%)` = Relative_Share_Pct)
  Sector_export <- Sector_report %>%
    select(Sector, `Total Impact` = Total_Impact, `Relative Share (%)` = Relative_Share_Pct)
  
  wb <- createWorkbook()
  modifyBaseFont(wb, fontSize = 11, fontName = "Calibri")
  
  # Styles
  header_style <- createStyle(fontSize = 12, fontColour = "#FFFFFF", halign = "center", valign = "center", fgFill = "#1E3A8A", textDecoration = "bold", border = "Bottom")
  pct_style    <- createStyle(numFmt = "0.00%", halign = "right")
  float_style  <- createStyle(numFmt = "0.00000", halign = "right")
  large_num    <- createStyle(numFmt = "#,##0.00", halign = "right")
  pos_style    <- createStyle(fontColour = "#15803D")
  neg_style    <- createStyle(fontColour = "#B91C1C")
  
  # Sheet 1: Dashboard
  addWorksheet(wb, "Dashboard")
  showGridLines(wb, "Dashboard", showGridLines = FALSE)
  setColWidths(wb, "Dashboard", cols = 1, widths = 3)
  
  summary_df <- data.frame(
    Metric = c("XGBoost Nowcast", "DFM Nowcast", "Ensemble Growth", "Ensemble GDP Level"),
    Value = c(current_gdp_nowcast, dfm_gdp_nowcast, ensemble_gdp_nowcast, nowcast_gdp_level_ens)
  )
  
  writeData(wb, "Dashboard", summary_df, startRow = 2, startCol = 2, headerStyle = header_style, borders = "rows")
  addStyle(wb, "Dashboard", style = pct_style, rows = 3:5, cols = 3, gridExpand = TRUE)
  addStyle(wb, "Dashboard", style = large_num, rows = 6, cols = 3, gridExpand = TRUE)
  setColWidths(wb, "Dashboard", cols = 2, widths = 25)
  setColWidths(wb, "Dashboard", cols = 3, widths = 20)
  
  # Inject Plots
  tmp_Sector <- tempfile(fileext = ".png")
  ggsave(tmp_Sector, p_Sector, width = 8, height = 5, dpi = 300)
  insertImage(wb, "Dashboard", tmp_Sector, width = 8, height = 5, startCol = 5, startRow = 2)
  
  tmp_vars <- tempfile(fileext = ".png")
  ggsave(tmp_vars, p_vars, width = 8, height = 6, dpi = 300)
  insertImage(wb, "Dashboard", tmp_vars, width = 8, height = 6, startCol = 5, startRow = 28)
  
  # Sheet 2: Sector Impact
  addWorksheet(wb, "Sector Impact")
  showGridLines(wb, "Sector Impact", showGridLines = FALSE)
  writeData(wb, "Sector Impact", Sector_export, startRow = 1, startCol = 1, headerStyle = header_style, borders = "rows")
  setColWidths(wb, "Sector Impact", cols = 1, widths = 35)
  setColWidths(wb, "Sector Impact", cols = 2:3, widths = 20)
  addStyle(wb, "Sector Impact", style = float_style, rows = 2:(nrow(Sector_export)+1), cols = 2, gridExpand = TRUE)
  addStyle(wb, "Sector Impact", style = pct_style, rows = 2:(nrow(Sector_export)+1), cols = 3, gridExpand = TRUE)
  conditionalFormatting(wb, "Sector Impact", cols = 2, rows = 2:(nrow(Sector_export)+1), rule = ">0", style = pos_style)
  conditionalFormatting(wb, "Sector Impact", cols = 2, rows = 2:(nrow(Sector_export)+1), rule = "<0", style = neg_style)
  
  # Sheet 3: Variable Impact
  addWorksheet(wb, "Variable Impact")
  showGridLines(wb, "Variable Impact", showGridLines = FALSE)
  writeData(wb, "Variable Impact", news_export, startRow = 1, startCol = 1, headerStyle = header_style, borders = "rows")
  setColWidths(wb, "Variable Impact", cols = 1, widths = 45)
  setColWidths(wb, "Variable Impact", cols = 2, widths = 25)
  setColWidths(wb, "Variable Impact", cols = 3:4, widths = 20)
  addStyle(wb, "Variable Impact", style = float_style, rows = 2:(nrow(news_export)+1), cols = 3, gridExpand = TRUE)
  addStyle(wb, "Variable Impact", style = pct_style, rows = 2:(nrow(news_export)+1), cols = 4, gridExpand = TRUE)
  conditionalFormatting(wb, "Variable Impact", cols = 3, rows = 2:(nrow(news_export)+1), rule = ">0", style = pos_style)
  conditionalFormatting(wb, "Variable Impact", cols = 3, rows = 2:(nrow(news_export)+1), rule = "<0", style = neg_style)
  
  # Save Workbook
  output_file <- file.path(output_dir, sprintf("Nowcast_Executive_Report_%s.xlsx", format(current_date, "%Y_%m")))
  if (!is_shiny) {
    saveWorkbook(wb, output_file, overwrite = TRUE)
    cat(sprintf("✓ Saved Report: %s\n", output_file))
  }
}

cat("\n=======================================================\n")
cat("SUCCESS: All 4 monthly executive reports generated.\n")
cat("=======================================================\n")