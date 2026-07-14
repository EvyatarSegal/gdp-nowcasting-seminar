

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

if (!exists("is_shiny")) is_shiny <- FALSE
if (!is_shiny) {
  while (!is.null(dev.list()))  dev.off()
  par(mfrow = c(1,1))
}
set.seed(2026)

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
#    (non-numeric columns like original Date are dropped; month_year stays)
data_agg <- df %>%
  group_by(month_year) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

df <- data_agg

# Convert month_year to Date (first day of month)
df$Date <- as.Date(paste0(df$month_year, "-01"))



df <- df %>% dplyr::select(-c(month_year))

# Shift the GDP column "one up" as requested (places NA at the very end)
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
print(paste("Running rolling forecast from", start_date, "to", end_date))

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
  
  # FIX 1: Dynamic mathematical horizon (replaces the fragile if/else block)
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
  
  # --- Safe Data Assignment & Index-Out-Of-Bounds Protection ---
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

# 1. Safely extract factors from the final loop iteration and map dates
all_factors <- data.frame(dfm_curr$F_qml)
colnames(all_factors) <- c("f1", "f2", "f3", "f4")
all_factors$Date <- tail(df_sub$Date, nrow(all_factors))

# 2. Extract ONLY the rows from df_clean where a shifted GDP actually exists
y_train_prep <- df_clean %>%
  filter(!is.na(GDP)) %>%
  dplyr::select(Date, GDP) %>%
  mutate(YM = format(Date, "%Y-%m"))

# 3. Create a unique, clean factor lookup table using the same Year-Month strings
x_train_prep <- all_factors %>%
  mutate(YM = format(Date, "%Y-%m")) %>%
  filter(YM %in% y_train_prep$YM) %>%
  distinct(YM, .keep_all = TRUE) # Heavy shield: Resolves any double-date memory splits

# 4. Join precisely on YM and apply the out-of-sample boundary check
train_data <- x_train_prep %>%
  inner_join(y_train_prep, by = "YM", suffix = c("_factor", "_gdp")) %>%
  filter(Date_factor < test_start_date)

# 5. Rebuild the C++ matrix structure cleanly using an aligned base data frame
base_df <- as.data.frame(train_data)
X_mat   <- as.matrix(base_df[, c("f1", "f2", "f3", "f4")])
mode(X_mat) <- "double" # Clear 64-bit float alignment assignment

y_vec   <- as.numeric(base_df$GDP)

# 6. Weights calculation
max_date    <- max(base_df$Date_factor)
time_diff   <- as.numeric(difftime(max_date, base_df$Date_factor, units = "days")) / 365.25
weights_vec <- as.numeric(1 / (1 + exp(3.968421 * (time_diff - 9.842105))))

# 7. Compile the DMatrix safely
dtrain <- xgb.DMatrix(data = X_mat, label = y_vec, weight = weights_vec)

# ==============================================================================
# 5. XGBOOST TRAINING & PREDICTION
# ==============================================================================
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.015,
  max_depth = 4,       
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 300)

# Extract test inputs (only rows with fully populated factors)
X_test_factors <- results_report[rowSums(is.na(results_report[ , -1])) < (ncol(results_report) - 1), ]
X_test_mat <- as.matrix(X_test_factors[, c("h0_f1","h0_f2","h0_f3", "h0_f4")])
colnames(X_test_mat) <- c("f1","f2","f3", "f4")

# Predict out-of-sample
pred_test <- predict(xgb_model, X_test_mat)

# ==============================================================================
# 4.1 VISUALIZE XGBOOST TEMPORAL WEIGHTS
# ==============================================================================
# Sort the data strictly by date just in case it's out of order before plotting
weight_df <- data.frame(Date = base_df$Date_factor, Weight = weights_vec)
weight_df <- weight_df[order(weight_df$Date), ]

# Plot the calculated weights against the timeline to visualize the decay effect
plot(weight_df$Date, weight_df$Weight, type = "l", col = "blue", lwd = 2,
     main = "XGBoost Temporal Observation Weights",
     ylab = "Assigned Weight", 
     xlab = "Timeline")

# Add gridlines for readability and points to show the actual data density
grid()
points(weight_df$Date, weight_df$Weight, col = "blue", pch = 16, cex = 0.8)

# ==============================================================================
# 6. BUILD RESULTS DATA FRAME WITH ACTUAL & FORECAST
# ==============================================================================
results <- results_report

# Merge actual quarterly GDP (aligned to month of release)
results <- results %>%
  left_join(df_clean %>% dplyr::select(Date, GDP), by = "Date")

# Add forecast column (initialised with NA)
results$GDP_FCST <- NA

# Identify rows where all four h0 factors are available (used as input to XGBoost)
idx_forecast <- which(rowSums(is.na(results[, c("h0_f1","h0_f2","h0_f3","h0_f4")])) == 0)
results$GDP_FCST[idx_forecast] <- pred_test


# ==============================================================================
# 6. BUILD RESULTS DATA FRAME WITH FORECASTS FOR ALL HORIZONS
# ==============================================================================
results <- results_report

# Merge actual GDP from the cleaned data frame
results <- results %>%
  left_join(df_clean %>% dplyr::select(Date, GDP), by = "Date")

# Initialize forecast columns for all 4 horizons (h0 to h3)
for (h in 0:3) {
  col_f <- paste0("h", h, "_f", 1:4)
  fcst_col_name <- paste0("GDP_FCST_h", h)
  results[[fcst_col_name]] <- NA_real_
  
  # Identify rows where all factor inputs exist for the current horizon
  idx_forecast <- which(rowSums(is.na(results[, col_f])) == 0)
  
  if (length(idx_forecast) > 0) {
    X_test_mat <- as.matrix(results[idx_forecast, col_f])
    colnames(X_test_mat) <- c("f1", "f2", "f3", "f4")
    
    # Predict using the trained XGBoost model with horizon-specific factors
    results[[fcst_col_name]][idx_forecast] <- predict(xgb_model, X_test_mat)
  }
}

# ==============================================================================
# 7. OUT-OF-SAMPLE ERROR METRICS FOR ALL HORIZONS (THE COMPARISON)
# ==============================================================================
# Filter data for the designated out-of-sample (OOS) testing period
oos_results <- results %>%
  filter(Date >= test_start_date, !is.na(GDP))

# Create an empty summary table to store metrics across horizons
horizon_comparison <- data.frame(
  Horizon = c("h0 (Nowcast - Month of Release)", 
              "h1 (1 Month before end of Quarter)", 
              "h2 (2 Months before end of Quarter)", 
              "h3 (3 Months before end of Quarter)"),
  RMSE = NA_real_,
  MAE = NA_real_
)

# Calculate RMSE and MAE for each horizon separately
for (h in 0:3) {
  fcst_col <- paste0("GDP_FCST_h", h)
  
  # Calculate errors only where both forecast and actual GDP are non-missing
  eval_sub <- oos_results %>% filter(!is.na(.data[[fcst_col]]))
  
  if (nrow(eval_sub) > 0) {
    horizon_comparison$RMSE[h + 1] <- sqrt(mean((eval_sub$GDP - eval_sub[[fcst_col]])^2))
    horizon_comparison$MAE[h + 1]  <- mean(abs(eval_sub$GDP - eval_sub[[fcst_col]]))
  }
}

# Print the final benchmark table to the console
cat("\n============================================================\n")
cat("          HORIZON BACKTESTING ACCURACY COMPARISON\n")
cat("============================================================\n")
print(horizon_comparison)
cat("============================================================\n")


# ==============================================================================
# 8. VISUALIZATIONS: RMSE COMPARISON & FORECAST PATHS
# ==============================================================================
# Setup a 2-row, 1-column plotting layout
par(mfrow = c(2, 1))

# Plot 1: Barplot comparing RMSE across the different horizons
barplot(horizon_comparison$RMSE, 
        names.arg = c("h0 (Nowcast)", "h1 (1m Lag)", "h2 (2m Lag)", "h3 (3m Lag)"),
        col = c("darkgreen", "blue", "orange", "red"),
        main = "RMSE of GDP Forecast by Horizon\n(Lower is Better)",
        ylab = "RMSE", xlab = "Forecast Horizon")
grid(nx = NA, ny = NULL)

# Plot 2: Line plot comparing actual GDP to selected forecast horizons
plot_actual <- results %>% filter(!is.na(GDP))

y_min <- min(c(plot_actual$GDP, results$GDP_FCST_h0, results$GDP_FCST_h3), na.rm = TRUE)
y_max <- max(c(plot_actual$GDP, results$GDP_FCST_h0, results$GDP_FCST_h3), na.rm = TRUE)

plot(plot_actual$Date, plot_actual$GDP, type = "o", col = "black", lwd = 3, pch = 16,
     main = "Real GDP vs. Forecasts from Different Horizons",
     ylab = "GDP Growth / Value", xlab = "Timeline",
     ylim = c(y_min, y_max))

# Add the short-term Nowcast (h0) line (dashed green)
lines(results$Date, results$GDP_FCST_h0, col = "darkgreen", lwd = 2, lty = 2)

# Add the long-term Forecast (h3) line (dotted red)
lines(results$Date, results$GDP_FCST_h3, col = "red", lwd = 2, lty = 3)

# Add a comprehensive legend
legend("bottomleft",
       legend = c("Actual GDP", "Nowcast (h0 - late)", "Forecast (h3 - early)"),
       col = c("black", "darkgreen", "red"), lty = c(1, 2, 3), pch = c(16, NA, NA), lwd = 2)

# Reset graphic parameters to default
par(mfrow = c(1,1))
# ==============================================================================
# 9. TRAIN FINAL XGBOOST ON ALL HISTORICAL DATA & NOWCAST CURRENT GDP
# ==============================================================================

# Extract all rows where both historical factors and shifted GDP exist
final_train <- results %>%
  filter(!is.na(GDP) & 
           !is.na(h0_f1) & !is.na(h0_f2) & !is.na(h0_f3) & !is.na(h0_f4))

# Features and target
X_all <- as.matrix(final_train[, c("h0_f1", "h0_f2", "h0_f3", "h0_f4")])
y_all <- final_train$GDP

# Recalculate weights using the full sample, giving more importance to recent data
max_date_all <- max(final_train$Date)
time_diff_all <- as.numeric(difftime(max_date_all, final_train$Date, units = "days")) / 365.25
weights_all   <- as.numeric(1 / (1 + exp(3.968421 * (time_diff_all - 9.842105))))

# Build DMatrix for the final training
dtrain_final <- xgb.DMatrix(data = X_all, label = y_all, weight = weights_all)

# Train the final model (using the same hyperparameters)
xgb_model_final <- xgb.train(params = params, data = dtrain_final, nrounds = 300)

# The "current date" is the last month in our results (most recent observation)
current_idx <- nrow(results)
current_factors <- as.matrix(results[current_idx, c("h0_f1", "h0_f2", "h0_f3", "h0_f4")])
colnames(current_factors) <- c("f1", "f2", "f3", "f4") # Ensure column names match training

# Predict GDP for the current date
current_gdp_nowcast <- predict(xgb_model_final, current_factors)

# Print or store the result
cat(sprintf("\nFinal nowcast for %s: %.4f\n", results$Date[current_idx], current_gdp_nowcast))


# ==============================================================================
# 10. LOAD QUARTERLY GDP LEVEL FROM THE TARGET SHEET & COMPUTE LEVEL NOWCAST
# ==============================================================================

# Read the 'target' sheet – it contains Date and GDP (level)
if (!exists("target_df")) {
  target_df <- read_excel("data/raw/nowcasting_data_raw_new.xlsx", sheet = "target")
}

# Ensure Date is parsed as Date
target_df$Date <- as.Date(target_df$Date)

# Get the latest non‑missing GDP level (the most recent official release)
last_gdp <- target_df %>%
  filter(!is.na(GDP)) %>%
  slice_tail(n = 1)

if (nrow(last_gdp) == 0) stop("No GDP level found in the 'target' sheet.")

base_date  <- last_gdp$Date
base_level <- last_gdp$GDP   # Quarterly GDP level (e.g., in millions)

# Apply predicted growth rate to the latest available actual level
nowcast_gdp_level <- base_level * (1 + current_gdp_nowcast)

# Print the results
cat(sprintf("\nLast official GDP (%s): %.2f\n", base_date, base_level))
cat(sprintf("Nowcasted quarterly growth: %.4f (%.2f%%)\n",
            current_gdp_nowcast, current_gdp_nowcast * 100))
cat(sprintf("Nowcasted GDP level for %s: %.2f\n\n",
            results$Date[current_idx], nowcast_gdp_level))


# ==============================================================================
# BENCHMARKING: AR(1) MODEL & THEIL'S U CALCULATION (FIXED)
# ==============================================================================

# FIX 1: Extract the RMSE of your h0 model calculated in Section 7
rmse_h0 <- horizon_comparison$RMSE[1] # h0 is the first row of your comparison table

# FIX 2: Correct Lag calculation for sparse quarterly GDP in a monthly timeline
benchmark_data <- oos_results %>%
  dplyr::select(Date, GDP, GDP_FCST_h0) %>%
  filter(!is.na(GDP)) %>%              # Work only with periods that have actual GDP
  mutate(GDP_lag1 = lag(GDP)) %>%      # Now lag(GDP) correctly gets the previous quarter
  filter(!is.na(GDP_lag1))             # Drop the first row since it won't have a lag

# 2. Fit simple AR(1) model: GDP_t = alpha + beta * GDP_{t-1}
ar1_model <- lm(GDP ~ GDP_lag1, data = benchmark_data)
benchmark_data$GDP_AR1 <- predict(ar1_model, newdata = benchmark_data)

# 3. Calculate Benchmark RMSE
rmse_ar1 <- sqrt(mean((benchmark_data$GDP - benchmark_data$GDP_AR1)^2, na.rm = TRUE))

# 4. Calculate Theil's U (Ratio < 1 means your model is better than naive)
theil_u <- rmse_h0 / rmse_ar1

# 5. Calculate Relative RMSE (Normalized by the standard deviation of actual GDP)
sd_actual <- sd(benchmark_data$GDP, na.rm = TRUE)
rrmse <- rmse_h0 / sd_actual

# 6. Report Comparison Metrics
cat("\n========== Model Benchmarking (Theil's U) ==========\n")
cat(sprintf("Your Nowcast (h0) RMSE : %.4f\n", rmse_h0))
cat(sprintf("AR(1) Model RMSE       : %.4f\n", rmse_ar1))
cat(sprintf("Theil's U Ratio        : %.4f\n", theil_u))
cat(sprintf("Relative RMSE          : %.4f\n", rrmse))
cat("=====================================================\n")

if(theil_u < 1) {
  cat("Result: Your model successfully outperforms the naive AR(1) benchmark!\n")
} else {
  cat("Result: Your model does not currently outperform a simple AR(1) baseline.\n")
}

# 7. Visualization of Model vs Naive Baseline
plot(benchmark_data$Date, benchmark_data$GDP, type="o", lwd=3, pch=16,
     main="Performance vs Naive Benchmark (AR1)", ylab="GDP Growth", xlab="Date")
lines(benchmark_data$Date, benchmark_data$GDP_FCST_h0, col="red", lwd=2, lty=2, type="o", pch=17)
lines(benchmark_data$Date, benchmark_data$GDP_AR1, col="blue", lwd=2, lty=3, type="o", pch=15)
legend("bottomleft", legend=c("Actual GDP", "Your Nowcast (h0)", "AR(1) Baseline"),
       col=c("black", "red", "blue"), lty=c(1, 2, 3), pch=c(16, 17, 15), lwd=2)