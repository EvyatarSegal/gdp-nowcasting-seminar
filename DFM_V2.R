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
library(vars)

while (!is.null(dev.list()))  dev.off()
par(mfrow = c(1,1))
set.seed(2026)

# Load data and drop completely empty columns
df <- read_excel("data/clean/combined_monthly_panel_Q_refined.xlsx") %>%
  dplyr::select(where(~ !all(is.na(.))))
df <- df %>% dplyr::select(-c('Net import purchase tax', 'Total Income Tax Division Net',
                              'Companies returns', 'praise tax returns', 'participation rate'))

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
# 1.5 HYPERPARAMETER OPTIMIZATION (r and p)
# ==============================================================================
# ==============================================================================
# ROBUST HYPERPARAMETER SEARCH
# ==============================================================================
r_range <- 1:8 # Keep this tight to avoid convergence issues
p_range <- 1:6
grid <- expand.grid(r = r_range, p = p_range)
grid$BIC <- NA

# Get T (number of observations)
T_obs <- nrow(X_xts)
N_vars <- ncol(X_xts)

for(i in 1:nrow(grid)){
  r_val <- grid$r[i]
  p_val <- grid$p[i]
  
  # Run without tryCatch first to see the error if it crashes
  model_temp <- DFM(X_xts, r = r_val, p = p_val, em.method = "BM")
  
  # Extract LogLikelihood (Check 'names(model_temp)' if this returns NULL)
  # Commonly 'dfms' stores loglik in 'logLik' or 'loglik'
  L <- logLik(model_temp) 
  
  # Calculate parameters
  k <- (N_vars * r_val) + (r_val^2 * p_val) + (r_val^2)
  
  # Compute BIC
  grid$BIC[i] <- -2 * L + k * log(T_obs)
  
  cat(sprintf("Tested r=%d, p=%d: BIC=%.2f\n", r_val, p_val, grid$BIC[i]))
}

# Find the best
optimal <- grid[which.min(grid$BIC), ]
cat(sprintf("\nOptimal Configuration: r=%d, p=%d\n", optimal$r, optimal$p))

# 1. Table for your report
library(knitr)
print(kable(grid, format = "simple", caption = "BIC Selection Table"))

# 2. Heatmap to visualize the BIC surface
library(ggplot2)
ggplot(grid, aes(x = factor(p), y = factor(r), fill = BIC)) +
  geom_tile() +
  scale_fill_gradient(low = "steelblue", high = "white") +
  geom_text(aes(label = round(BIC, 0)), color = "black") + # Adds numbers to the tiles
  labs(title = "BIC Optimization Surface: Finding the Ideal Model",
       x = "Number of Lags (p)",
       y = "Number of Factors (r)",
       fill = "BIC Score") +
  theme_minimal()

library(ggplot2)

# Ensure p is treated as a factor for distinct lines
grid$p_factor <- as.factor(grid$p)

# Create the Elbow Plot
ggplot(grid, aes(x = r, y = BIC, color = p_factor, group = p_factor)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Elbow Plot for BIC Selection",
       subtitle = "Look for the point where the BIC curve begins to flatten",
       x = "Number of Factors (r)",
       y = "BIC Value",
       color = "Number of Lags (p)") +
  theme(legend.position = "right") +
  # Optional: Adds a dashed line to show the visual 'elbow' if you decide on one
  geom_vline(xintercept = 3, linetype = "dashed", color = "grey50")

library(ggplot2)

# Ensure r is treated as a factor for the legend
grid$r_factor <- as.factor(grid$r)

ggplot(grid, aes(x = p, y = BIC, color = r_factor, group = r_factor)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "BIC Sensitivity to Lag Structure",
       subtitle = "Checking for parallel trends across factor counts",
       x = "Number of Lags (p)",
       y = "BIC Value",
       color = "Number of Factors (r)") +
  theme(legend.position = "right")




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

# xts type for dfms methods
xts_Q <- xts(
  x = as.matrix(df[ , !(names(df) == "Date") ]),
  order.by = df$Date
)


# create empty list to save results
ICr_report <- list()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# run ICr on xts file with Q variables
ICr_report <-ICr(xts_Q)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 3 different methods to select the number of factors
print(ICr_report)
# a "knee" like shape marks the ideal point
plot(ICr_report)
# PCA report
screeplot(ICr_report)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#                                     1:4 is for 4 factors, 1:2 is 2 factors...
var_q <- VARselect(ICr_report$F_pca[, 1:4])
var_q


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(var_q$criteria["AIC(n)",])
plot(var_q$criteria["HQ(n)",])
plot(var_q$criteria["SC(n)",])
plot(var_q$criteria["FPE(n)",])

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
# 6. EVALUATION & PLOTTING (ROBUST GRAPHICS FIX)
# ==============================================================================
# 1. Clean up missing values specifically for rendering the lines
plot_actual   <- results %>% filter(!is.na(GDP))
plot_forecast <- results %>% filter(!is.na(GDP_FCST))

# 2. Dynamically calculate a tight, accurate vertical range ignoring NAs safely
y_min <- min(c(plot_actual$GDP, plot_forecast$GDP_FCST), na.rm = TRUE)
y_max <- max(c(plot_actual$GDP, plot_forecast$GDP_FCST), na.rm = TRUE)

# 3. Initialize the base canvas using the forecast line (which has zero NAs)
plot(plot_forecast$Date, plot_forecast$GDP_FCST, type="l", col="red", lwd=2, lty=2,
     main="Real GDP vs Bridge Model Forecast (XGBoost)",
     ylab="GDP Growth / Value", xlab="Timeline",
     ylim = c(y_min, y_max))

# 4. Connect the quarterly actual GDP points using a solid black line
lines(plot_actual$Date, plot_actual$GDP, col="black", lwd=2, lty=1)

# 5. Drop clear physical points directly onto the chart where actual GDP targets sit
points(plot_actual$Date, plot_actual$GDP, col="black", pch=16, cex=1.2)

# 6. Add the descriptive chart legend
legend("bottomleft", legend=c("Actual GDP (Quarterly)","XGBoost Bridge Forecast"), 
       col=c("black","red"), lty=c(1,2), pch=c(16, NA), lwd=2)


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
# 7. OUT-OF-SAMPLE ERROR METRICS (RMSE & MAE)
# ==============================================================================
oos_results <- results %>%
  filter(Date >= test_start_date, !is.na(GDP), !is.na(GDP_FCST))

rmse <- sqrt(mean((oos_results$GDP - oos_results$GDP_FCST)^2, na.rm = TRUE))
mae  <- mean(abs(oos_results$GDP - oos_results$GDP_FCST), na.rm = TRUE)

cat("\n========== Out-of-Sample Forecast Accuracy ==========\n")
cat(sprintf("RMSE: %.4f\n", rmse))
cat(sprintf("MAE : %.4f\n", mae))
cat("=====================================================\n")

# ==============================================================================
# 8. PLOT ACTUAL VS FORECAST (WITH ERROR METRICS IN TITLE)
# ==============================================================================
plot_actual   <- results %>% filter(!is.na(GDP))
plot_forecast <- results %>% filter(!is.na(GDP_FCST))

y_min <- min(c(plot_actual$GDP, plot_forecast$GDP_FCST), na.rm = TRUE)
y_max <- max(c(plot_actual$GDP, plot_forecast$GDP_FCST), na.rm = TRUE)

plot(plot_forecast$Date, plot_forecast$GDP_FCST, type = "l", col = "red", lwd = 2, lty = 2,
     main = paste0("Real GDP vs Bridge Model Forecast (XGBoost)\nRMSE = ", round(rmse, 4),
                   " | MAE = ", round(mae, 4)),
     ylab = "GDP Growth / Value", xlab = "Timeline",
     ylim = c(y_min, y_max))

lines(plot_actual$Date, plot_actual$GDP, col = "black", lwd = 2, lty = 1)
points(plot_actual$Date, plot_actual$GDP, col = "black", pch = 16, cex = 1.2)

legend("bottomleft",
       legend = c("Actual GDP (Quarterly)", "XGBoost Bridge Forecast"),
       col = c("black", "red"), lty = c(1, 2), pch = c(16, NA), lwd = 2)



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

# Predict GDP for the current date
current_gdp_nowcast <- predict(xgb_model_final, current_factors)

# Print or store the result
cat(sprintf("\nFinal nowcast for %s: %.4f\n", results$Date[current_idx], current_gdp_nowcast))




# ==============================================================================
# 10. LOAD QUARTERLY GDP LEVEL FROM THE TARGET SHEET & COMPUTE LEVEL NOWCAST
# ==============================================================================

# Read the 'target' sheet – it contains Date and GDP (level)
target_df <- read_excel("data/raw/nowcasting_data_raw_new.xlsx", sheet = "target")

# Ensure Date is parsed as Date
target_df$Date <- as.Date(target_df$Date)

# Get the latest non‑missing GDP level (the most recent official release)
last_gdp <- target_df %>%
  filter(!is.na(GDP)) %>%
  slice_tail(n = 1)

if (nrow(last_gdp) == 0) stop("No GDP level found in the 'target' sheet.")

base_date  <- last_gdp$Date
base_level <- last_gdp$GDP   # Quarterly GDP level (e.g., in millions)

# The model nowcast is a quarterly growth rate (e.g., -0.0346 = -3.46%)
# Apply to the base level to get the next quarter's GDP
nowcast_gdp_level <- base_level * (1 + current_gdp_nowcast)

# Print the results
cat(sprintf("\nLast official GDP (%s): %.2f\n", base_date, base_level))
cat(sprintf("Nowcasted quarterly growth: %.4f (%.2f%%)\n",
            current_gdp_nowcast, current_gdp_nowcast * 100))
cat(sprintf("Nowcasted GDP level for %s: %.2f\n\n",
            results$Date[current_idx], nowcast_gdp_level))
