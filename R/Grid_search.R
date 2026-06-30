library(xgboost)
library(dplyr)
library(tidyr)
library(ggplot2)

# ==============================================================================
# 1. DEFINE THE HYPERPARAMETER GRID
# ==============================================================================
# keeping the grid tight so it doesn't take days to run
param_grid <- expand.grid(
  max_depth = c(2, 3, 4, 6),
  eta = c(0.01, 0.015, 0.03, 0.05),
  subsample = c(0.6, 0.8, 1.0),
  colsample_bytree = c(0.6, 0.8, 1.0)
)

# Initialize a dataframe to store the results
grid_results <- data.frame()

# ==============================================================================
# 2. EXPANDING WINDOW SETUP
# ==============================================================================
# Sort base_df strictly by date
base_df <- base_df %>% arrange(Date_factor)

total_rows <- nrow(base_df)
initial_window <- 60  # Start with 5 years of data (12 months * 5)
step_size <- 3        # Move forward a quarter at a time to speed up the loop

cat(sprintf("Starting grid search with %d combinations...\n", nrow(param_grid)))

# ==============================================================================
# 3. THE TUNING LOOP
# ==============================================================================
for (i in 1:nrow(param_grid)) {
  
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i]
  )
  
  fold_train_rmse <- c()
  fold_test_rmse <- c()
  
  # Expanding window loop
  for (fold_end in seq(initial_window, total_rows - 1, by = step_size)) {
    
    # Define train and test sets for this chronological split
    train_idx <- 1:fold_end
    test_idx  <- (fold_end + 1):min(fold_end + step_size, total_rows)
    
    current_train <- base_df[train_idx, ]
    current_test  <- base_df[test_idx, ]
    
    # --- Dynamic Weight Recalculation ---
    # The 'present day' shifts every fold, so weights must be recalculated
    max_date_fold <- max(current_train$Date_factor)
    time_diff_fold <- as.numeric(difftime(max_date_fold, current_train$Date_factor, units = "days")) / 365.25
    weights_fold <- as.numeric(1 / (1 + exp(3.968421 * (time_diff_fold - 9.842105))))
    
    # Matrices
    dtrain <- xgb.DMatrix(data = as.matrix(current_train[, c("f1", "f2", "f3", "f4")]), 
                          label = current_train$GDP, 
                          weight = weights_fold)
    
    dtest  <- xgb.DMatrix(data = as.matrix(current_test[, c("f1", "f2", "f3", "f4")]), 
                          label = current_test$GDP)
    
    # Train model
    # Suppress output to keep console clean during the massive loop
    xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 300, verbose = 0)
    
    # Predict & Calculate Errors
    pred_train <- predict(xgb_model, dtrain)
    pred_test  <- predict(xgb_model, dtest)
    
    rmse_train <- sqrt(mean((current_train$GDP - pred_train)^2))
    rmse_test  <- sqrt(mean((current_test$GDP - pred_test)^2))
    
    fold_train_rmse <- c(fold_train_rmse, rmse_train)
    fold_test_rmse  <- c(fold_test_rmse, rmse_test)
  }
  
  # Store the average performance across all chronological folds for this parameter combination
  grid_results <- rbind(grid_results, data.frame(
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i],
    Train_RMSE = mean(fold_train_rmse),
    Test_RMSE = mean(fold_test_rmse)
  ))
  
  # Progress tracker
  if (i %% 10 == 0) cat(sprintf("Completed %d / %d\n", i, nrow(param_grid)))
}

# Find the absolute best parameters
best_params <- grid_results %>% arrange(Test_RMSE) %>% head(1)
print("Best Hyperparameters Found:")
print(best_params)











# ==============================================================================
# 4. VISUALIZATION: BIAS-VARIANCE TRADEOFF
# ==============================================================================
library(ggplot2)
library(tidyr)

# Reshape data from wide to long format for ggplot2
plot_data <- grid_results %>% 
  pivot_longer(cols = c(Train_RMSE, Test_RMSE), 
               names_to = "Error_Type", 
               values_to = "RMSE")

# Create a function to generate plots for any hyperparameter
plot_param_effect <- function(data, param_name) {
  agg_data <- data %>% 
    group_by(!!sym(param_name), Error_Type) %>% 
    summarise(Mean_RMSE = mean(RMSE), .groups = "drop")
  
  ggplot(agg_data, aes(x = !!sym(param_name), y = Mean_RMSE, color = Error_Type, group = Error_Type)) +
    geom_line(size = 1.2) + 
    geom_point(size = 3) +
    scale_color_manual(values = c("Train_RMSE" = "steelblue", "Test_RMSE" = "darkred")) +
    theme_minimal() + 
    labs(title = paste("Overfitting Diagnostic:", param_name),
         y = "Average RMSE", 
         x = param_name) +
    theme(legend.title = element_blank())
}

# Generate the diagnostic plots
print(plot_param_effect(plot_data, "max_depth"))
print(plot_param_effect(plot_data, "eta"))
print(plot_param_effect(plot_data, "subsample"))
print(plot_param_effect(plot_data, "colsample_bytree"))






















# ==============================================================================
# MACROECONOMIC NOWCASTING PIPELINE: DFM + XGBOOST (EXPANDING WINDOW)
# ==============================================================================

library(dfms)
library(readxl)
library(dplyr)
library(lubridate)
library(xts)
library(xgboost)
library(zoo)
library(tidyr)
library(ggplot2)

while (!is.null(dev.list()))  dev.off()
par(mfrow = c(1,1))
set.seed(2026)

# ==============================================================================
# 1. DATA LOADING & PRE-PROCESSING
# ==============================================================================
cat("Loading and cleaning raw data...\n")

df <- read_excel("data/clean/combined_monthly_panel_Q_refined.xlsx") %>%
  dplyr::select(where(~ !all(is.na(.))))

df <- df %>% select(-c('Net import purchase tax', 'Total Income Tax Division Net',
                       'Companies returns', 'praise tax returns', 'participation rate'))

df$Date <- as.Date(df$Date)
df$month_year <- format(df$Date, "%Y-%m")

# Aggregate to monthly
data_agg <- df %>%
  group_by(month_year) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

df <- data_agg
df$Date <- as.Date(paste0(df$month_year, "-01"))
df <- df %>% select(-c(month_year))

# Shift the GDP column forward to align current indicators with future/concurrent GDP
df <- df %>% 
  dplyr::mutate(GDP = dplyr::lead(GDP, n = 1))

df_clean <- df %>% distinct(Date, .keep_all = TRUE)

# ==============================================================================
# 2. DYNAMIC FACTOR MODEL (DFM) EXTRACTION
# ==============================================================================
end_date   <- max(df$Date, na.rm = TRUE)
start_date <- end_date - lubridate::years(6) + months(7)
test_start_date <- start_date
all_months <- seq(start_date, end_date, by = "month")

cat(sprintf("Running DFM extraction from %s to %s...\n", start_date, end_date))

results_report <- data.frame(Date = all_months)
for (h in 0:3) {
  for (f in 1:4) {
    results_report[[paste0("h", h, "_f", f)]] <- NA_real_
  }
}

max_idx <- nrow(results_report)

for (i in seq_along(all_months)) {
  cutoff  <- all_months[i]
  month_i <- lubridate::month(cutoff)
  h_val <- 3 - (month_i - 1) %% 3
  
  df_sub <- df[df$Date <= cutoff, ]
  if (nrow(df_sub) == 0) next
  
  X_xts <- xts(as.matrix(df_sub[, !(names(df_sub) == "Date")]), order.by = df_sub$Date)
  X_xts[nrow(X_xts), "GDP"] <- NA # Blind latest GDP
  
  # Fit DFM using EM algorithm
  dfm_curr <- DFM(X = X_xts, r = 4, p = 2, quarterly.vars = "GDP", em.method = "BM")
  
  pred <- predict(dfm_curr, h = h_val, standardized = TRUE)
  F_h  <- pred$F[h_val, 1:4]
  F_h0 <- as.vector(tail(dfm_curr$F_qml, 1))[1:4]
  
  if ((i + h_val) <= max_idx) {
    results_report[i + h_val, paste0("h", h_val, "_f", 1:4)] <- F_h
  }
  results_report[i, paste0("h0_f", 1:4)] <- F_h0
}

# Combine actual GDP with extracted factors
results <- results_report %>%
  left_join(df_clean %>% dplyr::select(Date, GDP), by = "Date")

# Create a clean base dataframe for the ML phase
base_df <- results %>% 
  filter(!is.na(GDP) & !is.na(h0_f1) & !is.na(h0_f2) & !is.na(h0_f3) & !is.na(h0_f4)) %>%
  arrange(Date) # Ensure strictly chronological order

# ==============================================================================
# 3. XGBOOST EXPANDING WINDOW GRID SEARCH (FIXED)
# ==============================================================================
cat("Starting Expanding Window Hyperparameter Tuning...\n")

param_grid <- expand.grid(
  max_depth = c(2, 3, 4),
  eta = c(0.01, 0.015, 0.03),
  subsample = c(0.7, 0.85, 1.0),
  colsample_bytree = c(0.7, 0.85, 1.0)
)

grid_results <- data.frame()
total_rows <- nrow(base_df)

# --- DYNAMIC WINDOW LOGIC ---
# If you have < 20 rows, we use a smaller window. If you have more, we use 70%.
initial_window <- max(10, floor(total_rows * 0.7)) 
step_size <- ifelse(total_rows < 40, 1, 3) # Step 1 if tiny data, 3 if larger

if (initial_window >= total_rows) stop("Not enough data points to perform cross-validation.")

cat(sprintf("Using initial window of %d rows, stepping by %d.\n", initial_window, step_size))

# --- PROGRESS BAR ADDITION ---
pb <- txtProgressBar(min = 0, max = nrow(param_grid), style = 3)

for (i in 1:nrow(param_grid)) {
  
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i]
  )
  
  fold_test_rmse <- c()
  
  for (fold_end in seq(initial_window, total_rows - 1, by = step_size)) {
    
    train_idx <- 1:fold_end
    test_idx  <- (fold_end + 1):min(fold_end + step_size, total_rows)
    
    current_train <- base_df[train_idx, ]
    current_test  <- base_df[test_idx, ]
    
    # Dynamic weights
    max_date_fold <- max(current_train$Date)
    time_diff_fold <- as.numeric(difftime(max_date_fold, current_train$Date, units = "days")) / 365.25
    weights_fold <- as.numeric(1 / (1 + exp(3.968421 * (time_diff_fold - 9.842105))))
    
    dtrain <- xgb.DMatrix(data = as.matrix(current_train[, c("h0_f1", "h0_f2", "h0_f3", "h0_f4")]), 
                          label = current_train$GDP, weight = weights_fold)
    dtest  <- xgb.DMatrix(data = as.matrix(current_test[, c("h0_f1", "h0_f2", "h0_f3", "h0_f4")]), 
                          label = current_test$GDP)
    
    watchlist <- list(train = dtrain, test = dtest)
    xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 1500, 
                           watchlist = watchlist, early_stopping_rounds = 20, verbose = 0)
    
    fold_test_rmse <- c(fold_test_rmse, xgb_model$evaluation_log$test_rmse[xgb_model$best_iteration])
  }
  
  grid_results <- rbind(grid_results, data.frame(
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i],
    Test_RMSE = mean(fold_test_rmse)
  ))
  
  setTxtProgressBar(pb, i)
}
close(pb)

# ==============================================================================
# 4. BAYESIAN TUNING VISUALIZATION
# ==============================================================================
# ParBayesianOptimization has a built-in plotting function for the search process
plot(opt_results) 

# Alternatively, see how parameters converged:
plot(opt_results$scoreSummary$Iteration, opt_results$scoreSummary$Score,
     type = "b", pch = 16, col = "darkred",
     main = "Bayesian Optimization Convergence",
     xlab = "Iteration", ylab = "Negative RMSE")
grid()

# ==============================================================================
# 5. FINAL PRODUCTION MODEL & CURRENT NOWCAST
# ==============================================================================
cat("\nTraining final production model on full dataset...\n")

final_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  subsample = best_params$subsample,
  colsample_bytree = best_params$colsample_bytree
)

# Recalibrate weights for the entire historical dataset
max_date_all <- max(base_df$Date)
time_diff_all <- as.numeric(difftime(max_date_all, base_df$Date, units = "days")) / 365.25
weights_all   <- as.numeric(1 / (1 + exp(3.968421 * (time_diff_all - 9.842105))))

dtrain_final <- xgb.DMatrix(data = as.matrix(base_df[, c("h0_f1", "h0_f2", "h0_f3", "h0_f4")]), 
                            label = base_df$GDP, 
                            weight = weights_all)

# Note: In production, we run the model out to a safe nrounds determined by the CV average, 
# or use a standard conservative threshold since we don't have a future test set to early-stop against.
xgb_model_final <- xgb.train(params = final_params, data = dtrain_final, nrounds = 150)

# Extract current factors (the most recent month from our DFM results)
current_idx <- nrow(results)
current_factors <- as.matrix(results[current_idx, c("h0_f1", "h0_f2", "h0_f3", "h0_f4")])

# Predict Growth
current_gdp_growth <- predict(xgb_model_final, current_factors)

# ==============================================================================
# 6. CALCULATE FINAL NOMINAL/LEVEL NOWCAST
# ==============================================================================
target_df <- read_excel("data/raw/nowcasting_data_raw.xlsx", sheet = "target")
target_df$Date <- as.Date(target_df$Date)

last_gdp <- target_df %>% filter(!is.na(GDP)) %>% slice_tail(n = 1)
base_date  <- last_gdp$Date
base_level <- last_gdp$GDP  

nowcast_gdp_level <- base_level * (1 + current_gdp_growth)

cat("=====================================================\n")
cat(sprintf("Last official GDP (%s): %.2f\n", base_date, base_level))
cat(sprintf("Nowcasted quarterly growth: %.4f (%.2f%%)\n", current_gdp_growth, current_gdp_growth * 100))
cat(sprintf("Nowcasted GDP level for %s: %.2f\n", results$Date[current_idx], nowcast_gdp_level))
cat("=====================================================\n")