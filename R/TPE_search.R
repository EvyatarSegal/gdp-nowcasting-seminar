# ==============================================================================
# MASTER MACROECONOMIC NOWCASTING PIPELINE: DFM + BAYESIAN XGBOOST
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
library(ParBayesianOptimization)

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

cat(sprintf("\nRunning DFM extraction from %s to %s...\n", start_date, end_date))

results_report <- data.frame(Date = all_months)
for (h in 0:3) {
  for (f in 1:4) {
    results_report[[paste0("h", h, "_f", f)]] <- NA_real_
  }
}

max_idx <- nrow(results_report)

# --- PROGRESS BAR FOR DFM ---
pb_dfm <- txtProgressBar(min = 0, max = length(all_months), style = 3)

for (i in seq_along(all_months)) {
  cutoff  <- all_months[i]
  month_i <- lubridate::month(cutoff)
  h_val <- 3 - (month_i - 1) %% 3
  
  df_sub <- df[df$Date <= cutoff, ]
  if (nrow(df_sub) > 0) {
    X_xts <- xts(as.matrix(df_sub[, !(names(df_sub) == "Date")]), order.by = df_sub$Date)
    X_xts[nrow(X_xts), "GDP"] <- NA # Blind latest GDP
    
    # Supress warnings inside the loop to keep progress bar clean
    suppressWarnings({
      dfm_curr <- DFM(X = X_xts, r = 4, p = 2, quarterly.vars = "GDP", em.method = "BM")
    })
    
    pred <- predict(dfm_curr, h = h_val, standardized = TRUE)
    F_h  <- pred$F[h_val, 1:4]
    F_h0 <- as.vector(tail(dfm_curr$F_qml, 1))[1:4]
    
    if ((i + h_val) <= max_idx) {
      results_report[i + h_val, paste0("h", h_val, "_f", 1:4)] <- F_h
    }
    results_report[i, paste0("h0_f", 1:4)] <- F_h0
  }
  
  setTxtProgressBar(pb_dfm, i)
}
close(pb_dfm)

# Combine actual GDP with extracted factors and format column names for ML pipeline
base_df <- results_report %>%
  left_join(df_clean %>% dplyr::select(Date, GDP), by = "Date") %>%
  filter(!is.na(GDP) & !is.na(h0_f1) & !is.na(h0_f2) & !is.na(h0_f3) & !is.na(h0_f4)) %>%
  rename(Date_factor = Date, 
         f1 = h0_f1, f2 = h0_f2, f3 = h0_f3, f4 = h0_f4) %>%
  arrange(Date_factor) # Ensure strict chronological order

# ==============================================================================
# 3. BAYESIAN HYPERPARAMETER OPTIMIZATION
# ==============================================================================
cat("\nStarting Bayesian Optimization Process...\n")

scoring_function <- function(max_depth, min_child_weight, eta, subsample, 
                             colsample_bytree, reg_alpha, reg_lambda) {
  
  fold_test_rmse <- c()
  total_rows <- nrow(base_df)
  
  # DYNAMIC WINDOW FIX: Auto-adjust to the size of the filtered dataset
  initial_window <- max(10, floor(total_rows * 0.7)) 
  step_size <- ifelse(total_rows < 40, 1, 3) # Step 1 row if quarterly, 3 if monthly
  
  if (initial_window >= total_rows) return(list(Score = -9999)) # Failsafe
  
  for (fold_end in seq(initial_window, total_rows - 1, by = step_size)) {
    
    train_idx <- 1:fold_end
    test_idx  <- (fold_end + 1):min(fold_end + step_size, total_rows)
    
    current_train <- base_df[train_idx, ]
    current_test  <- base_df[test_idx, ]
    
    # Dynamic Temporal Weighting
    max_date_fold <- max(current_train$Date_factor) 
    time_diff_fold <- as.numeric(difftime(max_date_fold, current_train$Date_factor, units = "days")) / 365.25
    weights_fold <- as.numeric(1 / (1 + exp(3.968421 * (time_diff_fold - 9.842105))))
    
    # Compile DMatrices
    dtrain <- xgb.DMatrix(data = as.matrix(current_train[, c("f1", "f2", "f3", "f4")]), 
                          label = current_train$GDP, 
                          weight = weights_fold)
    
    dtest  <- xgb.DMatrix(data = as.matrix(current_test[, c("f1", "f2", "f3", "f4")]), 
                          label = current_test$GDP)
    
    # XGBoost Parameters
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      max_depth = as.integer(max_depth), 
      min_child_weight = min_child_weight, 
      eta = eta,
      subsample = subsample,
      colsample_bytree = colsample_bytree,
      alpha = reg_alpha,                 
      lambda = reg_lambda                
    )
    
    # Train with Early Stopping
    watchlist <- list(train = dtrain, test = dtest)
    xgb_model <- xgb.train(params = params, 
                           data = dtrain, 
                           nrounds = 5000, 
                           watchlist = watchlist, 
                           early_stopping_rounds = 20, 
                           verbose = 0)
    
    # Extract Optimal RMSE
    best_iter <- xgb_model$best_iteration
    fold_test_rmse  <- c(fold_test_rmse, xgb_model$evaluation_log$test_rmse[best_iter])
  }
  
  return(list(Score = -mean(fold_test_rmse)))
}

search_bounds <- list(
  max_depth = c(2L, 4L),            # Depth 6 is often too deep for <20 rows
  min_child_weight = c(1, 3),       # Lowered to ensure splits can actually happen
  eta = c(0.01, 0.2),               # Broader range for learning rate
  subsample = c(0.6, 1.0),          
  colsample_bytree = c(0.6, 1.0),   
  reg_alpha = c(0, 1),              # Reduced penalty range to prevent zeroing out weights
  reg_lambda = c(0, 1)              
)

opt_results <- bayesOpt(
  FUN = scoring_function,
  bounds = search_bounds,
  initPoints = 10,  
  iters.n = 30,     
  iters.k = 1,      
  verbose = 1       
)

best_params <- getBestPars(opt_results)

cat("\n=====================================================\n")
cat("Bayesian Optimization Complete. Best Parameters:\n")
print(best_params)
cat("=====================================================\n")

# ==============================================================================
# 4. EMPIRICALLY DETERMINE FINAL NROUNDS
# ==============================================================================
cat("\nCalculating mathematically justified nrounds for the final model...\n")

final_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = as.integer(best_params$max_depth),
  min_child_weight = best_params$min_child_weight,
  eta = best_params$eta,
  subsample = best_params$subsample,
  colsample_bytree = best_params$colsample_bytree,
  alpha = best_params$reg_alpha,
  lambda = best_params$reg_lambda
)

optimal_nrounds_history <- c()
total_rows <- nrow(base_df)

# Dynamic Window Fix applied here as well
initial_window <- max(10, floor(total_rows * 0.7)) 
step_size <- ifelse(total_rows < 40, 1, 3) 

folds <- seq(initial_window, total_rows - 1, by = step_size)

# --- PROGRESS BAR FOR NROUNDS ---
pb_cv <- txtProgressBar(min = 0, max = length(folds), style = 3)

for (f_idx in seq_along(folds)) {
  fold_end <- folds[f_idx]
  
  train_idx <- 1:fold_end
  test_idx  <- (fold_end + 1):min(fold_end + step_size, total_rows)
  
  current_train <- base_df[train_idx, ]
  current_test  <- base_df[test_idx, ]
  
  max_date_fold <- max(current_train$Date_factor)
  time_diff_fold <- as.numeric(difftime(max_date_fold, current_train$Date_factor, units = "days")) / 365.25
  weights_fold <- as.numeric(1 / (1 + exp(3.968421 * (time_diff_fold - 9.842105))))
  
  dtrain <- xgb.DMatrix(data = as.matrix(current_train[, c("f1", "f2", "f3", "f4")]), 
                        label = current_train$GDP, weight = weights_fold)
  dtest  <- xgb.DMatrix(data = as.matrix(current_test[, c("f1", "f2", "f3", "f4")]), 
                        label = current_test$GDP)
  
  watchlist <- list(train = dtrain, test = dtest)
  
  xgb_model_cv <- xgb.train(params = final_params, 
                            data = dtrain, 
                            nrounds = 5000, 
                            watchlist = watchlist, 
                            early_stopping_rounds = 20, 
                            verbose = 0)
  
  optimal_nrounds_history <- c(optimal_nrounds_history, xgb_model_cv$best_iteration)
  setTxtProgressBar(pb_cv, f_idx)
}
close(pb_cv)

# Calculate the justified nrounds (scaled median)
justified_nrounds <- round(median(optimal_nrounds_history) * 1.05)

cat("\nHistorical optimal iterations:", paste(optimal_nrounds_history, collapse = ", "), "\n")
cat(sprintf("Selected nrounds for final production model: %d\n", justified_nrounds))

# ==============================================================================
# 5. TRAIN FINAL PRODUCTION MODEL & GENERATE NOWCAST
# ==============================================================================
cat("\nTraining final production model on full dataset...\n")

# Recalibrate weights for the entire dataset
max_date_all <- max(base_df$Date_factor)
time_diff_all <- as.numeric(difftime(max_date_all, base_df$Date_factor, units = "days")) / 365.25
weights_all   <- as.numeric(1 / (1 + exp(3.968421 * (time_diff_all - 9.842105))))

dtrain_final <- xgb.DMatrix(data = as.matrix(base_df[, c("f1", "f2", "f3", "f4")]), 
                            label = base_df$GDP, 
                            weight = weights_all)

# Train the final model
xgb_model_final <- xgb.train(params = final_params, 
                             data = dtrain_final, 
                             nrounds = justified_nrounds)

# Extract current factors
current_idx <- nrow(results_report)
current_factors <- as.matrix(results_report[current_idx, c("h0_f1", "h0_f2", "h0_f3", "h0_f4")])
colnames(current_factors) <- c("f1", "f2", "f3", "f4")

# Predict out-of-sample GDP growth
current_gdp_growth <- predict(xgb_model_final, current_factors)

# Calculate final level
target_df <- read_excel("data/raw/nowcasting_data_raw.xlsx", sheet = "target")
target_df$Date <- as.Date(target_df$Date)
last_gdp <- target_df %>% filter(!is.na(GDP)) %>% slice_tail(n = 1)
base_date  <- last_gdp$Date
base_level <- last_gdp$GDP  

nowcast_gdp_level <- base_level * (1 + current_gdp_growth)

cat("\n=====================================================\n")
cat(sprintf("Last official GDP (%s): %.2f\n", base_date, base_level))
cat(sprintf("Nowcasted quarterly growth: %.4f (%.2f%%)\n", current_gdp_growth, current_gdp_growth * 100))
cat(sprintf("Nowcasted GDP level for %s: %.2f\n", results_report$Date[current_idx], nowcast_gdp_level))
cat("=====================================================\n")