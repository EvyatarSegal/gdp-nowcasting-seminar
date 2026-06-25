library(xgboost)
library(dplyr)

# ==============================================================================
# 0. CREATE VALIDATION SET
# ==============================================================================
cat("\n========== [DEBUG] EXTRACTING VALIDATION SET ==========\n")

val_data <- results_report %>%
  filter(Date >= test_start_date) %>%
  left_join(df_clean %>% dplyr::select(Date, GDP), by = "Date") %>%
  filter(!is.na(GDP), !is.na(h0_f1), !is.na(h0_f2), !is.na(h0_f3), !is.na(h0_f4))

X_val_mat <- as.matrix(val_data[, c("h0_f1", "h0_f2", "h0_f3", "h0_f4")])
colnames(X_val_mat) <- c("f1", "f2", "f3", "f4")
y_val <- val_data$GDP

cat(sprintf("[DEBUG] Successfully loaded %d validation observations.\n", nrow(val_data)))
cat(sprintf("[DEBUG] Validation timeframe: %s to %s\n", min(val_data$Date), max(val_data$Date)))

# ==============================================================================
# ASSUMED ENVIRONMENT VARIABLES (Ensure these exist before running)
# X_mat      : Matrix of training factors (h0_f1 to h0_f4)
# y_vec      : Numeric vector of training GDP targets
# time_diff  : Numeric vector of time differences (in years) for the training set
# X_val_mat  : (Created automatically above)
# y_val      : (Created automatically above)
# ==============================================================================

cat("\n========== STARTING ULTIMATE GRID SEARCH ==========\n")

max_diff <- max(time_diff)

# Fixed XGBoost Parameters
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.015,
  max_depth = 4,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Global trackers for the ultimate winner
global_best_rmse  <- Inf
global_best_func  <- NULL
global_best_p_str <- NULL
global_formula    <- ""

# ==============================================================================
# PHASE 1: THE 1D FUNCTIONS (Fast, run directly at 0.01 precision)
# ==============================================================================
cat("\n--- PHASE 1: Running 1D Functions (0.01 precision) ---\n")

fast_functions <- list(
  power = list(
    grid = data.frame(alpha = seq(0.2, 3.0, by = 0.01)),
    compute = function(p) (time_diff + 1)^(-p$alpha),
    format = function(p) sprintf("alpha=%.2f", p$alpha),
    form_str = function(p) sprintf("(time_diff + 1)^(-%.2f)", p$alpha)
  ),
  exponential = list(
    grid = data.frame(lambda = seq(0.05, 2.0, by = 0.01)),
    compute = function(p) exp(-p$lambda * time_diff),
    format = function(p) sprintf("lambda=%.2f", p$lambda),
    form_str = function(p) sprintf("exp(-%.2f * time_diff)", p$lambda)
  ),
  linear = list(
    grid = data.frame(m = seq(0.01, 0.20, by = 0.01)),
    compute = function(p) pmax(0, 1 - p$m * time_diff),
    format = function(p) sprintf("slope=%.2f", p$m),
    form_str = function(p) sprintf("pmax(0, 1 - %.2f * time_diff)", p$m)
  ),
  binary = list(
    grid = data.frame(c = seq(0.5, max_diff, by = 0.01)),
    compute = function(p) ifelse(time_diff <= p$c, 1, 0),
    format = function(p) sprintf("cutoff=%.2f", p$c),
    form_str = function(p) sprintf("ifelse(time_diff <= %.2f, 1, 0)", p$c)
  )
)

for (func_name in names(fast_functions)) {
  f_info <- fast_functions[[func_name]]
  grid <- f_info$grid
  
  cat(sprintf("\n[DEBUG] Testing %s (%d combinations)...\n", toupper(func_name), nrow(grid)))
  
  # Setup progress bar and local trackers
  pb <- txtProgressBar(min = 0, max = nrow(grid), style = 3)
  local_best_rmse <- Inf
  local_best_p_str <- ""
  skips <- 0
  
  for (i in 1:nrow(grid)) {
    p <- grid[i, , drop = FALSE]
    w <- f_info$compute(p)
    
    if (sum(w) <= 1) {
      skips <- skips + 1
      setTxtProgressBar(pb, i)
      next
    }
    
    dtrain <- xgb.DMatrix(data = X_mat, label = y_vec, weight = as.numeric(w))
    set.seed(2026) 
    model <- xgb.train(params = params, data = dtrain, nrounds = 300, verbose = 0)
    
    preds <- predict(model, X_val_mat)
    rmse_val <- sqrt(mean((preds - y_val)^2, na.rm = TRUE))
    
    # Track Local Winner
    if (rmse_val < local_best_rmse) {
      local_best_rmse <- rmse_val
      local_best_p_str <- f_info$format(p)
    }
    
    # Track Global Winner
    if (rmse_val < global_best_rmse) {
      global_best_rmse  <- rmse_val
      global_best_func  <- func_name
      global_best_p_str <- f_info$format(p)
      global_formula    <- f_info$form_str(p)
    }
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  cat(sprintf("[RESULT] %s Winner -> %s | RMSE: %.5f (%d skipped)\n", 
              toupper(func_name), local_best_p_str, local_best_rmse, skips))
}

# ==============================================================================
# PHASE 2: THE 2D SIGMOID (Two-Stage Coarse-to-Fine Optimization)
# ==============================================================================
cat("\n--- PHASE 2: Running 2D Sigmoid (Two-Stage Optimization) ---\n")

# ------------------------------------------------------------------------------
# STAGE 1: Coarse Search (0.1 steps)
coarse_grid <- expand.grid(k = seq(1, 5, by = 0.1), t0 = seq(1, 5, by = 0.1))
cat(sprintf("\n[DEBUG] Stage 1: Coarse Search (%d combinations)...\n", nrow(coarse_grid)))

pb_coarse <- txtProgressBar(min = 0, max = nrow(coarse_grid), style = 3)
best_rmse_coarse <- Inf
best_k_coarse    <- NULL
best_t0_coarse   <- NULL
skips_coarse     <- 0

for (i in 1:nrow(coarse_grid)) {
  p <- coarse_grid[i, ]
  w <- 1 / (1 + exp(p$k * (time_diff - p$t0)))
  
  if (sum(w) <= 1) {
    skips_coarse <- skips_coarse + 1
    setTxtProgressBar(pb_coarse, i)
    next
  }
  
  dtrain <- xgb.DMatrix(data = X_mat, label = y_vec, weight = as.numeric(w))
  set.seed(2026)
  model <- xgb.train(params = params, data = dtrain, nrounds = 300, verbose = 0)
  preds <- predict(model, X_val_mat)
  rmse_val <- sqrt(mean((preds - y_val)^2, na.rm = TRUE))
  
  if (rmse_val < best_rmse_coarse) {
    best_rmse_coarse <- rmse_val
    best_k_coarse    <- p$k
    best_t0_coarse   <- p$t0
  }
  setTxtProgressBar(pb_coarse, i)
}
close(pb_coarse)
cat(sprintf("[RESULT] Coarse Sigmoid Winner -> k=%.1f, t0=%.1f | RMSE: %.5f (%d skipped)\n", 
            best_k_coarse, best_t0_coarse, best_rmse_coarse, skips_coarse))

# ------------------------------------------------------------------------------
# STAGE 2: Fine Search (0.01 steps within ±0.5 bounds)
fine_k_min <- max(0.1, best_k_coarse - 0.5)
fine_k_max <- best_k_coarse + 0.5
fine_t0_min <- max(0.1, best_t0_coarse - 0.5)
fine_t0_max <- best_t0_coarse + 0.5

fine_grid <- expand.grid(
  k  = seq(fine_k_min, fine_k_max, by = 0.01), 
  t0 = seq(fine_t0_min, fine_t0_max, by = 0.01)
)

cat(sprintf("\n[DEBUG] Stage 2: Fine Search (%d combinations)...\n", nrow(fine_grid)))
cat(sprintf("[DEBUG] Bounding Box: k=[%.2f to %.2f], t0=[%.2f to %.2f]\n", 
            fine_k_min, fine_k_max, fine_t0_min, fine_t0_max))

pb_fine <- txtProgressBar(min = 0, max = nrow(fine_grid), style = 3)
best_rmse_fine <- Inf
best_k_fine    <- NULL
best_t0_fine   <- NULL
skips_fine     <- 0

for (i in 1:nrow(fine_grid)) {
  p <- fine_grid[i, ]
  w <- 1 / (1 + exp(p$k * (time_diff - p$t0)))
  
  if (sum(w) <= 1) {
    skips_fine <- skips_fine + 1
    setTxtProgressBar(pb_fine, i)
    next
  }
  
  dtrain <- xgb.DMatrix(data = X_mat, label = y_vec, weight = as.numeric(w))
  set.seed(2026)
  model <- xgb.train(params = params, data = dtrain, nrounds = 300, verbose = 0)
  preds <- predict(model, X_val_mat)
  rmse_val <- sqrt(mean((preds - y_val)^2, na.rm = TRUE))
  
  # Track Local Fine Winner
  if (rmse_val < best_rmse_fine) {
    best_rmse_fine <- rmse_val
    best_k_fine    <- p$k
    best_t0_fine   <- p$t0
  }
  
  # Check if this fine sigmoid beats the global winner
  if (rmse_val < global_best_rmse) {
    global_best_rmse  <- rmse_val
    global_best_func  <- "sigmoid"
    global_best_p_str <- sprintf("k=%.2f, t0=%.2f", p$k, p$t0)
    global_formula    <- sprintf("1 / (1 + exp(%.2f * (time_diff - %.2f)))", p$k, p$t0)
  }
  setTxtProgressBar(pb_fine, i)
}
close(pb_fine)
cat(sprintf("[RESULT] Fine Sigmoid Winner -> k=%.2f, t0=%.2f | RMSE: %.5f (%d skipped)\n", 
            best_k_fine, best_t0_fine, best_rmse_fine, skips_fine))

# ==============================================================================
# PHASE 3: FINAL OUTPUT
# ==============================================================================
cat("\n======================================================================\n")
cat(sprintf(">> ULTIMATE WINNING CONFIGURATION: %s <<\n", toupper(global_best_func)))
cat(sprintf(">> PARAMETERS                    : %s\n", global_best_p_str))
cat(sprintf(">> VALIDATION RMSE               : %.5f\n", global_best_rmse))
cat("======================================================================\n")

cat("\nCopy/Paste this EXACT line into your main production script:\n")
cat(sprintf("weights_vec <- as.numeric(%s)\n", global_formula))