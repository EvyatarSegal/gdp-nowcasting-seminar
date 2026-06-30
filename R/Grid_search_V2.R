# ==============================================================================
# MASTER NOWCASTING PIPELINE: DFM + XGBOOST (TIME-SERIES CV)
# ==============================================================================

library(xgboost)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. SETUP & PARAMETER GRID
# ==============================================================================
param_grid <- expand.grid(
  max_depth = c(2, 3, 4),
  eta = c(0.01, 0.05, 0.1),
  subsample = c(0.7, 1.0),
  colsample_bytree = c(0.7, 1.0),
  min_child_weight = c(1, 3)
)

grid_results <- data.frame()
total_rows <- nrow(base_df)
initial_window <- max(10, floor(total_rows * 0.6))
step_size <- 3 # Test on a full quarter to ensure stability

cat(sprintf("Starting serious grid search over %d combinations...\n", nrow(param_grid)))
pb <- txtProgressBar(min=0, max=nrow(param_grid), style=3)

# 2. THE GRID SEARCH LOOP
# ==============================================================================
for (i in 1:nrow(param_grid)) {
  train_errs <- c(); test_errs <- c()
  
  # Expanding window (TimeSeries CV)
  for (fe in seq(initial_window, total_rows - step_size, by = step_size)) {
    train_idx <- 1:fe
    test_idx  <- (fe + 1):(fe + step_size)
    
    train <- base_df[train_idx, ]; test <- base_df[test_idx, ]
    
    dtrain <- xgb.DMatrix(as.matrix(train[,c("f1","f2","f3","f4")]), label=train$GDP)
    dtest  <- xgb.DMatrix(as.matrix(test[,c("f1","f2","f3","f4")]), label=test$GDP)
    
    # Train with relaxed early stopping for stability
    m <- xgb.train(params=list(objective="reg:squarederror", max_depth=param_grid$max_depth[i],
                               eta=param_grid$eta[i], subsample=param_grid$subsample[i],
                               colsample_bytree=param_grid$colsample_bytree[i],
                               min_child_weight=param_grid$min_child_weight[i]),
                   data=dtrain, nrounds=200, evals=list(test=dtest), 
                   early_stopping_rounds=20, verbose=0)
    
    # Calculate accuracy
    train_errs <- c(train_errs, sqrt(mean((train$GDP - predict(m, dtrain))^2)))
    test_errs  <- c(test_errs, m$evaluation_log$test_rmse[m$best_iteration])
  }
  
  # Save results (only if the model converged)
  if(length(test_errs) > 0) {
    grid_results <- rbind(grid_results, cbind(param_grid[i,], 
                                              Train_RMSE=mean(train_errs, na.rm=TRUE), 
                                              Test_RMSE=mean(test_errs, na.rm=TRUE)))
  }
  setTxtProgressBar(pb, i)
}
close(pb)

# 3. VISUALIZATION: BIAS-VARIANCE TRADEOFF
# ==============================================================================
dir.create("grid search graphs", showWarnings = FALSE)
plot_data <- grid_results %>% 
  filter(!is.na(Test_RMSE)) %>% 
  pivot_longer(cols = c(Train_RMSE, Test_RMSE), names_to = "Error_Type", values_to = "RMSE")

# 
for (var in names(param_grid)) {
  p <- ggplot(plot_data, aes(x = .data[[var]], y = RMSE, color = Error_Type, group = Error_Type)) +
    stat_summary(fun = mean, geom = "line", size = 1.2) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    scale_color_manual(values = c("Train_RMSE" = "steelblue", "Test_RMSE" = "darkred")) +
    theme_minimal(base_size = 14) +
    labs(title = paste("Bias-Variance Tradeoff:", var), 
         subtitle = "Divergence between Train (Blue) and Test (Red) indicates overfitting",
         y = "Mean RMSE", x = var) +
    theme(legend.position = "bottom", legend.title = element_blank())
  
  ggsave(paste0("grid search graphs/plot_", var, "_bias_variance.png"), 
         plot = p, width = 8, height = 6, dpi = 300)
  print(p)
}

# 4. BEST MODEL EXTRACTION
# ==============================================================================
best_params <- grid_results %>% arrange(Test_RMSE) %>% head(1)
cat("\nBest Parameters:\n")
print(best_params)