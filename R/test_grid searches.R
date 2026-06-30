# ==============================================================================
# XGBOOST EXPANDING WINDOW GRID SEARCH
# ==============================================================================

# 1. Define the grid
param_grid <- expand.grid(
  max_depth = c(2, 3, 4),
  eta = c(0.01, 0.05),
  subsample = c(0.7, 1.0),
  colsample_bytree = c(0.7, 1.0)
)

grid_results <- data.frame()
total_rows <- nrow(base_df)
# Ensure at least 10 rows for training; step size is 3 (quarterly)
initial_window <- max(10, floor(total_rows * 0.7))
step_size <- 3

cat("Starting Grid Search...\n")
pb <- txtProgressBar(min=0, max=nrow(param_grid), style=3)

for (i in 1:nrow(param_grid)) {
  rmse_train <- c(); rmse_test <- c()
  
  for (fe in seq(initial_window, total_rows - 1, by = step_size)) {
    train <- base_df[1:fe, ]; test <- base_df[(fe+1):min(fe+step_size, total_rows), ]
    
    # Dynamic Weighting
    w <- 1 / (1 + exp(3.968421 * ((as.numeric(difftime(max(train$Date_factor), train$Date_factor))/365.25) - 9.842105)))
    
    dtrain <- xgb.DMatrix(as.matrix(train[,c("f1","f2","f3","f4")]), label=train$GDP, weight=w)
    dtest  <- xgb.DMatrix(as.matrix(test[,c("f1","f2","f3","f4")]), label=test$GDP)
    
    m <- xgb.train(params=list(objective="reg:squarederror", max_depth=param_grid$max_depth[i],
                               eta=param_grid$eta[i], subsample=param_grid$subsample[i], 
                               colsample_bytree=param_grid$colsample_bytree[i]),
                   data=dtrain, nrounds=1000, watchlist=list(train=dtrain, test=dtest), 
                   early_stopping_rounds=10, verbose=0)
    
    rmse_train <- c(rmse_train, sqrt(mean((train$GDP - predict(m, dtrain))^2)))
    rmse_test  <- c(rmse_test, m$evaluation_log$test_rmse[m$best_iteration])
  }
  
  grid_results <- rbind(grid_results, cbind(param_grid[i,], 
                                            Train_RMSE=mean(rmse_train), Test_RMSE=mean(rmse_test)))
  setTxtProgressBar(pb, i)
}
close(pb)

# 2. Visualization: Bias-Variance Tradeoff
library(tidyr)
library(ggplot2)

plot_data <- grid_results %>% pivot_longer(cols=c(Train_RMSE, Test_RMSE), names_to="Error_Type", values_to="RMSE")

ggplot(plot_data, aes(x=max_depth, y=RMSE, color=Error_Type, group=Error_Type)) + 
  stat_summary(fun=mean, geom="line", size=1.2) + 
  theme_minimal() + 
  labs(title="Bias-Variance Tradeoff: Max Depth", y="RMSE", x="Max Depth")
