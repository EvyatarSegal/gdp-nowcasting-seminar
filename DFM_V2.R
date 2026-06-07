

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

set.seed(2026)

# Load data and drop completely empty columns
df <- read_excel("data/clean/combined_monthly_panel_Q_refined_nuran.xlsx") %>%
  dplyr::select(where(~ !all(is.na(.))))

df$Date <- as.Date(df$Date)

# Shift the GDP column "one up" as requested (places NA at the very end)
df <- df %>% 
  dplyr::mutate(GDP = dplyr::lead(GDP, n = 1))

# Clean the dataset for later merging
df_clean <- df %>% distinct(Date, .keep_all = TRUE)

# ==============================================================================
# 2. DYNAMIC TIMELINE & PRE-ALLOCATION
# ==============================================================================
end_date   <- max(df$Date, na.rm = TRUE)
start_date <- end_date - lubridate::years(1) 
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
    X = X_xts, r = 4, p = 2,
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
# 6. EVALUATION & PLOTTING
# ==============================================================================
results <- data.frame(Date = X_test_factors$Date, GDP_FCST = pred_test)
results$YM <- format(results$Date, "%Y-%m")

# FIX: Create the YM column in df_clean on the fly so the join works seamlessly
df_clean_with_ym <- df_clean %>% 
  mutate(YM = format(Date, "%Y-%m"))

# Merge via YM to ensure bulletproof alignment with actual GDP
results <- results %>% 
  left_join(df_clean_with_ym[, c("YM", "GDP")], by = "YM")

eval_results <- results[!is.na(results$GDP), ]

if(nrow(eval_results) > 0) {
  RMSE <- sqrt(mean((eval_results$GDP_FCST - eval_results$GDP)^2))
  MAE  <- mean(abs(eval_results$GDP_FCST - eval_results$GDP))
  print(paste("XGBoost Bridge RMSE:", round(RMSE, 5)))
  print(paste("XGBoost Bridge MAE:", round(MAE, 5)))
} else {
  print("Warning: No matching actual GDP records found for the forecast evaluation window.")
}

# Final Output Plot
plot(results$Date, results$GDP, type="l", col="black", lwd=2,
     main="Real GDP vs Bridge Model Forecast (XGBoost)",
     ylab="GDP", xlab="Timeline",
     ylim = range(c(results$GDP, results$GDP_FCST), na.rm = TRUE))

lines(results$Date, results$GDP_FCST, col="red", lwd=2, lty=2)
legend("bottomleft", legend=c("Actual GDP","XGBoost Bridge Forecast"), 
       col=c("black","red"), lty=c(1,2), lwd=2)



