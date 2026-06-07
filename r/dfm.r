## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dfms)
library(readxl)
library(dplyr)
library(zoo)
library(purrr)
library(xts)
library(lubridate)
library(vars)
library(openxlsx)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

set.seed(2026)


df <- read_excel("data/clean/combined_monthly_panel_Q_refined.xlsx")

df <- df %>%
  select(where(~ !all(is.na(.))))



df$Date <- as.Date(df$Date)

# xts type for dfms methods
xts_Q <- xts(
  x = as.matrix(df[ , !(names(df) == "Date") ]),
  order.by = df$Date
)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# this function is retriving the orignial value of a X_imp at h=0
unscale_var <- function(dfm_obj, var_name, std_value) {
  stats <- attr(dfm_obj$X_imp, "stats")
  idx <- which(colnames(dfm_obj$X_imp) == var_name)
  mu <- stats[idx, "Mean"]
  sigma <- stats[idx, "SD"]
  return(std_value * sigma + mu)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# --- Load Data ---

df <- read_excel("data/clean/combined_monthly_panel_Q_refined.xlsx")
df$Date <- as.Date(df$Date)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# --- Define rolling dates ---
start_date <- as.Date("2021-01-01") # test start - forecast is generated for this
end_date   <- as.Date("2025-01-01") # date of the last forecast

all_months <- seq(start_date, end_date, by = "month")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create a result report
# this will hold the forecast from different time points
results_report <- data.frame(
  Date = all_months,
  h0_fcst = NA, # day of end of quarter forecast
  h1_fcst = NA, # 1 month before end of quarter forecast
  h2_fcst = NA, # 2 months before end of quarter forecast
  h3_fcst = NA # 3 months before end of quarter forecast
)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all_months = head(all_months, -1)
for (i in seq_along(all_months)) {

  cutoff <- all_months[i]
  month_i <- month(cutoff)

  # set dynamic horizon
  if (month_i %in% c(1,4,7,10)) {
    h_val <- 3
  } else if (month_i %in% c(2,5,8,11)) {
    h_val <- 2
  } else {
    h_val <- 1
  }
  
  # this creates a copy data set with all the availble data till date
  df_sub <- df[df$Date <= cutoff, ]
  X_xts <- xts(
    as.matrix(df_sub[ , !(names(df_sub) == "Date") ]),
    order.by = df_sub$Date
  )

  X_xts[nrow(X_xts), "GDP"] <- NA # remove GDP value

  dfm_curr <- DFM(
    X = X_xts,
    r = 4, # adjust for you number of factors
    p = 2, # adjust for your lags
    quarterly.vars = "GDP",
    em.method = "BM"
  )

  pred <- predict(dfm_curr, h = h_val, standardized = FALSE) # stnd=False keep it
  gdp_fcst <- pred$X_fcst[h_val, "GDP"] # the GDP forecast from prediction
  x_std <- tail(dfm_curr$X_imp[,"GDP"], 1) # this is adjusting the EoQ day value
  gdp_nowcast_day_of_pub <- unscale_var(dfm_curr, "GDP", x_std)

  
  # store forecast in the correct column
  results_report$h0_fcst[i] <- gdp_nowcast_day_of_pub
  if (h_val == 1) results_report$h1_fcst[i+h_val] <- gdp_fcst
  if (h_val == 2) results_report$h2_fcst[i+h_val] <- gdp_fcst
  if (h_val == 3) results_report$h3_fcst[i+h_val] <- gdp_fcst
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

write_forecast_info <- function(results_report, file_path = "output/forecast.xlsx") {
  wb <- openxlsx::createWorkbook()
  
  for (bn in names(results_report)) {
    openxlsx::addWorksheet(wb, bn)
    openxlsx::writeData(wb, bn, results_report[[bn]])
  }
  
  openxlsx::saveWorkbook(wb, file = file_path, overwrite = TRUE)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

write_forecast_info(results_report) # full results


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# this chunk organizes only quarterly forecasts and this is where your acutal relevant results go to

# create an empty list for the results
quarterly_fcst <- list() 

# add a month col
results_report$month <- as.integer(format(results_report$Date, "%m"))

# keep only the quarter end results and add the real value of GDP (log-diff)
quarterly_fcst <- results_report[results_report$month %in% c(1,4,7,10), ]
quarterly_fcst <- left_join(quarterly_fcst, df[, c("Date", "GDP")],
                            by = join_by(Date))

# format dates
quarterly_fcst$Date <- as.Date(quarterly_fcst$Date)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

write_forecast_info(quarterly_fcst, "output/quarterly_fcst.xlsx")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# end of quarter day
RMSE <- sqrt(mean((quarterly_fcst$h0_fcst - quarterly_fcst$GDP)^2, na.rm = TRUE))
MAE  <- mean(abs(quarterly_fcst$h0_fcst - quarterly_fcst$GDP), na.rm = TRUE)

RMSE
MAE


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1 month before end of quarter
RMSE <- sqrt(mean((quarterly_fcst$h1_fcst - quarterly_fcst$GDP)^2, na.rm = TRUE))
MAE  <- mean(abs(quarterly_fcst$h1_fcst - quarterly_fcst$GDP), na.rm = TRUE)

RMSE
MAE


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 2 months before end of quarter
RMSE <- sqrt(mean((quarterly_fcst$h2_fcst - quarterly_fcst$GDP)^2, na.rm = TRUE))
MAE  <- mean(abs(quarterly_fcst$h2_fcst - quarterly_fcst$GDP), na.rm = TRUE)

RMSE
MAE


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 3 months before end of quarter
RMSE <- sqrt(mean((quarterly_fcst$h3_fcst - quarterly_fcst$GDP)^2, na.rm = TRUE))
MAE  <- mean(abs(quarterly_fcst$h3_fcst - quarterly_fcst$GDP), na.rm = TRUE)

RMSE
MAE


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# average forecast between all 4 horizons
quarterly_fcst$h_avg_fcst <- rowMeans(
  quarterly_fcst[, c("h0_fcst", "h1_fcst", "h2_fcst", "h3_fcst")],
  na.rm = TRUE
)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# errors for average forecast
RMSE <- sqrt(mean((quarterly_fcst$h_avg_fcst - quarterly_fcst$GDP)^2, na.rm = TRUE))
MAE  <- mean(abs(quarterly_fcst$h_avg_fcst - quarterly_fcst$GDP), na.rm = TRUE)

RMSE
MAE


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot the results with time line 
plot(quarterly_fcst$Date, quarterly_fcst$GDP, type="l",
     col="black", lwd=2,
     xaxt="n",
     xlab="",      # remove default label
     ylim = range(c(
       quarterly_fcst$GDP,
       quarterly_fcst$h1_fcst,
       quarterly_fcst$h2_fcst,
       quarterly_fcst$h3_fcst
     ), na.rm = TRUE),
     main="Real GDP vs DFM Forecasts (h=1,2,3)",
     ylab="GDP")

# add or removes lines as you want
lines(quarterly_fcst$Date, quarterly_fcst$h1_fcst, col = "green",   lwd = 2)
lines(quarterly_fcst$Date, quarterly_fcst$h2_fcst, col = "red",  lwd = 2)
lines(quarterly_fcst$Date, quarterly_fcst$h3_fcst, col = "blue", lwd = 2)

legend("bottomleft",
       legend=c("Real GDP", "h=1", "h=2", "h=3"),
       col=c("black","green", "red", "blue"),
       lwd=2)

# Add monthly ticks
axis(1,
     at = quarterly_fcst$Date,
     labels = format(quarterly_fcst$Date, "%b %Y"),
     las = 2)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
quarterly_fcst$gdp_diff  <- sign(quarterly_fcst$GDP - dplyr::lag(quarterly_fcst$GDP))
quarterly_fcst$h1_diff   <- sign(quarterly_fcst$h1_fcst - dplyr::lag(quarterly_fcst$h1_fcst))
quarterly_fcst$h2_diff   <- sign(quarterly_fcst$h2_fcst - dplyr::lag(quarterly_fcst$h2_fcst))
quarterly_fcst$h3_diff   <- sign(quarterly_fcst$h3_fcst - dplyr::lag(quarterly_fcst$h3_fcst))
quarterly_fcst$h0_diff   <- sign(quarterly_fcst$h0_fcst - dplyr::lag(quarterly_fcst$h0_fcst))



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
direction_accuracy <- function(actual, forecast) {
  mean(actual == forecast, na.rm = TRUE)
}

direction_accuracy_h0 <- direction_accuracy(quarterly_fcst$gdp_diff, quarterly_fcst$h0_diff)
direction_accuracy_h1 <- direction_accuracy(quarterly_fcst$gdp_diff, quarterly_fcst$h1_diff)
direction_accuracy_h2 <- direction_accuracy(quarterly_fcst$gdp_diff, quarterly_fcst$h2_diff)
direction_accuracy_h3 <- direction_accuracy(quarterly_fcst$gdp_diff, quarterly_fcst$h3_diff)

c(h0 = direction_accuracy_h0,
  h1 = direction_accuracy_h1,
  h2 = direction_accuracy_h2,
  h3 = direction_accuracy_h3)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
contrib_F1 <- data.frame(
  variable = colnames(X_xts),
  loading  = dfm_curr$C[, 1], # Factor number e.g. 1,2,3,4...
  abs_loading = abs(dfm_curr$C[, 1]) # Factor number e.g. 1,2,3,4...
)

contrib_F1 <- contrib_F1[order(-contrib_F1$abs_loading), ]



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
contrib_F2 <- data.frame(
  variable = colnames(X_xts),
  loading  = dfm_curr$C[, 2], # Factor number e.g. 1,2,3,4...
  abs_loading = abs(dfm_curr$C[, 2]) # Factor number e.g. 1,2,3,4...
)

contrib_F2 <- contrib_F2[order(-contrib_F2$abs_loading), ]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
contrib_F3 <- data.frame(
  variable = colnames(X_xts),
  loading  = dfm_curr$C[, 3], # Factor number e.g. 1,2,3,4...
  abs_loading = abs(dfm_curr$C[, 3]) # Factor number e.g. 1,2,3,4...
)

contrib_F3 <- contrib_F3[order(-contrib_F3$abs_loading), ]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
contrib_F4 <- data.frame(
  variable = colnames(X_xts),
  loading  = dfm_curr$C[, 4], # Factor number e.g. 1,2,3,4...
  abs_loading = abs(dfm_curr$C[, 4]) # Factor number e.g. 1,2,3,4...
)

contrib_F4 <- contrib_F4[order(-contrib_F4$abs_loading), ]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# plot the factors by method of choice (qml, pca, 2s)
plot(dfm_curr, type="individual", method="qml")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
factors_qml <- dfm_curr$F_qml


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# print a full report of the DFM generated
summary(dfm_curr)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# --- Load Data ---

df <- read_excel("data/clean/combined_monthly_panel_Q_refined.xlsx")
df$Date <- as.Date(df$Date)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# --- Define rolling dates ---
start_date <- as.Date("2020-12-01")
end_date   <- as.Date("2025-01-01")

library(lubridate)

# --- Define DYNAMIC rolling dates ---

# 1. Automatically set the end date to the absolute latest month in your dataset
end_date <- max(df$Date, na.rm = TRUE)

# 2. Define your out-of-sample testing window. 
# If you want to keep testing on the most recent ~4 years of data, you can dynamically subtract:
start_date <- end_date - years(4) 
# OR just set a new explicit cutoff for testing: start_date <- as.Date("2022-01-01")

all_months <- seq(start_date, end_date, by = "month")

# Print to verify your new timeline
print(paste("Running rolling forecast from", start_date, "to", end_date))

all_months <- seq(start_date, end_date, by = "month")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# for each factor save the value for the EoQ date
results_report <- data.frame(
  Date = all_months,
  h0_f1 = NA, h0_f2 = NA, h0_f3 = NA, h0_f4 = NA,
  h1_f1 = NA, h1_f2 = NA, h1_f3 = NA, h1_f4 = NA,
  h2_f1 = NA, h2_f2 = NA, h2_f3 = NA, h2_f4 = NA,
  h3_f1 = NA, h3_f2 = NA, h3_f3 = NA, h3_f4 = NA
)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all_months <- head(all_months, -1)

for (i in seq_along(all_months)) {

  cutoff <- all_months[i]
  month_i <- month(cutoff)

  # dynamic horizon (unchanged)
  if (month_i %in% c(1,4,7,10)) {
    h_val <- 3
  } else if (month_i %in% c(2,5,8,11)) {
    h_val <- 2
  } else {
    h_val <- 1
  }

  # subset panel
  df_sub <- df[df$Date <= cutoff, ]
  X_xts <- xts(
    as.matrix(df_sub[, !(names(df_sub) == "Date")]),
    order.by = df_sub$Date
  )

  # GDP missing (unchanged)
  X_xts[nrow(X_xts), "GDP"] <- NA

  # run DFM
  dfm_curr <- DFM(
    X = X_xts,
    r = 4,
    p = 2,
    quarterly.vars = "GDP",
    em.method = "BM"
  )

  # predict factors
  pred <- predict(dfm_curr, h = h_val, standardized = TRUE)

  # factor names (assuming F has r=3)
  F_names <- colnames(pred$F)

    # H0 factor values (from smoothed factors)
  F_h0 <- tail(dfm_curr$F_qml, 1)  # vector length 3
  
  # forecasted factor values
  F_h <- pred$F[h_val, ]   # vector length 3
  
  # Store the forecast depending on horizon
  if (h_val == 1) {
    results_report$h1_f1[i+h_val] <- F_h[1]
    results_report$h1_f2[i+h_val] <- F_h[2]
    results_report$h1_f3[i+h_val] <- F_h[3]
    results_report$h1_f4[i+h_val] <- F_h[4]
  }

  if (h_val == 2) {
    results_report$h2_f1[i+h_val] <- F_h[1]
    results_report$h2_f2[i+h_val] <- F_h[2]
    results_report$h2_f3[i+h_val] <- F_h[3]
    results_report$h2_f4[i+h_val] <- F_h[4]
  }

  if (h_val == 3) {
    results_report$h3_f1[i+h_val] <- F_h[1]
    results_report$h3_f2[i+h_val] <- F_h[2]
    results_report$h3_f3[i+h_val] <- F_h[3]
    results_report$h3_f4[i+h_val] <- F_h[4]
    # Store H0 (always at row i)
    results_report$h0_f1[i] <- F_h0[1]
    results_report$h0_f2[i] <- F_h0[2]
    results_report$h0_f3[i] <- F_h0[3]
    results_report$h0_f4[i] <- F_h0[4]
  }

}




## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(xgboost)
library(dplyr)
library(zoo)

# 1. Sync the XGBoost test window with the DFM loop we defined earlier
test_start_date <- start_date 

# 2. Clean the original dataset to remove duplicate months
df_clean <- df %>% distinct(Date, .keep_all = TRUE)

# 3. Extract factors and align to the dataset's dates
all_factors <- data.frame(dfm_curr$F_qml)
colnames(all_factors) <- c("f1", "f2", "f3", "f4")
all_factors$Date <- df_sub$Date

# 4. Prepare Predictors (X) - Keep only target factor months (1, 4, 7, 10)
x_train_prep <- all_factors %>%
  filter(as.integer(format(Date, "%m")) %in% c(1, 4, 7, 10)) %>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  distinct(Quarter, .keep_all = TRUE) # Ensure exactly 1 factor row per quarter

# 5. Prepare Target (y) - Keep only actual GDP values
y_train_prep <- df_clean %>%
  filter(!is.na(GDP)) %>%
  dplyr::select(Date, GDP) %>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  distinct(Quarter, .keep_all = TRUE) # Ensure exactly 1 GDP value per quarter

# 6. Join safely by QUARTER and filter dynamically
train_data <- x_train_prep %>%
  inner_join(y_train_prep, by = "Quarter", suffix = c("_factor", "_gdp")) %>%
  # DYNAMIC FILTER: Cuts off right before testing begins to prevent data leakage
  filter(Date_factor < test_start_date)

# 7. Split into matrix and vector
X_mat <- as.matrix(train_data[, c("f1", "f2", "f3", "f4")])
y_vec <- train_data$GDP

# 8. Recalculate weights based on your rolling origin config
max_date <- max(train_data$Date_factor)
time_diff_years <- as.numeric(difftime(max_date, train_data$Date_factor, units = "days")) / 365.25
weights_vec <- 1 / (1 + exp(3.968421 * (time_diff_years - 9.842105)))

# 9. Build the DMatrix safely
dtrain <- xgb.DMatrix(data = X_mat, label = y_vec, weight = weights_vec)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.015,
  max_depth = 4,       
  subsample = 0.8,
  colsample_bytree = 0.8
)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 300
)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
X_test_factors <- results_report[rowSums(is.na(results_report[ , -1])) < ncol(results_report) - 1, ]



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
X_test_mat <- as.matrix(X_test_factors[, c("h0_f1","h0_f2","h0_f3", "h0_f4")])
colnames(X_test_mat) <- c("f1","f2","f3", "f4")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred_test <- predict(xgb_model, X_test_mat)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
results <- data.frame(
  Date = X_test_factors$Date,
  GDP_FCST = pred_test
)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
results <- left_join(results, df[, c("Date", "GDP")],
                            by = join_by(Date))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(results$Date, results$GDP, type="l", col="blue", lwd=2)
lines(results$Date, results$GDP_FCST, col="red", lwd=2)
legend("bottomleft", legend=c("Actual","Forecast"), col=c("blue","red"), lwd=2)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
RMSE <- sqrt(mean((results$GDP_FCST - results$GDP)^2, na.rm = TRUE))
MAE  <- mean(abs(results$GDP_FCST - results$GDP), na.rm = TRUE)

RMSE
MAE


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# I used this to plot comparison of end of quarter day results of DFM and Bridge method
plot(quarterly_fcst$Date, quarterly_fcst$GDP, type="l",
     col="black", lwd=2,
     xaxt="n",
     xlab="",      # remove default label
     ylim = range(c(
       quarterly_fcst$GDP,
       quarterly_fcst$h0_fcst
     ), na.rm = TRUE),
     main="Real GDP vs Nowcast (DFM and Bridge)",
     ylab="GDP")

lines(quarterly_fcst$Date, quarterly_fcst$h0_fcst, col = "green",   lwd = 2)
lines(results$Date, results$GDP_FCST, col="red", lwd=2)

legend("bottomleft",
       legend=c("Real GDP", "h=0 (internal)", "h=0 (bridge)"),
       col=c("black","green", "red", "blue"),
       lwd=2)

# Add monthly ticks
axis(1,
     at = quarterly_fcst$Date,
     labels = format(quarterly_fcst$Date, "%b %Y"),
     las = 2)

