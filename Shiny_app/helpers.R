library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(readxl)
library(purrr)
library(tseries)
library(forecast)
library(seasonal)
library(lubridate)
library(openxlsx)
library(readr)
library(dfms)
library(xts)
library(xgboost)

run_transformations <- function(raw_data_path, td_ts, hag_ts, update_log = function(msg) { message(msg) }, update_progress = function(val, msg) {}) {
  update_log("Starting transformations...")
  update_progress(0.1, "Reading raw data and running transformations script")
  
  env <- new.env(parent = globalenv())
  env$is_shiny <- TRUE
  env$path <- raw_data_path
  env$td_ts <- td_ts
  env$hag_ts <- hag_ts
  
  # Inject logging capture
  env$message <- function(...) update_log(paste(..., collapse = " "))
  env$cat <- function(...) update_log(paste(..., collapse = " "))
  env$print <- function(x, ...) {
    if(is.character(x)) update_log(paste(x, collapse = " "))
    else update_log(paste(capture.output(base::print(x, ...)), collapse = "\n"))
  }
  
  source("transformations.r", local = env)
  
  update_progress(1.0, "Transformations Complete")
  update_log("Transformations complete.")
  
  return(list(
    combined_panel = env$combined_monthly_panel_Q_refined,
    blocks_shifted = env$blocks_shifted,
    target_raw = env$blocks_raw$target
  ))
}

run_dfm_xgboost <- function(combined_panel, target_raw, r_val = 4, p_val = 3, update_progress = NULL, update_log = NULL) {
  if (is.null(update_progress)) update_progress <- function(val, msg) {}
  if (is.null(update_log)) update_log <- function(msg) {}
  
  update_log("Starting DFM and XGBoost modeling script...")
  update_progress(0.1, "Running DFM and XGBoost script")
  
  env <- new.env(parent = globalenv())
  env$is_shiny <- TRUE
  env$df <- combined_panel
  env$target_df <- target_raw
  env$dfm_r_val <- r_val
  env$dfm_p_val <- p_val
  
  # Inject logging capture
  env$message <- function(...) update_log(paste(..., collapse = " "))
  env$cat <- function(...) update_log(paste(..., collapse = " "))
  env$print <- function(x, ...) {
    if(is.character(x)) update_log(paste(x, collapse = " "))
    else update_log(paste(capture.output(base::print(x, ...)), collapse = "\n"))
  }
  
  source("dfm_v2.r", local = env)
  
  update_progress(1.0, "DFM/XGBoost Complete")
  
  return(list(
    results = env$results,
    df_sub = env$df_sub,
    dfm_curr = env$dfm_curr,
    xgb_model_final = env$xgb_model_final,
    current_factors = env$current_factors,
    current_gdp_nowcast = env$current_gdp_nowcast,
    base_level = env$base_level,
    base_date = env$base_date,
    target_raw = target_raw,
    out_df = env$out_df
  ))
}

generate_report <- function(models_res, blocks_shifted, r_val = 4, p_val = 3, update_log = function(msg) { message(msg) }, update_progress = function(val, msg) {}) {
  update_log("Generating ensemble nowcast and report script...")
  update_progress(0.1, "Running Report script")
  
  env <- new.env(parent = globalenv())
  env$is_shiny <- TRUE
  env$df_sub <- models_res$df_sub
  env$dfm_curr <- models_res$dfm_curr
  env$current_factors <- models_res$current_factors
  env$current_gdp_nowcast <- models_res$current_gdp_nowcast
  env$base_level <- models_res$base_level
  env$xgb_model_final <- models_res$xgb_model_final
  env$blocks_shifted <- blocks_shifted
  env$dfm_r_val <- r_val
  env$dfm_p_val <- p_val
  
  # Inject logging capture
  env$message <- function(...) update_log(paste(..., collapse = " "))
  env$cat <- function(...) update_log(paste(..., collapse = " "))
  env$print <- function(x, ...) {
    if(is.character(x)) update_log(paste(x, collapse = " "))
    else update_log(paste(capture.output(base::print(x, ...)), collapse = "\n"))
  }
  
  source("report_v4.r", local = env)
  
  update_progress(1.0, "Report Generation Complete")
  update_log("Excel workbook generated successfully in memory.")
  
  return(env$wb)
}
