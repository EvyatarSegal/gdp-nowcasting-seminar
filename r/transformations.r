## --------------------------------------------------------------------------------------
# Load every session
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


## --------------------------------------------------------------------------------------
save_list_to_excel <- function(lst, file_path = "output.xlsx") {
  # lst: a named list of data.frames
  # file_path: output Excel file path
  
  # Require openxlsx
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required but not installed.")
  }
  
  # Create workbook
  wb <- openxlsx::createWorkbook()
  
  # Loop through list elements and add worksheets
  for (sheet_name in names(lst)) {
    
    # Add worksheet
    openxlsx::addWorksheet(wb, sheet_name)
    
    # Write the data frame
    openxlsx::writeData(wb,
                        sheet = sheet_name,
                        x = lst[[sheet_name]])
  }
  
  # Save workbook
  openxlsx::saveWorkbook(wb,
                         file = file_path,
                         overwrite = TRUE)
  
  message("Saved Excel file: ", file_path)
}



## --------------------------------------------------------------------------------------
path <- "C:/Users/noran/OneDrive/מסמכים/סמינר מעשי/PROJ_DFM/PROJ_DFM/nowcasting_data_raw.xlsx"


## --------------------------------------------------------------------------------------
sheets <- excel_sheets(path)


## --------------------------------------------------------------------------------------
blocks_raw <- sheets %>% 
  set_names() %>% 
  map(~ read_excel(path, sheet = .x))

blocks_real <- sheets %>% 
  set_names() %>% 
  map(~ read_excel(path, sheet = .x))


## --------------------------------------------------------------------------------------
blocks_raw <- map(blocks_raw, ~ {
  df <- .x
  df$Date <- as.Date(df$Date)
  df
})

blocks_real <- map(blocks_raw, ~ {
  df <- .x
  df$Date <- as.Date(df$Date)
  df
})


## --------------------------------------------------------------------------------------
adjust_block_for_cpi <- function(block_df, cpi_df, cols = "all") {
  # join by date
  df <- block_df %>%
    dplyr::left_join(cpi_df %>% dplyr::select(Date, CPI), by = "Date")
  
  # CPI test for NA's
  if (any(is.na(df$CPI))) {
    warning("There are NA values in CPI after merging. Check date alignment.")
  }
  
  # select cols
  if (identical(cols, "all")) {
    cols_to_adjust <- setdiff(names(df), c("Date", "CPI"))
  } else {
    cols_to_adjust <- cols
  }
  
  # adjust for real values
  for (col in cols_to_adjust) {
    # skip non-exisitng cols
    if (!col %in% names(df)) {
      warning(paste("Column", col, "not found in block. Skipping."))
      next
    }
    
    # override with real val
    df[[col]] <- df[[col]] / (df$CPI / 100)
  }
  
    # remove CPI
    df <- df %>% dplyr::select(-CPI)
  
  return(df)
}



## --------------------------------------------------------------------------------------
blocks_real$personal_labor_income_taxes <-
  adjust_block_for_cpi(blocks_raw$personal_labor_income_taxes,
                       blocks_raw$adjusters)


## --------------------------------------------------------------------------------------
blocks_real$corporate_business_tax <-
  adjust_block_for_cpi(blocks_raw$corporate_business_tax,
                       blocks_raw$adjusters)


## --------------------------------------------------------------------------------------
blocks_real$consumption_tax <-
  adjust_block_for_cpi(blocks_raw$consumption_tax,
                       blocks_raw$adjusters)


## --------------------------------------------------------------------------------------
blocks_real$import_trade_tax <-
  adjust_block_for_cpi(blocks_raw$import_trade_tax,
                       blocks_raw$adjusters)


## --------------------------------------------------------------------------------------
blocks_real$real_estate <-
  adjust_block_for_cpi(blocks_raw$real_estate,
                       blocks_raw$adjusters,
                       cols = c("Real estate taxation",
                                "Property tax",
                                "praise tax",
                                "Real estate purchase tax",
                                "praise tax returns",
                                "purchase returns"))


## --------------------------------------------------------------------------------------
blocks_real$real_activity$Oil <- blocks_raw$real_activity$Oil * blocks_raw$FX_liqudity$Dollar


## --------------------------------------------------------------------------------------
blocks_real$real_activity <-
  adjust_block_for_cpi(blocks_real$real_activity,
                       blocks_real$adjusters,
                       cols = "Oil")


## --------------------------------------------------------------------------------------
blocks_real$FX_liqudity$`Foreign exchange reserves (millions of dollars)` <- blocks_raw$FX_liqudity$`Foreign exchange reserves (millions of dollars)` * blocks_raw$FX_liqudity$Dollar


## --------------------------------------------------------------------------------------
blocks_real$FX_liqudity <-
  adjust_block_for_cpi(blocks_real$FX_liqudity,
                       blocks_real$adjusters,
                       cols = "Foreign exchange reserves (millions of dollars)")


## --------------------------------------------------------------------------------------
head(blocks_real$FX_liqudity$`Foreign exchange reserves (millions of dollars)`)


## --------------------------------------------------------------------------------------
head(blocks_raw$FX_liqudity$`Foreign exchange reserves (millions of dollars)`)


## --------------------------------------------------------------------------------------
save_list_to_excel(blocks_real, "blocks_real.xlsx")


## --------------------------------------------------------------------------------------
td <- read_csv("C:/Users/noran/OneDrive/מסמכים/סמינר מעשי/PROJ_DFM/PROJ_DFM/data_for_sa/td_var.csv")
td_ts <- ts(
  td[, -1],
  start = c(year(min(td$date)), month(min(td$date))),
  frequency = 12
)

preadj <- read_excel("C:/Users/noran/OneDrive/מסמכים/סמינר מעשי/PROJ_DFM/PROJ_DFM/data_for_sa/hol_preadj.xlsx") %>%
  mutate(date = as.Date(date))

hag_ts <- ts(
  preadj[, -1],
  start = c(year(min(preadj$date)), month(min(preadj$date))),
  frequency = 12
)


## --------------------------------------------------------------------------------------
 seasonal_adjust_block <- function(block_df,
                                  columns = "all",
                                  hag_ts,
                                  td_ts,
                                  x11 = TRUE,
                                  outlier_types = "ao") {
  
  # Ensure 'Date' column exists
  if (!"Date" %in% names(block_df)) {
    stop("The block must contain a 'Date' column.")
  }
  
  # Select variables to process
  if (identical(columns, "all")) {
    vars <- setdiff(names(block_df), "Date")
  } else {
    vars <- columns
  }
  
  # Output containers
  info_list <- list()
  df_out <- block_df
  
  # Loop through variables
  for (v in vars) {
    message("Running SA on variable: ", v)
    
    x <- df_out[[v]]
    
    # Skip if all NA
    if (all(is.na(x))) {
      info_list[[v]] <- data.frame(
        variable = v,
        status = "Skipped - all NA",
        transform = NA,
        arima = NA,
        outliers = NA,
        stringsAsFactors = FALSE
      )
      next
    }
    
    # Identify leading NA region
    first_non_na <- min(which(!is.na(x)))
    last_non_na  <- max(which(!is.na(x)))
    segment <- x[first_non_na:last_non_na]
    
    # Skip if NA inside the segment (not allowed in X-13)
    if (any(is.na(segment))) {
      info_list[[v]] <- data.frame(
        variable = v,
        status = "Skipped - NA inside the series",
        transform = NA,
        arima = NA,
        outliers = NA,
        stringsAsFactors = FALSE
      )
      next
    }
    
    # Create TS object using full date range
    y <- ts(
      segment,
      start = c(year(df_out$Date[first_non_na]),
                month(df_out$Date[first_non_na])),
      frequency = 12
    )
    
    # Run X-13
    fit <- tryCatch({
      if (x11) {
        seas(
          y,
          x11 = "",
          outlier.types = outlier_types,
          transform.function = "auto",
          xreg = cbind(hag_ts, td_ts),
          regression.usertype = rep("holiday", ncol(hag_ts))
        )
      } else {
        seas(
          y,
          outlier.types = outlier_types,
          transform.function = "auto",
          xreg = cbind(hag_ts, td_ts),
          regression.usertype = rep("holiday", ncol(hag_ts))
        )
      }
    }, error = function(e) {
      info_list[[v]] <<- data.frame(
        variable = v,
        status = paste("SA failed:", e$message),
        transform = NA,
        arima = NA,
        outliers = NA,
        stringsAsFactors = FALSE
      )
      return(NULL)
    })
    
    if (is.null(fit)) next
    
    # Extract metadata
    trans_used <- tryCatch(as.character(fit$series$transfunc)[1], error = function(e) NA)
    arima_used <- tryCatch(paste(as.character(fit$est$arima), collapse = "; "), error = function(e) NA)
    outliers_used <- tryCatch(paste(as.character(fit$outlier$Type), collapse = ", "), error = function(e) NA)
    
    info_list[[v]] <- data.frame(
      variable = v,
      status = "OK",
      transform = trans_used,
      arima = arima_used,
      outliers = outliers_used,
      stringsAsFactors = FALSE
    )
    
    # --- Corrected assignment with leading NA preserved ---
    adj <- as.numeric(predict(fit))
    
    # Fill inside the original vector
    result_vec <- x
    result_vec[first_non_na:last_non_na] <- adj
    
    df_out[[v]] <- result_vec
  }
  
  return(list(
    data = df_out,
    info = dplyr::bind_rows(info_list)
  ))
}



## --------------------------------------------------------------------------------------
blocks_sa <- blocks_real


## --------------------------------------------------------------------------------------
sa_info <- list()


## --------------------------------------------------------------------------------------
out <- seasonal_adjust_block(
    blocks_real$real_estate, # choose your block
    columns = c("Real estate taxation",
                "Property tax",
                "praise tax",
                "Real estate purchase tax",
                "praise tax returns",
                "purchase returns"),
    hag_ts = hag_ts,
    td_ts = td_ts
)

blocks_sa$real_estate <- out$data # override with the SA data
sa_info$real_estate   <- out$info # save trans meta-data



## --------------------------------------------------------------------------------------
write_sa_info <- function(sa_info, file_path = "sa_info.xlsx") {
  wb <- createWorkbook()
  
  for (bn in names(sa_info)) {
    addWorksheet(wb, bn)
    writeData(wb, bn, sa_info[[bn]])
  }
  
  saveWorkbook(wb, file = file_path, overwrite = TRUE)
}


## --------------------------------------------------------------------------------------
write_sa_info(sa_info, "SA_results_01.xlsx")


## --------------------------------------------------------------------------------------
save_list_to_excel(blocks_sa, "blocks_sa_01.xlsx")


## --------------------------------------------------------------------------------------
test_transform <- function(x, freq = 12, alpha = 0.05) {
  
  x_clean <- na.omit(x)
  if (length(x_clean) < 24) return("too_short")
  
  # 1. Seasonality check with STL
  seas_strength <- NA
  has_seasonality <- FALSE
  if (freq > 1 && length(x_clean) > freq * 2) {
    stl_obj <- tryCatch(stl(ts(x_clean, frequency = freq), s.window = "periodic"), error = function(e) NULL)
    if (!is.null(stl_obj)) {
      seas <- stl_obj$time.series[, "seasonal"]
      resid <- stl_obj$time.series[, "remainder"]
      seas_strength <- max(0, 1 - var(resid) / var(seas + resid))
      has_seasonality <- seas_strength > 0.3
    }
  }
  
  # 2. ADF test
  adf_p <- tryCatch(adf.test(x_clean)$p.value, error = function(e) NA)
  
  # 3. KPSS test
  kpss_p <- tryCatch(kpss.test(x_clean)$p.value, error = function(e) NA)
  
  # 4. LOG possible?
  positive_only <- all(x_clean > 0)
  
  # 5. Logic
  adf_stationary <- !is.na(adf_p)  && adf_p < alpha
  kpss_stationary <- !is.na(kpss_p) && kpss_p > alpha
  
  # Base decision
  if (adf_stationary && kpss_stationary) {
    base <- "none"
  } else if (!adf_stationary && !kpss_stationary) {
    base <- "diff"
  } else if (adf_stationary && !kpss_stationary) {
    base <- "detrend"
  } else {
    base <- "diff"
  }
  
  # Prefer LOGDIFF if possible
  if (base == "diff" && positive_only) base <- "logdiff"
  
  # Add seasonality
  if (has_seasonality) {
    if (base == "none") {
      base <- "seasonal_diff"
    } else {
      base <- paste0(base, "+seasonal_diff")
    }
  }
  
  return(base)
}


## --------------------------------------------------------------------------------------
# These are the model input blocks. Keep adjusters and target unchanged.
blocks_to_transform <- setdiff(names(blocks_sa), c("adjusters", "target"))

transformation_recommendations <- blocks_sa[blocks_to_transform] |>
  purrr::map(function(block) {
    sapply(block %>% dplyr::select(-Date), test_transform)
  })

transformation_recommendations


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transform_block <- function(block_df,
                            codes_vector,
                            hag_ts,
                            td_ts,
                            freq = 12,
                            sa_codes = c(4,5,6,7,8)) {
  
  block_df <- as.data.frame(block_df)
  if (!"Date" %in% names(block_df)) stop("Date column missing.")
  
  vars <- setdiff(names(block_df), "Date")
  if (length(vars) != length(codes_vector)) stop("codes length mismatch.")
  
  out_df <- block_df
  info_list <- list()
  
  for (i in seq_along(vars)) {
    
    varname <- vars[i]
    code <- codes_vector[i]
    x <- out_df[[varname]] 
    n <- length(x)
    
    message("Processing variable: ", varname)
    
    sa_method <- "None"
    
    # ---------------------------------------------------
    # 1. SEASONAL ADJUSTMENT USING seasonal_adjust_block
    # ---------------------------------------------------
    
    if (code %in% sa_codes) {
      
      sa_res <- seasonal_adjust_block(
        block_df = data.frame(Date = block_df$Date, x = x),
        columns = "x",
        hag_ts = hag_ts,
        td_ts = td_ts
      )
      
      status <- sa_res$info$status
      
      if (status == "OK") {
        x <- sa_res$data$x
        sa_method <- "X13"
      } else {
        message("X13 failed for ", varname, " → using STL fallback")
        
        valid_idx <- which(!is.na(x))
        if (length(valid_idx) > freq * 2) { # וודוא שיש מספיק נתונים ל-STL
          first_non_na <- min(valid_idx)
          last_non_na  <- max(valid_idx)
          segment <- x[first_non_na:last_non_na]
          
          y <- ts(segment, 
                  start = c(year(block_df$Date[first_non_na]), 
                            month(block_df$Date[first_non_na])), 
                  frequency = freq)
          
          stl_fit <- tryCatch({ stl(y, s.window = "periodic", robust = TRUE) }, error = function(e) NULL)
          
          if (!is.null(stl_fit)) {
            adj <- as.numeric(y - stl_fit$time.series[, "seasonal"])
            x_adj <- x
            x_adj[first_non_na:last_non_na] <- adj
            x <- x_adj
            sa_method <- "STL"
          } else {
            sa_method <- "Failed"
          }
        }
      }
    }
    
    # ---------------------------------------------------
    # 2. APPLY TRANSFORMATION CODE
    # ---------------------------------------------------
    
    result <- rep(NA, n)
    
    safe_log <- function(vec) {
      if (any(vec <= 0, na.rm = TRUE)) {
        message("Non-positive values found in ", varname, " - using log(x+1) fallback")
        return(log(vec + 1))
      }
      return(log(vec))
    }
    
    if (code == 0) {
      result <- x
    } else if (code == 1) {
      result <- safe_log(x)
    } else if (code == 2) {
      result[2:n] <- diff(x)
    } else if (code == 3) {
      result[2:n] <- diff(safe_log(x))
    } else if (code == 4) {
      result <- x # SA already handled seasonality
    } else if (code == 5) {
      result <- safe_log(x)
    } else if (code == 6) {
      result[2:n] <- diff(x)
    } else if (code == 7) {
      result[2:n] <- diff(safe_log(x))
    } else if (code == 8) {
      valid_idx <- which(!is.na(x))
      if(length(valid_idx) > 0) {
        first <- min(valid_idx); last <- max(valid_idx)
        t_seg <- seq_len(last - first + 1)
        result[first:last] <- residuals(lm(x[first:last] ~ t_seg))
      }
    }
    
    out_df[[varname]] <- result 
    
    info_list[[varname]] <- data.frame(
      variable = varname,
      code = code,
      sa_method = sa_method,
      transformation = c("none","log","diff","logdiff",
                         "sa_only","sa_log",
                         "sa_diff","sa_logdiff",
                         "detrend")[ifelse(is.na(code), 0, code) + 1],
      stringsAsFactors = FALSE
    )
  }
  
  return(list(
    data = out_df,
    info = dplyr::bind_rows(info_list)
  ))
}




## --------------------------------------------------------------------------------------
transform_block <- function(block_df,
                            codes_vector,
                            hag_ts,
                            td_ts,
                            freq = 12,
                            sa_codes = c(4,5,6,7,8)) {
  
  block_df <- as.data.frame(block_df)
  if (!"Date" %in% names(block_df)) stop("Date column missing.")
  
  vars <- setdiff(names(block_df), "Date")
  if (length(vars) != length(codes_vector)) stop("codes length mismatch.")
  
  out_df <- block_df
  info_list <- list()
  
  for (i in seq_along(vars)) {
    
    varname <- vars[i]
    code <- codes_vector[i]
    x <- out_df[[varname]] 
    n <- length(x)
    
    message("Processing variable: ", varname)
    
    sa_method <- "None"
    
    # ---------------------------------------------------
    # 1. SEASONAL ADJUSTMENT USING seasonal_adjust_block
    # ---------------------------------------------------
    
    if (code %in% sa_codes) {
      
      sa_res <- seasonal_adjust_block(
        block_df = data.frame(Date = block_df$Date, x = x),
        columns = "x",
        hag_ts = hag_ts,
        td_ts = td_ts
      )
      
      status <- sa_res$info$status
      
      if (status == "OK") {
        x <- sa_res$data$x
        sa_method <- "X13"
      } else {
        message("X13 failed for ", varname, " → using STL fallback")
        
        valid_idx <- which(!is.na(x))
        if (length(valid_idx) > freq * 2) { # וודוא שיש מספיק נתונים ל-STL
          first_non_na <- min(valid_idx)
          last_non_na  <- max(valid_idx)
          segment <- x[first_non_na:last_non_na]
          
          y <- ts(segment, 
                  start = c(year(block_df$Date[first_non_na]), 
                            month(block_df$Date[first_non_na])), 
                  frequency = freq)
          
          stl_fit <- tryCatch({ stl(y, s.window = "periodic", robust = TRUE) }, error = function(e) NULL)
          
          if (!is.null(stl_fit)) {
            adj <- as.numeric(y - stl_fit$time.series[, "seasonal"])
            x_adj <- x
            x_adj[first_non_na:last_non_na] <- adj
            x <- x_adj
            sa_method <- "STL"
          } else {
            sa_method <- "Failed"
          }
        }
      }
    }
    
    # ---------------------------------------------------
    # 2. APPLY TRANSFORMATION CODE
    # ---------------------------------------------------
    
    result <- rep(NA, n)
    
    safe_log <- function(vec) {
      if (any(vec <= 0, na.rm = TRUE)) {
        message("Non-positive values found in ", varname, " - using log(x+1) fallback")
        return(log(vec + 1))
      }
      return(log(vec))
    }

    if (code == 0) {
      result <- x
    } else if (code == 1) {
      result <- safe_log(x)
    } else if (code == 2) {
      result[2:n] <- diff(x)
    } else if (code == 3) {
      result[2:n] <- diff(safe_log(x))
    } else if (code == 4) {
      result <- x # SA already handled seasonality
    } else if (code == 5) {
      result <- safe_log(x)
    } else if (code == 6) {
      result[2:n] <- diff(x)
    } else if (code == 7) {
      result[2:n] <- diff(safe_log(x))
    } else if (code == 8) {
      valid_idx <- which(!is.na(x))
      if(length(valid_idx) > 0) {
        first <- min(valid_idx); last <- max(valid_idx)
        t_seg <- seq_len(last - first + 1)
        result[first:last] <- residuals(lm(x[first:last] ~ t_seg))
      }
    }
    
    out_df[[varname]] <- result 

    info_list[[varname]] <- data.frame(
      variable = varname,
      code = code,
      sa_method = sa_method,
      transformation = c("none","log","diff","logdiff",
                         "sa_only","sa_log",
                         "sa_diff","sa_logdiff",
                         "detrend")[ifelse(is.na(code), 0, code) + 1],
      stringsAsFactors = FALSE
    )
  }
  
  return(list(
    data = out_df,
    info = dplyr::bind_rows(info_list)
  ))
}


## --------------------------------------------------------------------------------------
# ==============================================================================
# COMPARISON SCRIPT: Total Income Tax Division Net
# Methods: 1. Original (Double Diff) | 2. STL (Code 7) | 3. X-13 (seas)
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)

# --- Step 1: Extract and prepare the raw data ---
# Extract the specific variable from the tax block and filter out NAs
raw_data <- blocks_real$personal_labor_income_taxes %>%
  select(Date, `Total Income Tax Division Net`) %>%
  filter(!is.na(`Total Income Tax Division Net`))

# Define the start date dynamically based on the first available observation
start_year <- as.numeric(format(min(raw_data$Date), "%Y"))
start_month <- as.numeric(format(min(raw_data$Date), "%m"))

# Convert to a monthly time series (ts) object
y_ts <- ts(raw_data$`Total Income Tax Division Net`, 
           start = c(start_year, start_month), 
           frequency = 12)

# --- Method 1: The Original Approach (Double Differentiation) ---
# Transformation path: Log -> Seasonal Difference (lag=12) -> First Difference (lag=1)
# This was the implicit result of "logdiff+seasonal_diff"
method_orig <- diff(diff(log(y_ts), 12), 1)


# --- Method 2: The STL Approach (Code 7) ---
# Transformation path: Log -> STL Decomposition -> Seasonally Adjusted -> First Diff
ly <- log(y_ts)
# Fit STL model (s.window="periodic" assumes stable seasonality, robust=TRUE handles outliers)
stl_fit <- stl(ly, s.window = "periodic", robust = TRUE)
# Extract the seasonally adjusted series
sa_stl <- seasadj(stl_fit)
# Apply first difference to achieve stationarity
method_stl <- diff(sa_stl, 1)


# --- Method 3: The X-13 Approach (seasonal_adjust_block + Diff) ---
# Pass the data through your existing X-13 function (which accounts for holidays)
# We pass the entire block but specify only our target column
out_x13 <- seasonal_adjust_block(
  block = blocks_real$personal_labor_income_taxes,
  columns = c("Total Income Tax Division Net"),
  hag_ts = hag_ts,  # Israeli holidays regressor
  td_ts = td_ts     # Trading days regressor
)

# Extract the seasonally adjusted (SA) values returned by X-13
sa_x13_values <- out_x13$data$`Total Income Tax Division Net`
sa_x13_values <- sa_x13_values[!is.na(sa_x13_values)] # Clean potential NAs

# Convert the SA output to a time series object
sa_x13_ts <- ts(sa_x13_values, 
                start = c(start_year, start_month), 
                frequency = 12)

# Transformation path: Log -> First Difference (on the X-13 SA series)
# Note: "logdiff" means we need to log the SA data before differencing
method_x13 <- diff(log(sa_x13_ts), 1)


# --- Step 2: Combine Results and Visualize ---
# Since differencing removes observations (13 for Orig, 1 for STL/X-13), 
# we align them to the shortest series (method_orig)
n_orig <- length(method_orig)

# Create a clean data frame for ggplot
comparison_df <- data.frame(
  # Get the matching dates for the end of the series
  Date = tail(raw_data$Date, n_orig),
  Original_DoubleDiff = as.numeric(tail(method_orig, n_orig)),
  STL_Code7 = as.numeric(tail(method_stl, n_orig)),
  X13_Adjusted = as.numeric(tail(method_x13, n_orig))
)

# Reshape data from wide to long format for faceting in ggplot
comparison_long <- comparison_df %>%
  pivot_longer(cols = -Date, names_to = "Method", values_to = "Value")

# Plot the comparison
ggplot(comparison_long, aes(x = Date, y = Value, color = Method)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  facet_wrap(~Method, ncol = 1, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "Transformation Comparison: Total Income Tax Division Net",
       subtitle = "Evaluating Stationarity: Original vs. STL (Code 7) vs. X-13",
       x = "Date", y = "Transformed Shocks") +
  scale_color_manual(values = c("Original_DoubleDiff" = "darkred", 
                                "STL_Code7" = "blue", 
                                "X13_Adjusted" = "darkgreen")) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, face = "bold"))


# ==============================================================================
# --- Step 3: Statistical Evaluation Metrics ---
# ==============================================================================
library(tseries)

# Function to calculate performance metrics for a given time series
evaluate_method <- function(method_name, ts_data) {
  
  # Remove NAs for statistical testing
  x <- na.omit(as.numeric(ts_data))
  
  # 1. Stationarity Tests
  # ADF Test (H0: Non-stationary. We want p-value < 0.05 to reject H0)
  adf_p <- suppressWarnings(tryCatch(adf.test(x)$p.value, error = function(e) NA))
  
  # KPSS Test (H0: Stationary. We want p-value > 0.05 to NOT reject H0)
  kpss_p <- suppressWarnings(tryCatch(kpss.test(x)$p.value, error = function(e) NA))
  
  # 2. Volatility (Standard Deviation)
  # Lower volatility generally means less extreme shocks/noise
  volatility <- sd(x)
  
  # 3. Residual Seasonality (Autocorrelation at Lag 12)
  # Checks if there's still a relationship every 12 months. Closer to 0 is better.
  # Note: in acf(), index 1 is lag 0. So index 13 is lag 12.
  acf_12 <- acf(x, lag.max = 12, plot = FALSE)$acf[13]
  
  # Return as a data frame row
  data.frame(
    Method = method_name,
    ADF_p_value = round(adf_p, 3),         # Target: < 0.05
    KPSS_p_value = round(kpss_p, 3),       # Target: > 0.05
    Volatility_SD = round(volatility, 3),  # Target: Lower is better
    Abs_ACF_Lag12 = round(abs(acf_12), 3)  # Target: Closer to 0 is better
  )
}

# Apply the function to all three methods
eval_orig <- evaluate_method("1_Original_DoubleDiff", method_orig)
eval_stl  <- evaluate_method("2_STL_Code7", method_stl)
eval_x13  <- evaluate_method("3_X13_Adjusted", method_x13)

# Combine into one clear summary table
evaluation_summary <- rbind(eval_orig, eval_stl, eval_x13)

# Print the results to the console
print("--- Evaluation Summary ---")
print(evaluation_summary)


## --------------------------------------------------------------------------------------
# ==============================================================================
# COMPARISON SCRIPT: Independents advances
# Methods: 
# 1. Code 8 Original (Log -> Linear Detrend ONLY)
# 2. Code 8 + STL (Log -> Linear Detrend -> STL Seasonal Adjustment)
# 3. X13 + Code 8 (X13 Seasonal Adjustment -> Log -> Linear Detrend)
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(tseries)

# --- Step 1: Extract and prepare the raw data ---
raw_data <- blocks_real$personal_labor_income_taxes %>%
  select(Date, `Independents advances`) %>%
  filter(!is.na(`Independents advances`))

start_year <- as.numeric(format(min(raw_data$Date), "%Y"))
start_month <- as.numeric(format(min(raw_data$Date), "%m"))

y_ts <- ts(raw_data$`Independents advances`, 
           start = c(start_year, start_month), 
           frequency = 12)

# ==============================================================================
# --- Method 1: Original Code 8 (Linear Detrend ONLY) ---
# ==============================================================================
ly <- log(y_ts)
t_seg <- seq_len(length(ly))

# Fit linear model and extract residuals (detrending)
linear_model_orig <- lm(ly ~ t_seg)
method_orig <- ts(residuals(linear_model_orig), frequency = 12, start = start(y_ts))

# ==============================================================================
# --- Method 2: Code 8 + STL (Detrend -> STL) ---
# ==============================================================================
# We take the detrended series from Method 1 and remove seasonality using STL
if (length(method_orig) >= 24) {
  stl_fit <- stl(method_orig, s.window = "periodic", robust = TRUE)
  method_stl <- seasadj(stl_fit) # Extract the seasonally adjusted part
} else {
  method_stl <- method_orig # Fallback if data is too short
}

# ==============================================================================
# --- Method 3: X-13 + Code 8 (X13 -> Detrend) ---
# ==============================================================================
# Step A: Apply X-13 to the raw block data (handles holidays)
out_x13 <- seasonal_adjust_block(
  block = blocks_real$personal_labor_income_taxes,
  columns = c("Independents advances"),
  hag_ts = hag_ts,
  td_ts = td_ts
)

sa_x13_values <- out_x13$data$`Independents advances`
sa_x13_values <- sa_x13_values[!is.na(sa_x13_values)]

sa_x13_ts <- ts(sa_x13_values, start = c(start_year, start_month), frequency = 12)

# Step B: Log and apply linear detrending (Code 8 logic)
ly_x13 <- log(sa_x13_ts)
t_seg_x13 <- seq_len(length(ly_x13))

linear_model_x13 <- lm(ly_x13 ~ t_seg_x13)
method_x13 <- ts(residuals(linear_model_x13), frequency = 12, start = start(y_ts))


# ==============================================================================
# --- Step 2: Combine Results and Visualize ---
# ==============================================================================
# Since there is NO DIFF, all series have the exact same length!
comparison_df <- data.frame(
  Date = raw_data$Date,
  Original_Code8_DetrendOnly = as.numeric(method_orig),
  Code8_plus_STL = as.numeric(method_stl),
  X13_plus_Code8 = as.numeric(method_x13)
)

comparison_long <- comparison_df %>%
  pivot_longer(cols = -Date, names_to = "Method", values_to = "Value")

plot_advances <- ggplot(comparison_long, aes(x = Date, y = Value, color = Method)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  facet_wrap(~Method, ncol = 1, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "Comparison: Independents Advances",
       subtitle = "Original Code 8 vs. Code 8+STL vs. X13+Code 8",
       x = "Date", y = "Transformed Residuals") +
  scale_color_manual(values = c("Original_Code8_DetrendOnly" = "darkred", 
                                "Code8_plus_STL" = "blue", 
                                "X13_plus_Code8" = "darkgreen")) +
  theme(legend.position = "none", strip.text = element_text(size = 12, face = "bold"))

print(plot_advances)


# ==============================================================================
# --- Step 3: Statistical Evaluation Metrics ---
# ==============================================================================
evaluate_method <- function(method_name, ts_data) {
  x <- na.omit(as.numeric(ts_data))
  
  adf_p <- suppressWarnings(tryCatch(adf.test(x)$p.value, error = function(e) NA))
  kpss_p <- suppressWarnings(tryCatch(kpss.test(x)$p.value, error = function(e) NA))
  volatility <- sd(x)
  acf_12 <- acf(x, lag.max = 12, plot = FALSE)$acf[13]
  
  data.frame(
    Method = method_name,
    ADF_p_value = round(adf_p, 3),
    KPSS_p_value = round(kpss_p, 3),
    Volatility_SD = round(volatility, 3),
    Abs_ACF_Lag12 = round(abs(acf_12), 3) # The most crucial metric here!
  )
}

eval_orig <- evaluate_method("1_Original_Code8", method_orig)
eval_stl  <- evaluate_method("2_Code8_plus_STL", method_stl)
eval_x13  <- evaluate_method("3_X13_plus_Code8", method_x13)

evaluation_summary <- rbind(eval_orig, eval_stl, eval_x13)

print("--- Evaluation Summary: Independents Advances (NO DIFF) ---")
print(evaluation_summary)


## --------------------------------------------------------------------------------------
# ==============================================================================
# COMPARISON SCRIPT: Independents advances
# Methods: 1. Strict Original Code 8 (Detrend ONLY) | 2. STL | 3. X-13
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(tseries)

# --- Step 1: Extract and prepare the raw data ---
raw_data <- blocks_real$personal_labor_income_taxes %>%
  select(Date, `Independents advances`) %>%
  filter(!is.na(`Independents advances`))

start_year <- as.numeric(format(min(raw_data$Date), "%Y"))
start_month <- as.numeric(format(min(raw_data$Date), "%m"))

# Convert to monthly time series
y_ts <- ts(raw_data$`Independents advances`, 
           start = c(start_year, start_month), 
           frequency = 12)

# Use Log transformation to stabilize variance (standard for tax data)
ly <- log(y_ts)


# --- Method 1: Strict Original Code 8 (Linear Detrend ONLY) ---
# As per the provided legacy code, this strictly fits a linear model
# over time and extracts the residuals. Seasonality is left untouched!
time_index <- seq_len(length(ly))
linear_model <- lm(ly ~ time_index)
method_orig <- ts(residuals(linear_model), frequency = 12)


# --- Method 2: The STL Approach (Proposed Code 7/9) ---
# Flexibly removes seasonality and trend, then applies a regular first difference.
stl_fit <- stl(ly, s.window = "periodic", robust = TRUE)
sa_stl <- seasadj(stl_fit)
method_stl <- diff(sa_stl, lag = 1)


# --- Method 3: The X-13 Approach ---
# Uses your specific holiday-aware seasonal adjustment function, 
# followed by a regular first difference to make it stationary.
out_x13 <- seasonal_adjust_block(
  block = blocks_real$personal_labor_income_taxes,
  columns = c("Independents advances"),
  hag_ts = hag_ts,
  td_ts = td_ts
)

sa_x13_values <- out_x13$data$`Independents advances`
sa_x13_values <- sa_x13_values[!is.na(sa_x13_values)]

sa_x13_ts <- ts(sa_x13_values, 
                start = c(start_year, start_month), 
                frequency = 12)

method_x13 <- diff(log(sa_x13_ts), lag = 1)


# --- Step 2: Combine Results and Visualize ---
# Align all series to the shortest one (Method 2/3 lost 1 obs due to diff)
n_orig <- length(method_stl)

comparison_df <- data.frame(
  Date = tail(raw_data$Date, n_orig),
  Original_Code8_OnlyDetrend = as.numeric(tail(method_orig, n_orig)),
  STL_Approach = as.numeric(tail(method_stl, n_orig)),
  X13_Approach = as.numeric(tail(method_x13, n_orig))
)

comparison_long <- comparison_df %>%
  pivot_longer(cols = -Date, names_to = "Method", values_to = "Value")

plot_advances <- ggplot(comparison_long, aes(x = Date, y = Value, color = Method)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  facet_wrap(~Method, ncol = 1, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "Comparison: Independents Advances",
       subtitle = "Original Code 8 (Detrend Only) vs. STL vs. X-13",
       x = "Date", y = "Transformed Value") +
  scale_color_manual(values = c("Original_Code8_OnlyDetrend" = "darkred", 
                                "STL_Approach" = "blue", 
                                "X13_Approach" = "darkgreen")) +
  theme(legend.position = "none", strip.text = element_text(size = 12, face = "bold"))

print(plot_advances)


# --- Step 3: Statistical Evaluation Metrics ---
evaluate_method <- function(method_name, ts_data) {
  x <- na.omit(as.numeric(ts_data))
  
  adf_p <- suppressWarnings(tryCatch(adf.test(x)$p.value, error = function(e) NA))
  kpss_p <- suppressWarnings(tryCatch(kpss.test(x)$p.value, error = function(e) NA))
  volatility <- sd(x)
  acf_12 <- acf(x, lag.max = 12, plot = FALSE)$acf[13]
  
  data.frame(
    Method = method_name,
    ADF_p_value = round(adf_p, 3),         # Target: < 0.05
    KPSS_p_value = round(kpss_p, 3),       # Target: > 0.05
    Volatility_SD = round(volatility, 3),  # Target: Lowest possible
    Abs_ACF_Lag12 = round(abs(acf_12), 3)  # Target: Closest to 0
  )
}

eval_orig <- evaluate_method("1_Original_Code8", method_orig)
eval_stl  <- evaluate_method("2_STL_Approach", method_stl)
eval_x13  <- evaluate_method("3_X13_Approach", method_x13)

evaluation_summary <- rbind(eval_orig, eval_stl, eval_x13)

print("--- Evaluation Summary: Independents Advances ---")
print(evaluation_summary)


## --------------------------------------------------------------------------------------
recommendation_to_codes <- function(recommendations) {
  codes <- dplyr::case_when(
    recommendations == "none" ~ 0L,
    recommendations == "log" ~ 1L,
    recommendations == "diff" ~ 2L,
    recommendations == "logdiff" ~ 3L,
    recommendations == "seasonal_diff" ~ 4L,
    recommendations == "log+seasonal_diff" ~ 5L,
    recommendations == "diff+seasonal_diff" ~ 6L,
    recommendations == "logdiff+seasonal_diff" ~ 7L,
    recommendations == "detrend" ~ 8L,
    
# MAPPING UPDATE: Replaced Linear Detrend (8) with X13 Seasonal Adjustment + Log + Diff (7).    
    recommendations == "detrend+seasonal_diff" ~ 7L, 
    
    recommendations == "too_short" ~ 0L,
    TRUE ~ NA_integer_
  )
  
  names(codes) <- names(recommendations)
  
  unsupported <- unique(recommendations[is.na(codes)])
  if (length(unsupported)) {
    stop("Unsupported transformation recommendation(s): ", 
         paste(unsupported, collapse = ", "))
  }
  
  # עדכון האזהרה בהתאם לשינוי
  if (any(recommendations == "detrend+seasonal_diff", na.rm = TRUE)) {
    message("'detrend+seasonal_diff' is now mapped to X13 + Log + Diff (Code 7).")
  }
  
  codes
}


## --------------------------------------------------------------------------------------
# Optional manual overrides. Use named vectors by block and variable.
# Example:
# manual_transformation_overrides <- list(
#   FX_liqudity = c("Foreign exchange reserves (millions of dollars)" = 3)
# )
manual_transformation_overrides <- list()

apply_code_overrides <- function(block_name, codes) {
  overrides <- manual_transformation_overrides[[block_name]]
  if (is.null(overrides)) return(codes)
  
  unknown_vars <- setdiff(names(overrides), names(codes))
  if (length(unknown_vars)) {
    stop("Manual override variable(s) not found in block '", block_name, "': ",
         paste(unknown_vars, collapse = ", "))
  }
  
  codes[names(overrides)] <- unname(overrides)
  codes
}


## --------------------------------------------------------------------------------------

blocks_transformed <- blocks_sa
transformation_info <- list()
applied_transformation_codes <- list()

for (block_name in names(transformation_recommendations)) {
  recommendations <- transformation_recommendations[[block_name]]
  codes <- recommendation_to_codes(recommendations)
  codes <- apply_code_overrides(block_name, codes)
  
  # Execute seasonal adjustment and mathematical transformations
  out <- transform_block(
    block_df = blocks_sa[[block_name]], 
    codes_vector = codes, 
    hag_ts = hag_ts, 
    td_ts = td_ts
  )
  
  # Store metadata for the summary report
  out$info$recommended <- unname(recommendations[out$info$variable])
  out$info <- out$info[, c("variable", "recommended", "code", "sa_method", "transformation")]
  
  # Update the main list
  blocks_transformed[[block_name]] <- out$data
  transformation_info[[block_name]] <- out$info
  applied_transformation_codes[[block_name]] <- codes
}

if ("target" %in% names(blocks_raw)) {
  message("Updating Target: Transforming GDP levels to log-differences...")
  
  # Extract raw GDP data (large numbers)
  target_df <- as.data.frame(blocks_raw$target)
  target_df$Date <- as.Date(target_df$Date)
  target_df <- target_df %>% dplyr::arrange(Date)
  
  # Validation: Ensure GDP is positive before applying log
  if (any(target_df$GDP <= 0, na.rm = TRUE)) {
    stop("GDP must be positive for log-transformation.")
  }
  
  # Critical Calculation: Transform levels into Log-Differences (approx. growth rate)
  # This converts ~142,000 into ~0.02
  target_df$GDP <- c(NA_real_, diff(log(target_df$GDP)))
  
  # Update GDP in the final blocks_transformed list
  blocks_transformed$target <- target_df
  
  # Add GDP metadata to the info list manually
  transformation_info$target <- data.frame(
    variable = "GDP",
    recommended = "target",
    code = 3L,
    sa_method = "None",
    transformation = "logdiff_qoq",
    stringsAsFactors = FALSE
  )
}


# Combine all metadata into a single summary table
transformation_summary <- dplyr::bind_rows(
  lapply(names(transformation_info), function(block_name) {
    dplyr::mutate(transformation_info[[block_name]], block = block_name, .before = variable)
  })
)

# Transfer data to the Shifted object for the next steps in the pipeline
blocks_shifted <- blocks_transformed
shift_report <- list()

# Final check in Console
#print("Preview of transformed GDP (values should be around 0.02):")
#print(head(blocks_transformed$target$GDP))

# View the metadata summary
#View(transformation_summary)


## --------------------------------------------------------------------------------------



## --------------------------------------------------------------------------------------
table(transformation_summary$sa_method)
transformation_summary %>% 
  filter(sa_method == "None") %>% 
  group_by(code) %>% 
  summarise(count = n())


## --------------------------------------------------------------------------------------
save_transformation_info <- function(info_list, file_path = "transformation_info.xlsx") {
  # info_list: a named list where each element is a data.frame of metadata
  # file_path: output Excel file
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required but not installed.")
  }
  
  # Create workbook
  wb <- openxlsx::createWorkbook()
  
  # Loop through list elements
  for (block_name in names(info_list)) {
    
    # Add worksheet with the block name
    openxlsx::addWorksheet(wb, block_name)
    
    # Write the info table
    openxlsx::writeData(wb, 
                        sheet = block_name, 
                        x = info_list[[block_name]])
  }
  
  # Save file
  openxlsx::saveWorkbook(wb, file = file_path, overwrite = TRUE)
  
  message("Saved: ", file_path)
}



## --------------------------------------------------------------------------------------
recommendation_to_codes("too_short")


## --------------------------------------------------------------------------------------
save_transformation_info(transformation_info, "TRANSFORMATION_INFO.xlsx")



## --------------------------------------------------------------------------------------
save_list_to_excel(blocks_transformed, "blocks_transformed.xlsx")


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax$`Goods and services`)



## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax$`excess expenses`)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$real_estate$`Real estate purchase tax`)


## --------------------------------------------------------------------------------------
# 1. הגדרת המשתנה לבדיקה
var_name <- "Independents advances" # או כל שם משתנה אחר שמופיע בבלוק
block <- "personal_labor_income_taxes"

# 2. שליפת הנתונים
raw_data <- blocks_sa[[block]][[var_name]]
trans_data <- blocks_transformed[[block]][[var_name]]

# 3. מציאת גבולות גזרה שמתעלמים מ-5% הקיצוניים (כדי לראות משהו)
ylim_raw <- quantile(raw_data, probs = c(0.05, 0.95), na.rm = TRUE)
ylim_trans <- quantile(trans_data, probs = c(0.05, 0.95), na.rm = TRUE)

# 4. יצירת גרף ההשוואה
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1)) # שני גרפים אחד מעל השני

plot.ts(raw_data, ylim = ylim_raw, col = "black",
        main = paste("Original:", var_name, "(Zoomed 5-95%)"),
        ylab = "Levels")

plot.ts(trans_data, ylim = ylim_trans, col = "blue",
        main = paste("Transformed:", var_name, "(Zoomed 5-95%)"),
        ylab = "Transformed")

test_sa <- seasonal_adjust_block(blocks_sa$personal_labor_income_taxes[, c("Date", "Independents advances")])
print(test_sa)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$FX_liqudity$`Foreign exchange reserves (millions of dollars)`)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$personal_labor_income_taxes$`Total refunds from the Income Tax Department`)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$personal_labor_income_taxes$`Capital Gains Tax Refunds`)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$personal_labor_income_taxes$`Non-profit institution tax`)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax$`Cancellation companies`)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax$`tax differential companies`)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax$`Cancellations Deductions`)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax$`Bonds and dividends`)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax$`Companies returns`)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$import_trade_tax$`Total import taxes`)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$real_estate$`praise tax returns`)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$real_estate$`Apartments sold at an annual rate`)


## --------------------------------------------------------------------------------------
plot.ts(blocks_transformed$FX_liqudity)


## --------------------------------------------------------------------------------------
shift_by_vector <- function(df, vec) {
  # df: data.frame where the first column is 'Date'
  # vec: numeric vector of lags, matching columns 2..n of df
  
  # Sanity check
  if (length(vec) != (ncol(df) - 1)) {
    stop("Length of vec must match number of data columns (excluding Date).")
  }
  
  df_shifted <- df
  
  # Prepare the report
  report <- data.frame(
    variable = colnames(df)[-1],
    lag_applied = vec,
    stringsAsFactors = FALSE
  )
  
  # Apply shifts
  for (i in seq_along(vec)) {
    k <- vec[i]
    var <- colnames(df)[i + 1]   # +1 to skip Date
    
    if (!is.numeric(df[[var]])) {
      warning(paste("Column", var, "is not numeric — skipped."))
      next
    }
    
    if (k > 0) {
      # Shift forward: NA at the top, drop the last k values
      df_shifted[[var]] <- c(rep(NA, k), df[[var]][1:(nrow(df) - k)])
    } else {
      # k == 0: no change
      df_shifted[[var]] <- df[[var]]
    }
  }
  
  return(list(
    data = df_shifted,
    report = report
  ))
}


## --------------------------------------------------------------------------------------
blocks_shifted <- blocks_transformed
shift_report <- list()
#View(blocks_transformed$FX_liqudity)


## --------------------------------------------------------------------------------------
head(blocks_transformed$target$GDP)


## --------------------------------------------------------------------------------------
block <- blocks_transformed$FX_liqudity # choose your block
lags <- c(1,0,1) # rep(val, n)


## --------------------------------------------------------------------------------------
out <- shift_by_vector(block, lags)

blocks_shifted$FX_liqudity <- out$data
shift_report$FX_liqudity <- out$report


## --------------------------------------------------------------------------------------
# 1. Take the GDP from the raw data
target_df <- as.data.frame(blocks_raw$target)

# 2. Transform it (The Fix)
target_df$GDP <- c(NA_real_, diff(log(target_df$GDP)))

# 3. Put it into the object that is about to be saved
blocks_shifted$target <- target_df

# 4. NOW save it
blocks_shifted_path <- "C:/Users/noran/OneDrive/מסמכים/סמינר מעשי/PROJ_DFM/PROJ_DFM/blocks_shifted.xlsx"
save_list_to_excel(blocks_shifted, blocks_shifted_path)


## --------------------------------------------------------------------------------------
save_transformation_info(shift_report, "shift_report.xlsx")


## --------------------------------------------------------------------------------------
# Input: multi-sheet transformed + lag-adjusted workbook
if (!exists("blocks_shifted_path")) {
  blocks_shifted_path <- "C:/Users/eladb/OneDrive/Desktop/University/Bachelor's/2025 B/Nowcasting/Seminar/data/lag_adjusted/blocks_shifted.xlsx"
}

input_path <- blocks_shifted_path

# Output: one-sheet panel for DFM
output_path <- "data/clean/combined_monthly_panel_Q_refined.xlsx"

# Read sheet names, excluding adjusters
sheets <- readxl::excel_sheets(input_path)
sheets_to_join <- sheets[sheets != "adjusters"]

if (!"target" %in% sheets_to_join) {
  stop("The transformed workbook must include a 'target' sheet.")
}

# Read each sheet, preserving workbook sheet order and column order
blocks <- sheets_to_join |>
  setNames(sheets_to_join) |>
  purrr::map(function(sheet_name) {
    df <- readxl::read_excel(input_path, sheet = sheet_name)

    if (!"Date" %in% names(df)) {
      stop("Sheet '", sheet_name, "' does not contain a Date column.")
    }

    df$Date <- as.Date(df$Date)
    
# Target dates are end-of-month; move them to first day of next month
    if (sheet_name == "target") {
      df$Date <- df$Date + 1
    }

    # Keep Date first, then original column order
    df <- df[, c("Date", setdiff(names(df), "Date"))]

    df
  })

if (!"GDP" %in% names(blocks$target)) {
  stop("The 'target' sheet must contain a GDP column after transformation.")
}

# Optional safety check: no duplicated non-Date columns across sheets
all_variable_names <- unlist(lapply(blocks, function(df) setdiff(names(df), "Date")))
duplicated_vars <- unique(all_variable_names[duplicated(all_variable_names)])

if (length(duplicated_vars) > 0) {
  stop(
    "Duplicate variable names found across sheets: ",
    paste(duplicated_vars, collapse = ", "),
    ". Rename them before joining, or add prefixes intentionally."
  )
}

# Join all sheets by Date
combined_monthly_panel_Q_refined <- purrr::reduce(
  blocks,
  dplyr::full_join,
  by = "Date"
) |>
  dplyr::arrange(Date)

# Drop leading rows until at least 2 variables have observed data
non_date_cols <- setdiff(names(combined_monthly_panel_Q_refined), "Date")

row_non_missing_count <- rowSums(
  !is.na(combined_monthly_panel_Q_refined[, non_date_cols, drop = FALSE])
)

first_valid_row <- which(row_non_missing_count >= 2)[1]

if (is.na(first_valid_row)) {
  stop("No row found with at least 2 non-missing variables.")
}

combined_monthly_panel_Q_refined <- combined_monthly_panel_Q_refined[
  first_valid_row:nrow(combined_monthly_panel_Q_refined),
]

if (!"GDP" %in% names(combined_monthly_panel_Q_refined)) {
  stop("GDP was not carried into the unified modeling table.")
}

gdp_validation <- combined_monthly_panel_Q_refined %>%
  dplyr::select(Date, GDP) %>%
  dplyr::inner_join(
    blocks$target %>%
      dplyr::select(Date, GDP_from_target = GDP),
    by = "Date"
  )

if (nrow(gdp_validation) == 0 || all(is.na(gdp_validation$GDP))) {
  stop("No non-missing transformed GDP values were carried into the unified table.")
}

gdp_mismatch <- gdp_validation %>%
  dplyr::filter(
    xor(is.na(GDP), is.na(GDP_from_target)) |
      (!is.na(GDP) &
         !is.na(GDP_from_target) &
         abs(GDP - GDP_from_target) > sqrt(.Machine$double.eps))
  )

if (nrow(gdp_mismatch) > 0) {
  stop("GDP in the unified table does not match the transformed target sheet.")
}

# Save as one-sheet Excel file
openxlsx::write.xlsx(
  combined_monthly_panel_Q_refined,
  file = output_path,
  sheetName = "combined_monthly_panel",
  overwrite = TRUE
)

message("Saved: ", output_path)


