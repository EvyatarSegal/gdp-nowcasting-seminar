## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# path <- "C:/Users/eladb/OneDrive/Desktop/University/Bachelor's/2025 B/Nowcasting/Seminar/data/raw/nowcasting_data_raw.xlsx"

# Hardcoding a path is a bad habit, lets use relative path
setwd("..")
path <- paste0(getwd(), "/data/raw/nowcasting_data_raw.xlsx", sep = "")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sheets <- excel_sheets(path)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
blocks_raw <- sheets %>% 
  set_names() %>% 
  map(~ read_excel(path, sheet = .x))

blocks_real <- sheets %>% 
  set_names() %>% 
  map(~ read_excel(path, sheet = .x))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
blocks_real$personal_labor_income_taxes <-
  adjust_block_for_cpi(blocks_raw$personal_labor_income_taxes,
                       blocks_raw$adjusters)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
blocks_real$corporate_business_tax <-
  adjust_block_for_cpi(blocks_raw$corporate_business_tax,
                       blocks_raw$adjusters)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
blocks_real$consumption_tax <-
  adjust_block_for_cpi(blocks_raw$consumption_tax,
                       blocks_raw$adjusters)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
blocks_real$import_trade_tax <-
  adjust_block_for_cpi(blocks_raw$import_trade_tax,
                       blocks_raw$adjusters)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
blocks_real$real_estate <-
  adjust_block_for_cpi(blocks_raw$real_estate,
                       blocks_raw$adjusters,
                       cols = c("Real estate taxation",
                                "Property tax",
                                "praise tax",
                                "Real estate purchase tax",
                                "praise tax returns",
                                "purchase returns"))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
blocks_real$real_activity$Oil <- blocks_raw$real_activity$Oil * blocks_raw$FX_liqudity$Dollar


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
blocks_real$real_activity <-
  adjust_block_for_cpi(blocks_real$real_activity,
                       blocks_real$adjusters,
                       cols = "Oil")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Its no longer Dollars, its shekels Now
blocks_real$FX_liqudity$`Foreign exchange reserves (millions of shekels)` <- blocks_raw$FX_liqudity$`Foreign exchange reserves (millions of dollars)` * blocks_raw$FX_liqudity$Dollar


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
blocks_real$FX_liqudity <-
  adjust_block_for_cpi(blocks_real$FX_liqudity,
                       blocks_real$adjusters,
                       cols = "Foreign exchange reserves (millions of shekels)")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(blocks_real$FX_liqudity$`Foreign exchange reserves (millions of shekels)`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(blocks_real$FX_liqudity$`Foreign exchange reserves (millions of shekels)`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Note from Evyatar: Again, lets use relative paths
setwd("..")
save_list_to_excel(blocks_real, paste0(getwd(), "/data/intermediate/blocks_real.xlsx"))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd("..")
td <- read_csv("data/raw/td_var.csv")
td_ts <- ts(
  td[, -1],
  start = c(year(min(td$date)), month(min(td$date))),
  frequency = 12
)

preadj <- read_excel("data/raw/hol_preadj.xlsx") %>%
  mutate(date = as.Date(date))

hag_ts <- ts(
  preadj[, -1],
  start = c(year(min(preadj$date)), month(min(preadj$date))),
  frequency = 12
)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
blocks_sa <- blocks_real


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sa_info <- list()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
write_sa_info <- function(sa_info, file_path = "sa_info.xlsx") {
  wb <- createWorkbook()
  
  for (bn in names(sa_info)) {
    addWorksheet(wb, bn)
    writeData(wb, bn, sa_info[[bn]])
  }
  
  saveWorkbook(wb, file = file_path, overwrite = TRUE)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd("..")
write_sa_info(sa_info, "data/intermediate/SA_results_01.xlsx")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd("..")
save_list_to_excel(blocks_sa, "data/intermediate/blocks_sa_01.xlsx")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
  # Check "dfgls" - more modern than adf
  # Base decision
  if (adf_stationary && kpss_stationary) {
    base <- "none"
  } else if (!adf_stationary && !kpss_stationary) {
    base <- "diff"
  } else if (adf_stationary && !kpss_stationary) {
    base <- "detrend"
  } else if (!adf_stationary && kpss_stationary) {
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


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# These are the model input blocks. Keep adjusters and target unchanged.
blocks_to_transform <- setdiff(names(blocks_sa), c("adjusters", "target"))

transformation_recommendations <- blocks_sa[blocks_to_transform] |>
  purrr::map(function(block) {
    sapply(block %>% dplyr::select(-Date), test_transform)
  })

transformation_recommendations


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transform_block <- function(block_df, codes_vector, freq = 12) {
  
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
    
    # always initialize full-length result
    result <- rep(NA, n)
    
    transformed <- tryCatch({
      
      if (code == 0) {
        result <- x
        result
        
      } else if (code == 1) {
        result <- log(x)
        result
        
      } else if (code == 2) {
        result[2:n] <- diff(x)
        result
        
      } else if (code == 3) {
        lx <- log(x)
        result[2:n] <- diff(lx)
        result
        
      } else if (code == 4) {
        if (n > freq) result[(freq+1):n] <- x[(freq+1):n] - x[1:(n-freq)]
        result
        
      } else if (code == 5) {
        if (n > freq) {
          sd <- x[(freq+1):n] - x[1:(n-freq)]
          result[(freq+1):n] <- log(sd)
        }
        result
        
      } else if (code == 6) {
        if (n > freq+1) {
          sd <- x[(freq+1):n] - x[1:(n-freq)]
          result[(freq+2):n] <- diff(sd)
        }
        result
        
      } else if (code == 7) {
        if (n > freq+1) {
          lx <- log(x)
          ld <- diff(lx)
          result[(freq+2):n] <- ld[(freq+1):(n-1)] - ld[1:(n-freq-1)]
        }
        result
        
      } else if (code == 8) {
        # Identify non-NA segment
        first_non_na <- min(which(!is.na(x)))
        last_non_na  <- max(which(!is.na(x)))
        
        x_seg <- x[first_non_na:last_non_na]
        t_seg <- seq_len(length(x_seg))
        
        # Detrend only this segment
        detr <- residuals(lm(x_seg ~ t_seg))
        
        # Place back into full result vector
        result <- rep(NA, n)
        result[first_non_na:last_non_na] <- detr
        
        result
        
      } else {
        stop("Unknown code.")
      }
      
    }, error = function(e) {
      warning(paste("Failed", varname, ":", e$message))
      rep(NA, n)
    })
    
    # FORCE LENGTH TO MATCH
    if (length(transformed) != n) {
      tmp <- rep(NA, n)
      tmp[1:length(transformed)] <- transformed
      transformed <- tmp
    }
    
    # assign safely
    out_df[[varname]] <- transformed
    
    info_list[[varname]] <- data.frame(
      variable = varname,
      code = code,
      transformation = c(
        "none","log","diff","logdiff",
        "seasonal_diff","log_seasonal_diff",
        "diff_seasonal_diff","logdiff_seasonal_diff",
        "detrend"
      )[code + 1],
      stringsAsFactors = FALSE
    )
  }
  
  return(list(
    data = out_df,
    info = dplyr::bind_rows(info_list)
  ))
}




## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
    recommendations == "detrend+seasonal_diff" ~ 8L,
    recommendations == "too_short" ~ 0L,
    TRUE ~ NA_integer_
  )
  
  names(codes) <- names(recommendations)
  
  unsupported <- unique(recommendations[is.na(codes)])
  if (length(unsupported)) {
    stop("Unsupported transformation recommendation(s): ",
         paste(unsupported, collapse = ", "))
  }
  
  if (any(recommendations == "detrend+seasonal_diff", na.rm = TRUE)) {
    warning("'detrend+seasonal_diff' is mapped to 'detrend'. Review manually if needed.")
  }
  
  codes
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Optional manual overrides. Use named vectors by block and variable.
# Example:
# manual_transformation_overrides <- list(
#   FX_liqudity = c("Foreign exchange reserves (millions of shekels)" = 3)
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


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
blocks_transformed <- blocks_sa
transformation_info <- list()
applied_transformation_codes <- list()

for (block_name in names(transformation_recommendations)) {
  recommendations <- transformation_recommendations[[block_name]]
  codes <- recommendation_to_codes(recommendations)
  codes <- apply_code_overrides(block_name, codes)
  
  out <- transform_block(blocks_sa[[block_name]], codes)
  
  out$info$recommended <- unname(recommendations[out$info$variable])
  out$info <- out$info[, c("variable", "recommended", "code", "transformation")]
  
  blocks_transformed[[block_name]] <- out$data
  transformation_info[[block_name]] <- out$info
  applied_transformation_codes[[block_name]] <- codes
}

# Keep the target from the raw input and transform quarterly GDP to QoQ log-diff.
if (!"target" %in% names(blocks_raw)) {
  stop("Raw input must include a 'target' sheet.")
}

target_df <- as.data.frame(blocks_raw$target)

if (!"Date" %in% names(target_df)) {
  stop("The 'target' sheet must contain a Date column.")
}

if (!"GDP" %in% names(target_df)) {
  stop("The 'target' sheet must contain a GDP column.")
}

target_df$Date <- as.Date(target_df$Date)
target_df <- target_df %>% dplyr::arrange(Date)

if (any(target_df$GDP <= 0, na.rm = TRUE)) {
  stop("GDP must be positive to calculate log-diff.")
}

target_df$GDP <- c(NA_real_, diff(log(target_df$GDP)))
target_df <- target_df[, c("Date", setdiff(names(target_df), "Date"))]

blocks_transformed$target <- target_df
transformation_info$target <- data.frame(
  variable = "GDP",
  recommended = "target",
  code = 3L,
  transformation = "logdiff_qoq",
  stringsAsFactors = FALSE
)

transformation_summary <- dplyr::bind_rows(
  lapply(names(transformation_info), function(block_name) {
    dplyr::mutate(transformation_info[[block_name]], block = block_name, .before = variable)
  })
)

transformation_summary


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
post_transformation_tests <- blocks_transformed[blocks_to_transform] |>
  purrr::map(function(block) {
    sapply(block %>% dplyr::select(-Date), test_transform)
  })

post_transformation_tests


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd("..")
save_transformation_info(transformation_info, "output/TRANSFORMATION_INFO.xlsx")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd("..")
save_list_to_excel(blocks_transformed, "blocks_transformed.xlsx")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax$`Goods and services`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax$`excess expenses`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$real_estate$`Real estate purchase tax`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$FX_liqudity$`Foreign exchange reserves (millions of shekels)`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$personal_labor_income_taxes$`Total refunds from the Income Tax Department`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$personal_labor_income_taxes$`Capital Gains Tax Refunds`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$personal_labor_income_taxes$`Non-profit institution tax`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax$`Cancellation companies`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax$`tax differential companies`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax$`Cancellations Deductions`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax$`Bonds and dividends`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$corporate_business_tax$`Companies returns`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$import_trade_tax$`Total import taxes`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$real_estate$`praise tax returns`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$real_estate$`Apartments sold at an annual rate`)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(blocks_transformed$FX_liqudity)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
blocks_shifted <- blocks_transformed
shift_report <- list()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
block <- blocks_transformed$FX_liqudity # choose your block
# lags <- c(1,0,1) # rep(val, n) # Evyatar: I'm adding a value for this to run with no errors
lags <- c(1,0,1,0)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
out <- shift_by_vector(block, lags)

blocks_shifted$FX_liqudity <- out$data
shift_report$FX_liqudity <- out$report


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd("..")
blocks_shifted_path <- "data/clean/blocks_shifted.xlsx"

save_list_to_excel(blocks_shifted, blocks_shifted_path)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd("..")
save_transformation_info(shift_report, "output/shift_report.xlsx")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Input: multi-sheet transformed + lag-adjusted workbook
setwd("..")
if (!exists("blocks_shifted_path")) {
  blocks_shifted_path <- "data/clean/blocks_shifted.xlsx"
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


