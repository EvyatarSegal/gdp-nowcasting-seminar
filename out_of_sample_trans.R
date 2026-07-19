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



if (!exists("is_shiny")) is_shiny <- FALSE

## --------------------------------------------------------------------------------------
path <- "data_h3.xlsx"
## --------------------------------------------------------------------------------------


## --------------------------------------------------------------------------------------
sheets <- excel_sheets(path)

sheets <- sheets[!sheets %in% c("dataupdate")]
## --------------------------------------------------------------------------------------
blocks_raw <- sheets %>% 
  set_names() %>% 
  map(~ read_excel(path, sheet = .x))

blocks_real <- sheets %>% 
  set_names() %>% 
  map(~ read_excel(path, sheet = .x))


## --------------------------------------------------------------------------------------
# 1. Convert Date column to standard Date format (as before)
blocks_raw <- map(blocks_raw, ~ {
  df <- .x
  df$Date <- as.Date(df$Date)
  df
})

# 2. Create the master timeline WITHOUT including the target sheet
# This ensures quarterly sheets don't force monthly NAs into themselves too early
all_dates <- map(blocks_raw[names(blocks_raw) != "target"], ~ .x$Date) %>% 
  unlist() %>% 
  as.Date(origin = "1970-01-01") %>% 
  na.omit() %>% 
  unique() %>% 
  sort()

master_dates <- tibble(Date = all_dates)

# 3. Align only the MONTHLY sheets to the master timeline, leave 'target' as is
blocks_raw <- imap(blocks_raw, ~ {
  if (.y == "target") {
    return(.x) # Keep target in its original quarterly length for lag processing
  } else {
    return(left_join(master_dates, .x, by = "Date"))
  }
})

blocks_real <- blocks_raw

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
if (!is_shiny) save_list_to_excel(blocks_real, "blocks_real.xlsx")


## --------------------------------------------------------------------------------------
if (!exists("td")) td <- read_csv("data/raw/td_var.csv")
if (!exists("td_ts")) {
  td_ts <- ts(
    td[, -1],
    start = c(year(min(td$date)), month(min(td$date))),
    frequency = 12
  )
}

if (!exists("preadj")) {
  preadj <- read_excel("data/raw/hol_preadj.xlsx") %>%
    mutate(date = as.Date(date))
}

if (!exists("hag_ts")) {
  hag_ts <- ts(
    preadj[, -1],
    start = c(year(min(preadj$date)), month(min(preadj$date))),
    frequency = 12
  )
}


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
if (!is_shiny) write_sa_info(sa_info, "SA_results_01.xlsx")


## --------------------------------------------------------------------------------------
if (!is_shiny) save_list_to_excel(blocks_sa, "blocks_sa_01.xlsx")


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
if (!is_shiny) save_transformation_info(transformation_info, "TRANSFORMATION_INFO.xlsx")



## --------------------------------------------------------------------------------------
if (!is_shiny) save_list_to_excel(blocks_transformed, "blocks_transformed.xlsx")


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
#shift_by_vector <- function(df, vec) {
# df: data.frame where the first column is 'Date'
# vec: numeric vector of lags, matching columns 2..n of df

# Sanity check
#  if (length(vec) != (ncol(df) - 1)) {
#    stop("Length of vec must match number of data columns (excluding Date).")
#  }

#  df_shifted <- df

# Prepare the report
#  report <- data.frame(
#    variable = colnames(df)[-1],
#    lag_applied = vec,
#    stringsAsFactors = FALSE
#  )

# Apply shifts
#  for (i in seq_along(vec)) {
#    k <- vec[i]
#    var <- colnames(df)[i + 1]   # +1 to skip Date

#    if (!is.numeric(df[[var]])) {
#      warning(paste("Column", var, "is not numeric — skipped."))
#      next
#    }

#    if (k > 0) {
# Shift forward: NA at the top, drop the last k values
#      df_shifted[[var]] <- c(rep(NA, k), df[[var]][1:(nrow(df) - k)])
#    } else {
# k == 0: no change
#      df_shifted[[var]] <- df[[var]]
#    }
#  }

#  return(list(
#    data = df_shifted,
#    report = report
#  ))
#}


## --------------------------------------------------------------------------------------
#blocks_shifted <- blocks_transformed
#shift_report <- list()
#View(blocks_transformed$FX_liqudity)


## --------------------------------------------------------------------------------------
#head(blocks_transformed$target$GDP)


## --------------------------------------------------------------------------------------
#block <- blocks_transformed$FX_liqudity # choose your block
#lags <- c(0,0,0) # rep(val, n)


## --------------------------------------------------------------------------------------
#out <- shift_by_vector(block, lags)

#blocks_shifted$FX_liqudity <- out$data
#shift_report$FX_liqudity <- out$report

## --------------------------------------------------------------------------------------
#block <- blocks_transformed$personal_labor_income_taxes 
#lags <- c(rep(1, ncol(block) - 1)) 

#out <- shift_by_vector(block, lags)
#blocks_shifted$personal_labor_income_taxes <- out$data
#shift_report$personal_labor_income_taxes <- out$report

## --------------------------------------------------------------------------------------
#block <- blocks_transformed$corporate_business_tax
#lags <- c(rep(1, ncol(block) - 1)) 

#out <- shift_by_vector(block, lags)
#blocks_shifted$corporate_business_tax <- out$data
#shift_report$corporate_business_tax <- out$report
## --------------------------------------------------------------------------------------
#block <- blocks_transformed$consumption_tax
#lags <- c(rep(1, ncol(block) - 1)) 

#out <- shift_by_vector(block, lags)
#blocks_shifted$consumption_tax <- out$data
#shift_report$consumption_tax <- out$report

## --------------------------------------------------------------------------------------
#block <- blocks_transformed$import_trade_tax
#lags <- c(rep(1, ncol(block) - 1)) 

#out <- shift_by_vector(block, lags)
#blocks_shifted$import_trade_tax <- out$data
#shift_report$import_trade_tax <- out$report

## --------------------------------------------------------------------------------------
#block <- blocks_transformed$real_estate
#lags <- c(rep(1, ncol(block) - 1)) 

#out <- shift_by_vector(block, lags)
#blocks_shifted$real_estate <- out$data
#shift_report$real_estate <- out$report

## -------------------------------------------------------------------------------------
#block <- blocks_transformed$real_activity 
#lags <- c(rep(2, ncol(block) - 1))

#out <- shift_by_vector(block, lags)
#blocks_shifted$real_activity <- out$data
#shift_report$real_activity <- out$report

## --------------------------------------------------------------------------------------
#block <- blocks_transformed$capital_markets 
#lags <- c(rep(0, ncol(block) - 1))

#out <- shift_by_vector(block, lags)
#blocks_shifted$capital_markets <- out$data
#shift_report$capital_markets <- out$report

## --------------------------------------------------------------------------------------
#block <- blocks_transformed$labor
#lags <- c(rep(2, ncol(block) - 1))

#out <- shift_by_vector(block, lags)
#blocks_shifted$labor <- out$data
#shift_report$labor <- out$report


## --------------------------------------------------------------------------------------
#block <- blocks_transformed$adjusters 
#lags <- c(0,2)


## --------------------------------------------------------------------------------------
#out <- shift_by_vector(block, lags)

#blocks_shifted$adjusters <- out$data
#shift_report$adjusters <- out$report

apply_paper_methodology_shifts <- function(blocks_transformed_list, raw_file_path) {
  # -------------------------------------------------------------------------
  # 1. READ LAG RULES
  # Read the publication lag metadata from the 'dataupdate' sheet.
  # This sheet contains the rules (e.g., "1month lag", "month+20 days lag").
  # -------------------------------------------------------------------------
  lag_rules <- readxl::read_excel(raw_file_path, sheet = "dataupdate")
  colnames(lag_rules) <- c("block_name", "lag_description")
  
  # -------------------------------------------------------------------------
  # 2. MAP TEXT TO SHIFT VALUES (BASED ON PAPER METHODOLOGY)
  # Convert the textual descriptions into numeric shifts (Push) based on the 
  # real-time forecasting rules:
  # - < 30 days lag -> push 1 month
  # - 30 to 60 days lag -> push 2 months
  # - > 60 days lag -> push 3 months
  # -------------------------------------------------------------------------
  lag_rules <- lag_rules %>%
    mutate(
      push_months = case_when(
        # Rule A: Daily or immediately known data -> available same month (Push = 0)
        grepl("known|daily", lag_description, ignore.case = TRUE) ~ 0,
        
        # Rule B: Lag of 1 month + additional days (e.g., "month+15", "month+20").
        # Total lag is ~45-50 days. By the paper's rule (>30 and <=60), we push 2 months.
        grepl("month\\+", lag_description, ignore.case = TRUE) ~ 1,
        
        # Rule C: Lag of exactly 1 month (<=30 days). We push 1 month.
        grepl("1month", lag_description, ignore.case = TRUE) ~ 1,
        
        # Default fallback if rule is not recognized
        TRUE ~ 1 
      )
    )
  
  # -------------------------------------------------------------------------
  # 3. APPLY SHIFTS TO THE DATA
  # Iterate over all blocks and apply the calculated shift to create the 
  # "Release-Date Aligned" dataset.
  # -------------------------------------------------------------------------
  blocks_shifted <- blocks_transformed_list
  
  for (block in names(blocks_transformed_list)) {
    # Skip the target (GDP) and adjusters (like CPI/VAT rates) from shifting.
    # Target variable must remain anchored to its reference quarter.
    if (block %in% c("target", "adjusters")) next 
    
    # Extract the required push value (k) for the current block
    k <- lag_rules %>% filter(block_name == block) %>% pull(push_months)
    
    # If the block is somehow missing from the update table, default to 1 month
    if (length(k) == 0) k <- 1
    
    df <- blocks_transformed_list[[block]]
    
    # Apply the shift only if k > 0
    if (k > 0) {
      df_shifted <- df
      for (col in setdiff(names(df), "Date")) {
        # Shift data forward: prepend 'k' NAs and drop the last 'k' observations
        df_shifted[[col]] <- c(rep(NA_real_, k), head(df[[col]], -k))
      }
      blocks_shifted[[block]] <- df_shifted
      cat(sprintf("✓ Block '%s': History shifted forward by %d month(s) (Release Alignment)\n", block, k))
    } else {
      cat(sprintf("✓ Block '%s': Kept in place (Lag = 0)\n", block))
    }
  }
  
  return(blocks_shifted)
}

blocks_shifted <- apply_paper_methodology_shifts(blocks_transformed, "data/raw/nowcasting_data_raw.xlsx")


## --------------------------------------------------------------------------------------
# 1. Take the GDP from the raw data
target_df <- as.data.frame(blocks_raw$target)                          
# 2. Transform it (The Fix)
target_df$GDP <- c(NA_real_, diff(log(target_df$GDP)))

# 3. Put it into the object that is about to be saved
blocks_shifted$target <- target_df

# 4. NOW save it (if not in Shiny)
blocks_shifted_path <- "data/clean/blocks_shifted.xlsx"
if (!is_shiny) save_list_to_excel(blocks_shifted, blocks_shifted_path)


## --------------------------------------------------------------------------------------
if (!is_shiny) save_transformation_info(shift_report, "shift_report.xlsx")


## --------------------------------------------------------------------------------------
# Input: multi-sheet transformed + lag-adjusted workbook
if (!exists("blocks_shifted_path")) {
  blocks_shifted_path <- "data/clean/blocks_shifted.xlsx"
}

input_path <- blocks_shifted_path

# Output: one-sheet panel for DFM
output_path <- "data/clean/combined_monthly_panel_Q_h3.xlsx"

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
combined_monthly_panel_Q_h3 <- purrr::reduce(
  blocks,
  dplyr::full_join,
  by = "Date"
) |>
  dplyr::arrange(Date)

# Drop leading rows until at least 2 variables have observed data
non_date_cols <- setdiff(names(combined_monthly_panel_Q_h3), "Date")

row_non_missing_count <- rowSums(
  !is.na(combined_monthly_panel_Q_h3[, non_date_cols, drop = FALSE])
)

first_valid_row <- which(row_non_missing_count >= 2)[1]

if (is.na(first_valid_row)) {
  stop("No row found with at least 2 non-missing variables.")
}

combined_monthly_panel_Q_h3 <- combined_monthly_panel_Q_h3[
  first_valid_row:nrow(combined_monthly_panel_Q_h3),
]

if (!"GDP" %in% names(combined_monthly_panel_Q_h3)) {
  stop("GDP was not carried into the unified modeling table.")
}

gdp_validation <- combined_monthly_panel_Q_h3 %>%
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
if (!is_shiny) {
  openxlsx::write.xlsx(
    combined_monthly_panel_Q_h3,
    file = output_path,
    sheetName = "combined_monthly_panel",
    overwrite = TRUE
  )
  message("Saved: ", output_path)
}




# =============================================================================
# DIAGNOSTIC: Check for newly created columns in final objects
# =============================================================================

cat("\n========== COLUMN COMPARISON ==========\n")

# 1. Compare raw vs transformed blocks (excluding Date and adjusters)
all_blocks <- names(blocks_raw)
blocks_to_check <- setdiff(all_blocks, "adjusters")  # adjusters is never transformed

for (b in blocks_to_check) {
  raw_cols <- sort(setdiff(names(blocks_raw[[b]]), "Date"))
  
  # For blocks_transformed (after SA + mathematical transforms)
  trans_cols <- sort(setdiff(names(blocks_transformed[[b]]), "Date"))
  
  # For blocks_shifted (after lags)
  shift_cols <- sort(setdiff(names(blocks_shifted[[b]]), "Date"))
  
  # New columns = in transformed but not in raw
  new_in_trans <- setdiff(trans_cols, raw_cols)
  new_in_shift <- setdiff(shift_cols, raw_cols)
  
  # Columns that disappeared (should never happen)
  missing_in_trans <- setdiff(raw_cols, trans_cols)
  missing_in_shift <- setdiff(raw_cols, shift_cols)
  
  if (length(new_in_trans) == 0 && length(new_in_shift) == 0) {
    cat(sprintf("✓ Block '%s': No new columns.\n", b))
  } else {
    if (length(new_in_trans) > 0) {
      cat(sprintf("⚠ Block '%s' (transformed) has NEW columns: %s\n", 
                  b, paste(new_in_trans, collapse = ", ")))
    }
    if (length(new_in_shift) > 0) {
      cat(sprintf("⚠ Block '%s' (shifted) has NEW columns: %s\n", 
                  b, paste(new_in_shift, collapse = ", ")))
    }
  }
  
  if (length(missing_in_trans) > 0) {
    cat(sprintf("   (lost columns in transformed: %s)\n", 
                paste(missing_in_trans, collapse = ", ")))
  }
  if (length(missing_in_shift) > 0) {
    cat(sprintf("   (lost columns in shifted: %s)\n", 
                paste(missing_in_shift, collapse = ", ")))
  }
}

# 2. Check the final combined panel
if (exists("combined_monthly_panel_Q_h3")) {
  panel_cols <- sort(setdiff(names(combined_monthly_panel_Q_h3), "Date"))
  
  # Gather all original columns from all raw blocks (excluding Date)
  all_raw_cols <- unique(unlist(lapply(blocks_raw, function(df) 
    setdiff(names(df), "Date"))))
  
  new_in_panel <- setdiff(panel_cols, all_raw_cols)
  missing_in_panel <- setdiff(all_raw_cols, panel_cols)
  
  if (length(new_in_panel) == 0) {
    cat("✓ Final panel: No new columns.\n")
  } else {
    cat(sprintf("⚠ Final panel has NEW columns: %s\n", 
                paste(new_in_panel, collapse = ", ")))
  }
  
  if (length(missing_in_panel) > 0) {
    cat(sprintf("   (columns from raw data missing in panel: %s)\n", 
                paste(missing_in_panel, collapse = ", ")))
  }
} else {
  cat("! Final panel 'combined_monthly_panel_Q_h3' not found.\n")
}

cat("========================================\n")
