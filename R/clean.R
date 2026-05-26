## -------------------------------------------------------------------------
## clean.R
## Refactored Data Preprocessing for State-Space Nowcasting Pipeline
## -------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(lubridate)
library(seasonal)
library(openxlsx)
library(readr)

# --- Configuration & Helpers ---
raw_path <- "data/raw/nowcasting_data_raw.xlsx"
td_path  <- "data/raw/td_var.csv"
hol_path <- "data/raw/hol_preadj.xlsx"

if (!dir.exists("data/clean")) dir.create("data/clean", recursive = TRUE)

# --- 1. Load Data ---
message("--- Step 1: Loading Raw Data from Excel ---")
sheets <- excel_sheets(raw_path)
blocks_raw <- sheets %>% 
  set_names() %>% 
  map(~ {
    df <- read_excel(raw_path, sheet = .x)
    df$Date <- as.Date(df$Date)
    df
  })

# Target anchor logic
# We save the initial GDP levels BEFORE ANY transformations.
target_raw <- blocks_raw$target %>% arrange(Date)
gdp_level_anchors <- list(
  Date = target_raw$Date,
  GDP_Level = target_raw$GDP
)
saveRDS(gdp_level_anchors, "data/clean/gdp_level_anchors.rds")
message("Saved GDP level anchors.")

# --- 2. CPI and FX Adjustments ---
message("--- Step 2: Applying CPI and FX Adjustments ---")
# We convert all nominal values to real values using the CPI from 'adjusters' sheet.
cpi_df <- blocks_raw$adjusters %>% dplyr::select(Date, CPI)

adjust_for_cpi <- function(df, cpi, cols = "all") {
  df <- df %>% left_join(cpi, by = "Date")
  if (identical(cols, "all")) cols <- setdiff(names(df), c("Date", "CPI"))
  for (col in cols) {
    if (col %in% names(df)) df[[col]] <- df[[col]] / (df$CPI / 100)
  }
  df %>% dplyr::select(-CPI)
}

blocks_real <- blocks_raw
blocks_real$personal_labor_income_taxes <- adjust_for_cpi(blocks_raw$personal_labor_income_taxes, cpi_df)
blocks_real$corporate_business_tax <- adjust_for_cpi(blocks_raw$corporate_business_tax, cpi_df)
blocks_real$consumption_tax <- adjust_for_cpi(blocks_raw$consumption_tax, cpi_df)
blocks_real$import_trade_tax <- adjust_for_cpi(blocks_raw$import_trade_tax, cpi_df)

# Specific columns for real estate
re_cols <- c("Real estate taxation", "Property tax", "praise tax", 
             "Real estate purchase tax", "praise tax returns", "purchase returns")
blocks_real$real_estate <- adjust_for_cpi(blocks_raw$real_estate, cpi_df, cols = re_cols)

# Real activity - multiply oil by dollar
blocks_real$real_activity$Oil <- blocks_raw$real_activity$Oil * blocks_raw$FX_liqudity$Dollar
blocks_real$real_activity <- adjust_for_cpi(blocks_real$real_activity, cpi_df, cols = "Oil")

# FX Liquidity
fx_col <- "Foreign exchange reserves (millions of shekels)"
blocks_real$FX_liqudity[[fx_col]] <- blocks_raw$FX_liqudity$`Foreign exchange reserves (millions of dollars)` * blocks_raw$FX_liqudity$Dollar
blocks_real$FX_liqudity <- adjust_for_cpi(blocks_real$FX_liqudity, cpi_df, cols = fx_col)
# Remove the old dollar column as it's been converted
blocks_real$FX_liqudity <- blocks_real$FX_liqudity %>% dplyr::select(-`Foreign exchange reserves (millions of dollars)`)

# --- 3. Seasonal Adjustment (X-13) ---
message("--- Step 3: Running Seasonal Adjustments (X-13ARIMA) ---")
td_df <- read_csv(td_path, show_col_types = FALSE)
td_ts <- ts(td_df[, -1], start = c(year(min(td_df$date)), month(min(td_df$date))), frequency = 12)

hol_df <- read_excel(hol_path) %>% mutate(date = as.Date(date))
hag_ts <- ts(hol_df[, -1], start = c(year(min(hol_df$date)), month(min(hol_df$date))), frequency = 12)

seasonal_adjust_block <- function(df, hag_ts, td_ts) {
  df_out <- df
  vars <- setdiff(names(df), "Date")
  message("Running seasonal adjustments for ", length(vars), " variables. This may take a moment...")
  pb <- txtProgressBar(min = 0, max = length(vars), style = 3)
  for (i in seq_along(vars)) {
    v <- vars[i]
    setTxtProgressBar(pb, i)
    x <- df[[v]]
    first_idx <- min(which(!is.na(x)))
    last_idx  <- max(which(!is.na(x)))
    segment <- x[first_idx:last_idx]
    
    if (any(is.na(segment)) || all(is.na(x))) next
    
    y <- ts(segment, start = c(year(df$Date[first_idx]), month(df$Date[first_idx])), frequency = 12)
    
    fit <- tryCatch({
      seas(y, x11 = "", outlier.types = "ao", transform.function = "auto",
           xreg = cbind(hag_ts, td_ts), regression.usertype = rep("holiday", ncol(hag_ts)))
    }, error = function(e) NULL)
    
    if (!is.null(fit)) {
      df_out[[v]][first_idx:last_idx] <- as.numeric(predict(fit))
    }
  }
  close(pb)
  return(df_out)
}

# Apply SA
blocks_sa <- blocks_real
blocks_sa$real_estate <- seasonal_adjust_block(blocks_real$real_estate, hag_ts, td_ts)
# NOTE: If other blocks need SA, they can be added here. (Following original logic, mostly real estate was highlighted)

# --- 4. Controlled Transformations (Dictionary Mapping) ---
message("--- Step 4: Applying Controlled Transformations ---")
# Heuristic mapping: 
# Most macroeconomic aggregates (production, tax revenues, consumption) are volumes/levels -> log-diff.
# Interest rates, unemployment rates, sentiment indices are rates -> first-diff.

apply_transform <- function(x, method = "log_diff") {
  n <- length(x)
  out <- rep(NA_real_, n)
  if (method == "log_diff") {
    # Ensure positive values before log
    x_pos <- ifelse(x > 0, x, NA)
    out[2:n] <- diff(log(x_pos))
  } else if (method == "first_diff") {
    out[2:n] <- diff(x)
  }
  return(out)
}

blocks_to_transform <- setdiff(names(blocks_sa), c("adjusters", "target"))
blocks_stat <- blocks_sa

# Define specific rates (if any exist in your data). Otherwise, default to log_diff.
rate_variables <- c("Unemployment Rate", "Interest Rate") 

for (b in blocks_to_transform) {
  vars <- setdiff(names(blocks_stat[[b]]), "Date")
  for (v in vars) {
    method <- ifelse(v %in% rate_variables, "first_diff", "log_diff")
    blocks_stat[[b]][[v]] <- apply_transform(blocks_sa[[b]][[v]], method)
  }
}

# Transform target variable (GDP) to Quarterly Log-Difference
target_stat <- blocks_sa$target %>% arrange(Date)
target_stat_valid <- target_stat %>% filter(!is.na(GDP))
target_stat_valid$GDP <- apply_transform(target_stat_valid$GDP, "log_diff")
blocks_stat$target <- target_stat_valid

# --- 5. Combine into Final Panel ---
message("--- Step 5: Combining Blocks into Final Panel ---")
# Join all transformed blocks (except adjusters)
sheets_to_join <- setdiff(names(blocks_stat), "adjusters")
blocks_to_join <- map(sheets_to_join, ~ {
  df <- blocks_stat[[.x]]
  if (.x == "target") df$Date <- df$Date + 1 # Align to start of next month like original script
  df
})

combined_panel <- purrr::reduce(blocks_to_join, full_join, by = "Date") %>% arrange(Date)

# Drop leading rows with entirely missing data
row_non_missing <- rowSums(!is.na(combined_panel %>% dplyr::select(-Date)))
first_valid <- which(row_non_missing >= 2)[1]
combined_panel <- combined_panel[first_valid:nrow(combined_panel), ]

# --- 6. Export ---
message("--- Step 6: Exporting Stationary Panel ---")
output_path <- "data/clean/combined_monthly_panel_Q_stat.xlsx"
write.xlsx(combined_panel, file = output_path, overwrite = TRUE)
message("Saved stationary panel matrix: ", output_path)

