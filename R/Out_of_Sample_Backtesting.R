# ==============================================================================
# CORRECTED DATA VINTAGE GENERATOR FOR NOWCASTING BACKTEST (PRESERVE SHEETS)
# Keeps future rows intact as NA so the model can generate forecasts for March 2026.
# Preserves the original multi-sheet structure.
# ==============================================================================

# Load required libraries
library(readxl)
library(openxlsx) # Using openxlsx as it's better for writing multi-sheet workbooks
library(dplyr)
library(purrr)

# 1. Path to the raw Excel file
file_path <- "data/raw/nowcasting_data_raw.xlsx" # Make sure this path is correct

# 2. Get all sheet names
sheets <- excel_sheets(file_path)

# 3. Define the lagged columns (same as before)
lagged_cols <- c(
  # Personal & Corporate taxes
  "Total Gross Income Tax Division", "Total refunds from the Income Tax Department", 
  "Total Income Tax Division Net", "Deductions and the capital market", 
  "Deduction from salary", "Independents advances", "Self-employed tax differences", 
  "Independent Cancellations", "self employed returns", "Capital Gains Tax Refunds", 
  "VAT Financial Institutions (Salary)", "Non-profit institution tax",
  "Companies advances", "tax differential companies", "Cancellation companies", 
  "Income tax for self-employed individuals and companies (advances and deductions)", 
  "Companies returns", "excess expenses", "Bonds and dividends", 
  "Cancellations Deductions", "Goods and services",
  
  # Consumption & Import taxes
  "Gross local VAT", "VAT refund autonomy and traders", "Total net VAT", 
  "Net local sales tax", "Gross fuel tax", "Gross import VAT", "Net customs", 
  "Net import purchase tax", "Total import taxes",
  
  # Real Estate
  "Real estate taxation", "Property tax", "praise tax", "Real estate purchase tax", 
  "praise tax returns", "purchase returns", "Apartments sold at an annual rate", 
  "Apartment Price Index (1993=100) - Mid-period reviewed",
  
  # Real Activity
  "cons_trust", "madad meshulav", "madad_cc_purchases_sa", "madad_pedio", 
  "madad_yetzur_industrial", "Oil", "madad_hadash",
  
  # Labor & Macro
  "real salary", "salaried jobs", "unemployment rate", "participation rate", 
  "employment rate", "CPI", "GDP"
)

# Function to process a single sheet based on the vintage rules
process_sheet <- function(sheet_name, vintage_type) {
  # Read the sheet
  df <- read_excel(file_path, sheet = sheet_name)
  
  # If there's no Date column or it's the 'dataupdate' sheet, return as is
  if (!"Date" %in% names(df) || sheet_name == "dataupdate") {
    return(df)
  }
  
  df$Date <- as.Date(df$Date)
  
  # Filter up to March 2026
  df <- df %>% filter(Date <= as.Date("2026-03-31"))
  
  # Identify columns to process in THIS specific sheet
  sheet_cols <- setdiff(names(df), "Date")
  sheet_lagged_cols <- intersect(sheet_cols, lagged_cols)
  
  # Apply vintage logic
  if (vintage_type == "H0") {
    # H0: Slightly before the official GDP publication.
    # All data for March 2026 is fully available, except GDP itself (set to NA).
    if ("GDP" %in% names(df)) {
      df <- df %>% mutate(GDP = ifelse(Date >= as.Date("2026-03-01"), NA, GDP))
    }
    
  } else if (vintage_type == "H1") {
    # H1: After February data release.
    # Fast series have data up to March 2026.
    # Lagged series have data up to February 2026 (delete March 2026).
    if (length(sheet_lagged_cols) > 0) {
      df <- df %>% mutate(across(all_of(sheet_lagged_cols), ~ ifelse(Date >= as.Date("2026-03-01"), NA, .)))
    }
    
  } else if (vintage_type == "H2") {
    # H2: After January data release.
    # Fast series have data up to February 2026 (delete March 2026).
    # Lagged series have data up to January 2026 (delete February 2026 and March 2026).
    fast_cols <- setdiff(sheet_cols, sheet_lagged_cols)
    if (length(fast_cols) > 0) {
      df <- df %>% mutate(across(all_of(fast_cols), ~ ifelse(Date >= as.Date("2026-03-01"), NA, .)))
    }
    if (length(sheet_lagged_cols) > 0) {
      df <- df %>% mutate(across(all_of(sheet_lagged_cols), ~ ifelse(Date >= as.Date("2026-02-01"), NA, .)))
    }
    
  } else if (vintage_type == "H3") {
    # H3: Start of the quarter (Nowcast/Forecast with limited information).
    # Fast series have data up to January 2026 (delete February and March 2026).
    # Lagged series only have data up to December 2025 (delete January, February, and March 2026).
    fast_cols <- setdiff(sheet_cols, sheet_lagged_cols)
    if (length(fast_cols) > 0) {
      df <- df %>% mutate(across(all_of(fast_cols), ~ ifelse(Date >= as.Date("2026-02-01"), NA, .)))
    }
    if (length(sheet_lagged_cols) > 0) {
      df <- df %>% mutate(across(all_of(sheet_lagged_cols), ~ ifelse(Date >= as.Date("2026-01-01"), NA, .)))
    }
  }
  
  return(df)
}

# Function to create and save a full workbook for a given vintage
create_vintage_workbook <- function(vintage_type, output_filename) {
  wb <- createWorkbook()
  
  for (sheet in sheets) {
    # Process the data for this sheet
    processed_df <- process_sheet(sheet, vintage_type)
    
    # Add sheet to workbook and write data
    addWorksheet(wb, sheet)
    writeData(wb, sheet, processed_df)
  }
  
  saveWorkbook(wb, output_filename, overwrite = TRUE)
  cat(paste("Created:", output_filename, "\n"))
}

# ------------------------------------------------------------------------------
# Generate all 4 vintages
# ------------------------------------------------------------------------------
create_vintage_workbook("H0", "data_h0.xlsx")
create_vintage_workbook("H1", "data_h1.xlsx")
create_vintage_workbook("H2", "data_h2.xlsx")
create_vintage_workbook("H3", "data_h3.xlsx")

print("All 4 Multi-Sheet Vintages successfully created!")