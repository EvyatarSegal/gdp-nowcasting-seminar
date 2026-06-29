# =============================================================================
# Covariance Matrix for combined_monthly_panel_Q_refined.xls (text only)
# =============================================================================

# Load required packages
if (!require(readxl)) install.packages("readxl")
library(readxl)

# -----------------------------
# 1. Read the Excel file
# -----------------------------
file_path <- "data/clean/combined_monthly_panel_Q_refined.xlsx"

if (!file.exists(file_path)) {
  stop("File not found at: ", file_path)
}

data <- read_excel(file_path, sheet = 1)

cat("\nDataset dimensions:", dim(data), "\n")
cat("Column names:\n")
print(colnames(data))

# -----------------------------
# 2. Prepare numeric data
# -----------------------------
numeric_data <- data[sapply(data, is.numeric)]
# Remove constant columns (variance = 0) if any
numeric_data <- numeric_data[, sapply(numeric_data, function(x) var(x, na.rm = TRUE) > 0)]

cat("\nNumber of numeric columns used:", ncol(numeric_data), "\n")
cat("Column names for covariance matrix:\n")
print(colnames(numeric_data))

# -----------------------------
# 3. Compute covariance matrix
# -----------------------------
# Use pairwise complete observations to handle missing values
cov_matrix <- cov(numeric_data, use = "pairwise.complete.obs")

# -----------------------------
# 4. Print the full covariance matrix
# -----------------------------
cat("\n========== COVARIANCE MATRIX ==========\n")
print(cov_matrix)

# Optionally, print with 2 decimal places (if numbers are large, you can adjust)
cat("\n========== COVARIANCE MATRIX (rounded to 2 decimals) ==========\n")
print(round(cov_matrix, 2))

# -----------------------------
# 5. Save to CSV (optional)
# -----------------------------
if (!dir.exists("output")) dir.create("output", recursive = TRUE)
write.csv(cov_matrix, file = "output/covariance_matrix.csv", row.names = TRUE)
cat("\nCovariance matrix saved to 'output/covariance_matrix.csv'\n")

# -----------------------------
# 6. Also show variances (diagonal) and standard deviations
# -----------------------------
variances <- diag(cov_matrix)
cat("\n========== VARIANCES (diagonal) ==========\n")
print(variances)

cat("\n========== STANDARD DEVIATIONS ==========\n")
print(sqrt(variances))

cat("\nDone.\n")



library(readxl)
data <- read_excel("data/clean/combined_monthly_panel_Q_refined.xlsx")
num <- data[sapply(data, is.numeric)]
corm <- cor(num, use = "pairwise.complete.obs")
# Get pairs with |cor| > 0.93 (excluding diagonal)
high <- which(abs(corm) > 0.9 & lower.tri(corm), arr.ind = TRUE)
pairs <- data.frame(
  Var1 = rownames(corm)[high[,1]],
  Var2 = rownames(corm)[high[,2]],
  Cor = corm[high]
)
print(pairs[order(-abs(pairs$Cor)), ])



library(readxl)
library(dplyr)

# Read data (adjust file extension if needed)
data <- read_excel("data/clean/combined_monthly_panel_Q_refined.xls")

# Flatten Date to month-year (assumes column named "Date")
if("Date" %in% colnames(data)) {
  data$month_year <- format(data$Date, "%Y-%m")
  cat("Added 'month_year' column. Unique periods:", length(unique(data$month_year)), "\n")
} else {
  warning("Column 'Date' not found. Proceeding without month_year.")
}

# 1. NA count per variable (column)
na_per_var <- colSums(is.na(data))
cat("\n========== NA count per variable ==========\n")
print(na_per_var[na_per_var > 0])  # only columns with NAs

# 2. Row-wise NA count: how many NAs in each row (including the new month_year column if present)
row_na_count <- rowSums(is.na(data))
cat("\n========== Row-wise NA summary ==========\n")
cat("Mean NAs per row:", mean(row_na_count), "\n")
cat("Median NAs per row:", median(row_na_count), "\n")

# 3. For rows where GDP is not NA, average number of NA columns
if("GDP" %in% colnames(data)) {
  rows_gdp_not_na <- !is.na(data$GDP)
  mean_na_in_gdp_rows <- mean(rowSums(is.na(data[rows_gdp_not_na, ])))
  cat("\nAverage number of NA columns in rows where GDP is not NA:", mean_na_in_gdp_rows, "\n")
} else {
  cat("\nColumn 'GDP' not found. Check column names.\n")
}




library(readxl)
library(dplyr)

# Read data
data <- read_excel("data/clean/combined_monthly_panel_Q_refined.xlsx")

# 1. Create month-year column
data$month_year <- format(data$Date, "%Y-%m")

# 2. Aggregate: one row per month-year, average numeric columns
#    (non-numeric columns like original Date are dropped; month_year stays)
data_agg <- data %>%
  group_by(month_year) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# 3. Now run NA exploration on the aggregated dataset
# 3a. NA count per variable
na_per_var <- colSums(is.na(data_agg))
cat("\n========== NA count per variable (aggregated monthly) ==========\n")
print(na_per_var[na_per_var > 0])

# 3b. Row-wise NA count
row_na_count <- rowSums(is.na(data_agg))
cat("\n========== Row-wise NA summary (aggregated) ==========\n")
cat("Mean NAs per row:", mean(row_na_count), "\n")
cat("Median NAs per row:", median(row_na_count), "\n")

# 3c. For rows where GDP is not NA (if GDP exists)
if("GDP" %in% colnames(data_agg)) {
  rows_gdp_not_na <- !is.na(data_agg$GDP)
  mean_na_in_gdp_rows <- mean(rowSums(is.na(data_agg[rows_gdp_not_na, ])))
  cat("\nAverage number of NA columns in rows where GDP is not NA (aggregated):", mean_na_in_gdp_rows, "\n")
} else {
  cat("\nColumn 'GDP' not found in aggregated data.\n")
}

