# =============================================================================
# EXPLORATORY DATA ANALYSIS (EDA) FOR blocks_raw AND blocks_real
# =============================================================================

# -----------------------------
# 0. Setup: install & load required packages (if missing)
# -----------------------------

required_packages <- c("tidyverse", "ggplot2", "corrplot", "skimr", "summarytools")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Optional: set a nice plotting theme
theme_set(theme_minimal())

# -----------------------------
# 1. Check if datasets exist in environment
# -----------------------------

if (!exists("blocks_raw")) {
  stop("Object 'blocks_raw' not found. Please load or create it before running this script.")
}
if (!exists("blocks_real")) {
  stop("Object 'blocks_real' not found. Please load or create it before running this script.")
}

# -----------------------------
# 2. Basic overview of each dataset
# -----------------------------

cat("\n========== BLOCKS_RAW ==========\n")
cat("Dimensions:", dim(blocks_raw), "\n")
cat("Column names:\n")
print(colnames(blocks_raw))
cat("\nFirst 5 rows:\n")
print(head(blocks_raw, 5))
cat("\nStructure:\n")
str(blocks_raw)

cat("\n\n========== BLOCKS_REAL ==========\n")
cat("Dimensions:", dim(blocks_real), "\n")
cat("Column names:\n")
print(colnames(blocks_real))
cat("\nFirst 5 rows:\n")
print(head(blocks_real, 5))
cat("\nStructure:\n")
str(blocks_real)

# -----------------------------
# 3. Missing values analysis
# -----------------------------

cat("\n========== MISSING VALUES ==========\n")
cat("\n--- blocks_raw ---\n")
missing_raw <- colSums(is.na(blocks_raw))
print(missing_raw[missing_raw > 0])
cat("Total missing values:", sum(missing_raw), "\n")

cat("\n--- blocks_real ---\n")
missing_real <- colSums(is.na(blocks_real))
print(missing_real[missing_real > 0])
cat("Total missing values:", sum(missing_real), "\n")

# Optional: visualise missingness
if (require(naniar, quietly = TRUE)) {
  library(naniar)
  gg_miss_var(blocks_raw) + labs(title = "Missing values in blocks_raw")
  gg_miss_var(blocks_real) + labs(title = "Missing values in blocks_real")
} else {
  cat("\nInstall 'naniar' for advanced missing data plots: install.packages('naniar')\n")
}

# -----------------------------
# 4. Summary statistics (numerical & categorical)
# -----------------------------

cat("\n========== SUMMARY STATISTICS ==========\n")
cat("\n--- blocks_raw (skim) ---\n")
print(skimr::skim(blocks_raw))

cat("\n--- blocks_real (skim) ---\n")
print(skimr::skim(blocks_real))

# Alternative: summarytools (more detailed for categorical)
cat("\n--- blocks_raw (freq for categorical) ---\n")
print(summarytools::dfSummary(blocks_raw, 
                              max.col.width = 40,
                              plain.ascii = FALSE,
                              style = "grid"))

cat("\n--- blocks_real (freq for categorical) ---\n")
print(summarytools::dfSummary(blocks_real,
                              max.col.width = 40,
                              plain.ascii = FALSE,
                              style = "grid"))

# -----------------------------
# 5. Visual distributions
# -----------------------------

# Function to plot histograms for all numeric columns
plot_histograms <- function(df, title_prefix) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  if (length(numeric_cols) == 0) {
    cat("No numeric columns in", title_prefix, "\n")
    return(invisible())
  }
  
  for (col in numeric_cols) {
    p <- ggplot(df, aes(x = .data[[col]])) +
      geom_histogram(fill = "steelblue", color = "white", bins = 30, alpha = 0.7) +
      labs(title = paste(title_prefix, "-", col), x = col, y = "Count") +
      theme_minimal()
    print(p)
  }
}

# Function to plot boxplots for numeric columns (to spot outliers)
plot_boxplots <- function(df, title_prefix) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  if (length(numeric_cols) == 0) return(invisible())
  
  df_long <- df %>%
    select(all_of(numeric_cols)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value")
  
  p <- ggplot(df_long, aes(x = variable, y = value)) +
    geom_boxplot(fill = "tomato", alpha = 0.6, outlier.color = "red") +
    labs(title = paste(title_prefix, "- Boxplots for numeric variables"),
         x = "", y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

# Function to plot bar charts for categorical columns (max 10 categories)
plot_bars <- function(df, title_prefix) {
  categorical_cols <- names(df)[sapply(df, function(x) is.character(x) | is.factor(x))]
  if (length(categorical_cols) == 0) {
    cat("No categorical columns in", title_prefix, "\n")
    return(invisible())
  }
  
  for (col in categorical_cols) {
    freq <- table(df[[col]])
    if (length(freq) > 20) {
      cat("Column", col, "has >20 categories – skipping bar plot.\n")
      next
    }
    df_plot <- data.frame(category = names(freq), count = as.numeric(freq))
    p <- ggplot(df_plot, aes(x = reorder(category, -count), y = count)) +
      geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.7) +
      labs(title = paste(title_prefix, "-", col), x = col, y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
  }
}

# Generate plots
cat("\n========== GENERATING PLOTS ==========\n")
cat("Histograms for blocks_raw...\n")
plot_histograms(blocks_raw, "blocks_raw")
cat("Boxplots for blocks_raw...\n")
plot_boxplots(blocks_raw, "blocks_raw")
cat("Bar charts for blocks_raw...\n")
plot_bars(blocks_raw, "blocks_raw")

cat("\nHistograms for blocks_real...\n")
plot_histograms(blocks_real, "blocks_real")
cat("Boxplots for blocks_real...\n")
plot_boxplots(blocks_real, "blocks_real")
cat("Bar charts for blocks_real...\n")
plot_bars(blocks_real, "blocks_real")

# -----------------------------
# 6. Correlation analysis (numeric columns)
# -----------------------------

correlation_analysis <- function(df, title_prefix) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  if (length(numeric_cols) < 2) {
    cat("Not enough numeric columns in", title_prefix, "for correlation.\n")
    return(invisible())
  }
  
  # Remove columns with zero variance or all NA
  valid_cols <- numeric_cols[sapply(df[numeric_cols], function(x) var(x, na.rm = TRUE) > 0)]
  if (length(valid_cols) < 2) {
    cat("Insufficient variance in numeric columns of", title_prefix, "\n")
    return(invisible())
  }
  
  cor_matrix <- cor(df[valid_cols], use = "pairwise.complete.obs")
  cat("\n--- Correlation matrix (", title_prefix, ") ---\n")
  print(round(cor_matrix, 3))
  
  # Heatmap
  corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                     tl.col = "black", tl.srt = 45,
                     title = paste(title_prefix, "- Correlation heatmap"),
                     mar = c(0,0,2,0))
}

cat("\n========== CORRELATION ANALYSIS ==========\n")
correlation_analysis(blocks_raw, "blocks_raw")
correlation_analysis(blocks_real, "blocks_real")

# -----------------------------
# 7. Compare the two datasets (if they share columns)
# -----------------------------

cat("\n========== COMPARISON BETWEEN blocks_raw AND blocks_real ==========\n")
common_cols <- intersect(colnames(blocks_raw), colnames(blocks_real))
cat("Common columns:", paste(common_cols, collapse = ", "), "\n")

if (length(common_cols) > 0) {
  # Compare dimensions
  cat("\nRow counts: raw =", nrow(blocks_raw), ", real =", nrow(blocks_real), "\n")
  
  # Compare summary for first common numeric column (if any)
  num_common <- common_cols[sapply(blocks_raw[common_cols], is.numeric)]
  if (length(num_common) > 0) {
    first_num <- num_common[1]
    cat("\nComparison of numeric column '", first_num, "':\n", sep = "")
    cat("blocks_raw: mean =", mean(blocks_raw[[first_num]], na.rm = TRUE),
        ", sd =", sd(blocks_raw[[first_num]], na.rm = TRUE), "\n")
    cat("blocks_real: mean =", mean(blocks_real[[first_num]], na.rm = TRUE),
        ", sd =", sd(blocks_real[[first_num]], na.rm = TRUE), "\n")
  }
  
  # Side-by-side boxplot for common numeric columns
  if (length(num_common) >= 1) {
    comb_data <- bind_rows(
      blocks_raw %>% select(all_of(num_common)) %>% mutate(dataset = "raw"),
      blocks_real %>% select(all_of(num_common)) %>% mutate(dataset = "real")
    ) %>%
      pivot_longer(cols = all_of(num_common), names_to = "variable", values_to = "value")
    
    p_compare <- ggplot(comb_data, aes(x = dataset, y = value, fill = dataset)) +
      geom_boxplot(alpha = 0.6) +
      facet_wrap(~ variable, scales = "free_y") +
      labs(title = "Comparison of numeric variables: raw vs real",
           x = "", y = "Value") +
      theme_minimal()
    print(p_compare)
  }
} else {
  cat("No common column names – cannot directly compare.\n")
}

cat("\n========== EDA COMPLETED ==========\n")