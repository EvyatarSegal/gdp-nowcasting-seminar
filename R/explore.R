# =============================================================================
# EXPLORATORY DATA ANALYSIS (EDA) FOR LIST OF DATAFRAMES: blocks_raw AND blocks_real
# =============================================================================

# -----------------------------
# 0. Setup: install & load required packages (if missing)
# -----------------------------

required_packages <- c("tidyverse", "ggplot2", "corrplot", "skimr", "summarytools", "gridExtra")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

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

# Helper function to check if an object is a list of data frames
is_list_of_dfs <- function(x) {
  is.list(x) && all(sapply(x, function(y) is.data.frame(y) || tibble::is_tibble(y)))
}

if (!is_list_of_dfs(blocks_raw)) {
  stop("blocks_raw is not a list of data frames/tibbles. Please check its structure.")
}
if (!is_list_of_dfs(blocks_real)) {
  stop("blocks_real is not a list of data frames/tibbles. Please check its structure.")
}

cat("\n==================================================\n")
cat("Found", length(blocks_raw), "blocks in blocks_raw")
cat("\nFound", length(blocks_real), "blocks in blocks_real")
cat("\n==================================================\n")

# -----------------------------
# 2. Function to explore a single data frame (block)
# -----------------------------

explore_block <- function(df, block_name, dataset_name) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Block:", block_name, " (", dataset_name, ")\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  # Basic info
  cat("Dimensions:", paste(dim(df), collapse = " x "), "\n")
  cat("Column names:\n")
  print(colnames(df))
  cat("\nFirst 3 rows:\n")
  print(head(df, 3))
  
  # Missing values
  missing_counts <- colSums(is.na(df))
  if (any(missing_counts > 0)) {
    cat("\nMissing values per column:\n")
    print(missing_counts[missing_counts > 0])
    cat("Total missing values:", sum(missing_counts), "\n")
  } else {
    cat("\nNo missing values found.\n")
  }
  
  # Summary statistics (skimr)
  cat("\nSummary statistics (skimr):\n")
  print(skimr::skim(df))
  
  # Separate numeric and categorical columns
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  categorical_cols <- names(df)[sapply(df, function(x) is.character(x) | is.factor(x))]
  
  # Histograms for numeric columns (max 6 per block to avoid overload)
  if (length(numeric_cols) > 0) {
    cat("\nGenerating histograms for numeric columns...\n")
    n_plots <- min(length(numeric_cols), 6)
    for (i in seq_len(n_plots)) {
      col <- numeric_cols[i]
      p <- ggplot(df, aes(x = .data[[col]])) +
        geom_histogram(fill = "steelblue", color = "white", bins = 30, alpha = 0.7) +
        labs(title = paste(dataset_name, "-", block_name, "-", col),
             x = col, y = "Count") +
        theme_minimal()
      print(p)
    }
    if (length(numeric_cols) > 6) {
      cat("... (only first 6 numeric columns shown)\n")
    }
    
    # Boxplots for numeric columns (all together, if more than 1)
    if (length(numeric_cols) >= 2) {
      df_long <- df %>%
        select(all_of(numeric_cols)) %>%
        pivot_longer(everything(), names_to = "variable", values_to = "value")
      p_box <- ggplot(df_long, aes(x = variable, y = value)) +
        geom_boxplot(fill = "tomato", alpha = 0.6, outlier.color = "red") +
        labs(title = paste(dataset_name, "-", block_name, "- Boxplots"),
             x = "", y = "Value") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      print(p_box)
    }
  } else {
    cat("\nNo numeric columns in this block.\n")
  }
  
  # Bar plots for categorical columns (max 5 categories shown)
  if (length(categorical_cols) > 0) {
    cat("\nGenerating bar plots for categorical columns...\n")
    for (col in categorical_cols) {
      freq <- table(df[[col]])
      if (length(freq) > 20) {
        cat("Column", col, "has >20 categories – skipping bar plot.\n")
        next
      }
      df_plot <- data.frame(category = names(freq), count = as.numeric(freq))
      p <- ggplot(df_plot, aes(x = reorder(category, -count), y = count)) +
        geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.7) +
        labs(title = paste(dataset_name, "-", block_name, "-", col),
             x = col, y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      print(p)
    }
  }
  
  # Correlation matrix for numeric columns (if at least 2)
  if (length(numeric_cols) >= 2) {
    # Remove columns with zero variance or all NA
    valid_cols <- numeric_cols[sapply(df[numeric_cols], function(x) var(x, na.rm = TRUE) > 0)]
    if (length(valid_cols) >= 2) {
      cor_matrix <- cor(df[valid_cols], use = "pairwise.complete.obs")
      cat("\nCorrelation matrix (first 6x6 if large):\n")
      if (ncol(cor_matrix) > 6) {
        print(round(cor_matrix[1:6, 1:6], 3))
        cat("... (showing first 6 rows/cols)\n")
      } else {
        print(round(cor_matrix, 3))
      }
      # Heatmap
      corrplot::corrplot(cor_matrix, method = "color", type = "upper",
                         tl.col = "black", tl.srt = 45,
                         title = paste(dataset_name, block_name, "correlation"),
                         mar = c(0,0,2,0))
    } else {
      cat("\nNot enough numeric columns with variance for correlation.\n")
    }
  }
}

# -----------------------------
# 3. Loop over each block in blocks_raw and blocks_real
# -----------------------------

cat("\n\n========== EXPLORING blocks_raw ==========\n")
for (block_name in names(blocks_raw)) {
  df <- blocks_raw[[block_name]]
  explore_block(df, block_name, "RAW")
}

cat("\n\n========== EXPLORING blocks_real ==========\n")
for (block_name in names(blocks_real)) {
  df <- blocks_real[[block_name]]
  explore_block(df, block_name, "REAL")
}

# -----------------------------
# 4. Compare raw vs real for matching block names
# -----------------------------

common_blocks <- intersect(names(blocks_raw), names(blocks_real))
if (length(common_blocks) == 0) {
  cat("\nNo common block names between raw and real – skipping comparison.\n")
} else {
  cat("\n\n========== COMPARISON: RAW vs REAL ==========\n")
  cat("Common blocks:", paste(common_blocks, collapse = ", "), "\n")
  
  for (block in common_blocks) {
    df_raw <- blocks_raw[[block]]
    df_real <- blocks_real[[block]]
    
    cat("\n", paste(rep("-", 50), collapse = ""), "\n")
    cat("Comparing block:", block, "\n")
    
    # Dimensions
    cat("Dimensions - raw:", paste(dim(df_raw), collapse = " x "),
        " | real:", paste(dim(df_real), collapse = " x "), "\n")
    
    # Check if both have same columns
    common_cols <- intersect(colnames(df_raw), colnames(df_real))
    if (length(common_cols) == 0) {
      cat("No common column names – cannot compare numeric values.\n")
      next
    }
    
    # Identify common numeric columns
    num_common <- common_cols[sapply(df_raw[common_cols], is.numeric) &
                                sapply(df_real[common_cols], is.numeric)]
    if (length(num_common) == 0) {
      cat("No common numeric columns to compare.\n")
      next
    }
    
    # For the first common numeric column, show summary comparison
    first_num <- num_common[1]
    cat("\nComparison of numeric column '", first_num, "':\n", sep = "")
    cat("RAW: mean =", mean(df_raw[[first_num]], na.rm = TRUE),
        ", sd =", sd(df_raw[[first_num]], na.rm = TRUE), "\n")
    cat("REAL: mean =", mean(df_real[[first_num]], na.rm = TRUE),
        ", sd =", sd(df_real[[first_num]], na.rm = TRUE), "\n")
    
    # Side-by-side boxplot for up to first 3 common numeric columns
    n_plot <- min(length(num_common), 3)
    if (n_plot >= 1) {
      comb_data <- bind_rows(
        df_raw %>% select(all_of(num_common[1:n_plot])) %>% mutate(dataset = "raw"),
        df_real %>% select(all_of(num_common[1:n_plot])) %>% mutate(dataset = "real")
      ) %>%
        pivot_longer(cols = all_of(num_common[1:n_plot]), names_to = "variable", values_to = "value")
      
      p_compare <- ggplot(comb_data, aes(x = dataset, y = value, fill = dataset)) +
        geom_boxplot(alpha = 0.6) +
        facet_wrap(~ variable, scales = "free_y") +
        labs(title = paste("Comparison:", block),
             x = "", y = "Value") +
        theme_minimal()
      print(p_compare)
    }
  }
}

cat("\n\n========== EDA COMPLETED ==========\n")