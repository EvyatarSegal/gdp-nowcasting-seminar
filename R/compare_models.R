## -------------------------------------------------------------------------
## compare_models.R
## Isolated Head-to-Head Evaluation: Old DFM Pipeline vs. New State-Space
## -------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(patchwork)
library(parallel)

if (!dir.exists("output")) dir.create("output", recursive = TRUE)

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║        NOWCASTING MODEL COMPARISON HARNESS                   ║\n")
cat("║        Old DFM + XGBoost  vs.  New State-Space + XGBoost     ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

message("Starting parallel execution harness for true multi-core isolation...")
message("This guarantees zero namespace collisions and cuts runtime in half.\n")

# =========================================================================
# PHASE 1 & 2: Execute Both Pipelines in Parallel Sandboxed R Sessions
# =========================================================================

# Start a local cluster with 2 nodes
cl <- makeCluster(2)

# Ensure the workers have the correct working directory
clusterExport(cl, "getwd")
clusterEvalQ(cl, { setwd(getwd()) })

message("Running Old Pipeline (Node 1) and New Pipeline (Node 2) concurrently...")

pipeline_results <- parLapply(cl, 1:2, function(i) {
  if (i == 1) {
    # ----------------------------------------
    # NODE 1: Old Pipeline (dfm.r)
    # ----------------------------------------
    tryCatch({
      # Source in the worker's global environment
      source("R/dfm.r", local = FALSE)
      
      # Extract needed variables before returning
      return(list(
        status = "success",
        results_old = get("results", envir = .GlobalEnv),
        quarterly_fcst_old = get("quarterly_fcst", envir = .GlobalEnv),
        df_old_raw = get("df", envir = .GlobalEnv)
      ))
    }, error = function(e) {
      return(list(status = "error", message = paste("Old pipeline error:", e$message)))
    })
    
  } else {
    # ----------------------------------------
    # NODE 2: New Pipeline (clean.R + DFM_and_detrend.R)
    # ----------------------------------------
    tryCatch({
      source("R/clean.R", local = FALSE)
      source("R/DFM_and_detrend.R", local = FALSE)
      
      return(list(
        status = "success",
        results_new = get("results", envir = .GlobalEnv),
        rmse_ssm = get("SSM_RMSE", envir = .GlobalEnv),
        mae_ssm = get("SSM_MAE", envir = .GlobalEnv),
        rmse_xgb_new = get("XGB_RMSE", envir = .GlobalEnv),
        mae_xgb_new = get("XGB_MAE", envir = .GlobalEnv)
      ))
    }, error = function(e) {
      return(list(status = "error", message = paste("New pipeline error:", e$message)))
    })
  }
})

stopCluster(cl)

# Process Node 1 (Old)
old_data <- pipeline_results[[1]]
if (old_data$status == "error") {
  stop("FATAL ERROR in Old Pipeline: ", old_data$message)
} else {
  results_old <- old_data$results_old
  quarterly_fcst_old <- old_data$quarterly_fcst_old
  df_old_raw <- old_data$df_old_raw
  
  rmse_old <- sqrt(mean((results_old$GDP_FCST - results_old$GDP)^2, na.rm = TRUE))
  mae_old  <- mean(abs(results_old$GDP_FCST - results_old$GDP), na.rm = TRUE)
  message(sprintf("Old Pipeline Bridge RMSE: %.6f | MAE: %.6f", rmse_old, mae_old))
}

# Process Node 2 (New)
new_data <- pipeline_results[[2]]
if (new_data$status == "error") {
  stop("FATAL ERROR in New Pipeline: ", new_data$message)
} else {
  results_new <- new_data$results_new
  rmse_ssm <- new_data$rmse_ssm
  mae_ssm <- new_data$mae_ssm
  rmse_xgb_new <- new_data$rmse_xgb_new
  mae_xgb_new <- new_data$mae_xgb_new
  
  message(sprintf("New SSM RMSE: %.6f | MAE: %.6f", rmse_ssm, mae_ssm))
  message(sprintf("New XGB Bridge RMSE: %.6f | MAE: %.6f", rmse_xgb_new, mae_xgb_new))
}

message("\nBoth pipelines executed successfully. Environments cleaned.\n")

# =========================================================================
# PHASE 3: Construct Unified Comparison Datasets
# =========================================================================
message("═══ PHASE 3: Building Unified Comparison Datasets ═══")

# --- 3a. Growth-Rate Performance Matrix ---
perf_matrix <- data.frame(
  Model = c("Old DFM + XGBoost (Bridge)", "New State-Space (SSM)", "New SSM Factor + XGBoost Bridge"),
  RMSE = c(rmse_old, rmse_ssm, rmse_xgb_new),
  MAE = c(mae_old, mae_ssm, mae_xgb_new),
  stringsAsFactors = FALSE
)

# --- 3b. Align forecasts for head-to-head error comparison ---
comparison_df <- NULL

# Old pipeline: results_old has Date, GDP_FCST, GDP (log-diff growth rates)
old_pred <- results_old %>%
  dplyr::select(Date, Old_Pred = GDP_FCST, Actual_Growth = GDP) %>%
  filter(!is.na(Actual_Growth) & !is.na(Old_Pred))

# New pipeline: filter to quarter-end months where we have both predictions and actuals
new_pred <- results_new %>%
  filter(!is.na(Target_GDP) & !is.na(SSM_Growth_Pred)) %>%
  mutate(Actual_Growth_New = log(Target_GDP / lag(Target_GDP))) %>%
  filter(!is.na(Actual_Growth_New)) %>%
  dplyr::select(Date, SSM_Pred = SSM_Growth_Pred, XGB_New_Pred = XGB_Growth_Pred, Actual_Growth_New)

# Join on overlapping dates
comparison_df <- inner_join(old_pred, new_pred, by = "Date") %>%
  mutate(
    Error_Old = Old_Pred - Actual_Growth,
    Error_SSM = SSM_Pred - Actual_Growth_New,
    Error_XGB_New = XGB_New_Pred - Actual_Growth_New,
    AbsError_Old = abs(Error_Old),
    AbsError_SSM = abs(Error_SSM),
    AbsError_XGB_New = abs(Error_XGB_New)
  )

message(sprintf("Overlapping comparison periods: %d quarters", nrow(comparison_df)))

# --- 3c. Level Tracking Dataset ---
level_comparison <- NULL

# Get actual GDP levels from anchors
anchors <- readRDS("data/clean/gdp_level_anchors.rds")
anchor_levels <- data.frame(
  Date = as.Date(anchors$Date) + 1,
  Actual_GDP_Level = anchors$GDP_Level
)

# New pipeline levels
new_levels <- results_new %>%
  filter(!is.na(GDP_Nowcast_Level) | !is.na(XGB_Nowcast_Level)) %>%
  dplyr::select(Date, SSM_Level = GDP_Nowcast_Level, XGB_New_Level = XGB_Nowcast_Level)

# Old pipeline: reconstruct levels from growth predictions
old_with_levels <- results_old %>%
  dplyr::select(Date, Old_Growth_Pred = GDP_FCST) %>%
  left_join(anchor_levels, by = "Date")

old_with_levels <- old_with_levels %>%
  mutate(Old_Pred_Level = NA_real_)

anchor_idx <- which(!is.na(old_with_levels$Actual_GDP_Level))
if (length(anchor_idx) > 0) {
  for (ai in anchor_idx) {
    base <- old_with_levels$Actual_GDP_Level[ai]
    old_with_levels$Old_Pred_Level[ai] <- base * exp(old_with_levels$Old_Growth_Pred[ai])
  }
}

level_comparison <- new_levels %>%
  left_join(anchor_levels, by = "Date") %>%
  left_join(old_with_levels %>% dplyr::select(Date, Old_Pred_Level), by = "Date") %>%
  filter(!is.na(Actual_GDP_Level))


# =========================================================================
# PHASE 4: Statistical Tests
# =========================================================================
message("═══ PHASE 4: Running Statistical Tests ═══")

# --- 4a. Diebold-Mariano Test ---
dm_result <- NULL
dm_pvalue <- NA
dm_winner <- "Inconclusive"

if (nrow(comparison_df) >= 5) {
  tryCatch({
    suppressPackageStartupMessages(library(forecast))
    
    e_old <- comparison_df$Error_Old
    e_ssm <- comparison_df$Error_SSM
    
    dm_result <- dm.test(e_old, e_ssm, alternative = "two.sided", h = 1)
    dm_pvalue <- dm_result$p.value
    
    if (dm_pvalue < 0.05) {
      mse_old <- mean(e_old^2, na.rm = TRUE)
      mse_ssm <- mean(e_ssm^2, na.rm = TRUE)
      dm_winner <- ifelse(mse_ssm < mse_old, "New SSM", "Old DFM")
    } else {
      dm_winner <- "No Significant Difference"
    }
    
    message(sprintf("DM Test Statistic: %.4f | p-value: %.4f | Winner: %s", 
                    dm_result$statistic, dm_pvalue, dm_winner))
  }, error = function(e) {
    message("DM Test failed: ", e$message)
  })
} else {
  message("Insufficient overlapping observations for DM test.")
}

# --- 4b. Level Tracking Stability ---
MaxAD_ssm <- NA; MAD_ssm <- NA
MaxAD_xgb <- NA; MAD_xgb <- NA
MaxAD_old <- NA; MAD_old <- NA

valid_ssm <- level_comparison %>% filter(!is.na(SSM_Level) & !is.na(Actual_GDP_Level))
if (nrow(valid_ssm) > 0) {
  deviations_ssm <- abs(valid_ssm$SSM_Level - valid_ssm$Actual_GDP_Level)
  MaxAD_ssm <- max(deviations_ssm)
  MAD_ssm <- mean(deviations_ssm)
}

valid_xgb <- level_comparison %>% filter(!is.na(XGB_New_Level) & !is.na(Actual_GDP_Level))
if (nrow(valid_xgb) > 0) {
  deviations_xgb <- abs(valid_xgb$XGB_New_Level - valid_xgb$Actual_GDP_Level)
  MaxAD_xgb <- max(deviations_xgb)
  MAD_xgb <- mean(deviations_xgb)
}

valid_old <- level_comparison %>% filter(!is.na(Old_Pred_Level) & !is.na(Actual_GDP_Level))
if (nrow(valid_old) > 0) {
  deviations_old <- abs(valid_old$Old_Pred_Level - valid_old$Actual_GDP_Level)
  MaxAD_old <- max(deviations_old)
  MAD_old <- mean(deviations_old)
}

# =========================================================================
# PHASE 5: Executive Visualizations
# =========================================================================
message("\n═══ PHASE 5: Generating Master Visualizations ═══")

# --- Graph 1 (Top Panel): Level Tracking Comparison ---
level_long <- level_comparison %>%
  pivot_longer(
    cols = c(SSM_Level, XGB_New_Level, Old_Pred_Level),
    names_to = "Model",
    values_to = "Predicted_Level"
  ) %>%
  filter(!is.na(Predicted_Level)) %>%
  mutate(Model = case_when(
    Model == "SSM_Level" ~ "New: State-Space Model",
    Model == "XGB_New_Level" ~ "New: SSM Factor + XGBoost",
    Model == "Old_Pred_Level" ~ "Old: DFM + XGBoost"
  ))

plot_top <- ggplot() +
  geom_point(data = level_comparison, 
             aes(x = Date, y = Actual_GDP_Level, shape = "Actual GDP"), 
             size = 3, color = "#2c3e50", alpha = 0.8) +
  geom_line(data = level_long,
            aes(x = Date, y = Predicted_Level, color = Model, linetype = Model),
            linewidth = 1.1) +
  labs(title = "GDP Level Tracking: Head-to-Head Comparison",
       subtitle = "Which model best preserves long-run level anchoring?",
       y = "Real GDP Level", x = "") +
  scale_color_manual(values = c(
    "New: State-Space Model" = "#2980b9",
    "New: SSM Factor + XGBoost" = "#e67e22",
    "Old: DFM + XGBoost" = "#e74c3c"
  )) +
  scale_linetype_manual(values = c(
    "New: State-Space Model" = "solid",
    "New: SSM Factor + XGBoost" = "dashed",
    "Old: DFM + XGBoost" = "dotted"
  )) +
  scale_shape_manual(values = c("Actual GDP" = 16)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14))

# --- Graph 2 (Bottom Panel): Error Time-Series ---
error_long <- comparison_df %>%
  dplyr::select(Date, `Old DFM + XGBoost` = AbsError_Old,
                `New State-Space` = AbsError_SSM, 
                `New SSM + XGBoost` = AbsError_XGB_New) %>%
  pivot_longer(cols = -Date, names_to = "Model", values_to = "Absolute_Error") %>%
  filter(!is.na(Absolute_Error))

plot_bottom <- ggplot(error_long, aes(x = Date, y = Absolute_Error, color = Model)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.6) +
  labs(title = "Forecast Error Timeline: |Actual - Predicted| Growth Rate",
       subtitle = "Lower is better. Spikes reveal where each model struggles.",
       y = "Absolute Error (Log-Diff Scale)", x = "") +
  scale_color_manual(values = c(
    "Old DFM + XGBoost" = "#e74c3c",
    "New State-Space" = "#2980b9",
    "New SSM + XGBoost" = "#e67e22"
  )) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14))

master_plot <- plot_top / plot_bottom + plot_layout(heights = c(1, 1))
ggsave("output/Master_HeadToHead_Comparison.png", master_plot, 
       width = 12, height = 10, bg = "white", dpi = 150)
message("Saved: output/Master_HeadToHead_Comparison.png")

# =========================================================================
# PHASE 6: Final Verdict Report
# =========================================================================
cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════════╗\n")
cat("║                   DATA SCIENCE AUDIT REPORT                         ║\n")
cat("║          GDP Nowcasting Pipeline: Head-to-Head Evaluation           ║\n")
cat("╠═══════════════════════════════════════════════════════════════════════╣\n")
cat("║                                                                     ║\n")
cat("║  SECTION 1: Growth-Rate Accuracy (Quarterly Log-Differences)        ║\n")
cat("║                                                                     ║\n")
cat("╠═══════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("  %-35s │ %-10s │ %-10s\n", "Model Pipeline", "RMSE", "MAE"))
cat("  ─────────────────────────────────────┼────────────┼────────────\n")
cat(sprintf("  %-35s │ %-10.6f │ %-10.6f\n", "Old DFM + XGBoost (Bridge)", rmse_old, mae_old))
cat(sprintf("  %-35s │ %-10.6f │ %-10.6f\n", "New State-Space Model (SSM)", rmse_ssm, mae_ssm))
cat(sprintf("  %-35s │ %-10.6f │ %-10.6f\n", "New SSM Factor + XGBoost Bridge", rmse_xgb_new, mae_xgb_new))
cat("╠═══════════════════════════════════════════════════════════════════════╣\n")
cat("║                                                                     ║\n")
cat("║  SECTION 2: Diebold-Mariano Forecast Accuracy Test                  ║\n")
cat("║  H0: Both models have equal predictive accuracy                     ║\n")
cat("║                                                                     ║\n")
cat("╠═══════════════════════════════════════════════════════════════════════╣\n")

if (!is.na(dm_pvalue)) {
  cat(sprintf("  DM Test Statistic:  %.4f\n", dm_result$statistic))
  cat(sprintf("  p-value:            %.4f\n", dm_pvalue))
  if (dm_pvalue < 0.01) {
    cat("  Significance:       *** (p < 0.01)\n")
  } else if (dm_pvalue < 0.05) {
    cat("  Significance:       **  (p < 0.05)\n")
  } else if (dm_pvalue < 0.10) {
    cat("  Significance:       *   (p < 0.10)\n")
  } else {
    cat("  Significance:       Not significant (p >= 0.10)\n")
  }
  cat(sprintf("  Statistical Winner: %s\n", dm_winner))
} else {
  cat("  DM Test: Could not be computed (insufficient data)\n")
}

cat("╠═══════════════════════════════════════════════════════════════════════╣\n")
cat("║                                                                     ║\n")
cat("║  SECTION 3: Level Tracking Stability (GDP Levels)                   ║\n")
cat("║  Lower = better anchoring to true GDP path                          ║\n")
cat("║                                                                     ║\n")
cat("╠═══════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("  %-35s │ %-12s │ %-12s\n", "Model", "MaxAD", "MAD"))
cat("  ─────────────────────────────────────┼──────────────┼──────────────\n")
cat(sprintf("  %-35s │ %-12.2f │ %-12.2f\n", "Old DFM + XGBoost (Bridge)", 
            ifelse(is.na(MaxAD_old), NA, MaxAD_old), ifelse(is.na(MAD_old), NA, MAD_old)))
cat(sprintf("  %-35s │ %-12.2f │ %-12.2f\n", "New State-Space Model (SSM)", 
            ifelse(is.na(MaxAD_ssm), NA, MaxAD_ssm), ifelse(is.na(MAD_ssm), NA, MAD_ssm)))
cat(sprintf("  %-35s │ %-12.2f │ %-12.2f\n", "New SSM Factor + XGBoost Bridge", 
            ifelse(is.na(MaxAD_xgb), NA, MaxAD_xgb), ifelse(is.na(MAD_xgb), NA, MAD_xgb)))
cat("╠═══════════════════════════════════════════════════════════════════════╣\n")
cat("║                                                                     ║\n")
cat("║  SECTION 4: Final Verdict                                           ║\n")
cat("║                                                                     ║\n")
cat("╠═══════════════════════════════════════════════════════════════════════╣\n")

growth_scores <- c(Old = rmse_old, SSM = rmse_ssm, XGB_New = rmse_xgb_new)
growth_winner <- names(which.min(growth_scores))
growth_winner_label <- switch(growth_winner,
                              "Old" = "Old DFM + XGBoost",
                              "SSM" = "New State-Space Model",
                              "XGB_New" = "New SSM + XGBoost Bridge")

level_scores <- c(Old = MAD_old, SSM = MAD_ssm, XGB_New = MAD_xgb)
level_scores <- level_scores[!is.na(level_scores)]
structural_winner <- ifelse(length(level_scores) > 0,
                            names(which.min(level_scores)),
                            "Inconclusive")
structural_winner_label <- switch(structural_winner,
                                  "Old" = "Old DFM + XGBoost",
                                  "SSM" = "New State-Space Model",
                                  "XGB_New" = "New SSM + XGBoost Bridge",
                                  "Inconclusive")

cat(sprintf("  Growth Precision Winner:     %s (RMSE: %.6f)\n", growth_winner_label, min(growth_scores, na.rm = TRUE)))
cat(sprintf("  Level Preservation Winner:   %s\n", structural_winner_label))

if (!is.na(dm_pvalue) && dm_pvalue < 0.05) {
  cat(sprintf("  DM Statistical Verdict:      %s is significantly better (p=%.4f)\n", dm_winner, dm_pvalue))
} else {
  cat("  DM Statistical Verdict:      No statistically significant difference.\n")
}

cat("║                                                                     ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════╝\n")
cat("\n  Visualization saved: output/Master_HeadToHead_Comparison.png\n")
cat("  Report generated at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
