# ==============================================================================
# 14. HISTORICAL DYNAMIC ATTRIBUTION (TIME-SERIES NEWS)
# ==============================================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# ==============================================================================
# 13. TIME-SERIES VISUALIZATIONS & REPORTING (FIXED DISCRETE BAR VIEW)
# ==============================================================================
cat("[3/3] Generating visual tracking dashboards...\n")

# Plot 1: Heatmap of Variable Impacts over Time
p_historical_heatmap <- ggplot(historical_news_df, aes(x = Date, y = Variable, fill = Impact)) +
  geom_tile() +
  scale_fill_gradient2(low = "#B91C1C", mid = "white", high = "#15803D", midpoint = 0, 
                       name = "GDP Growth Impact") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    title = "Macro Feature Importance Trajectory",
    subtitle = "Tracking which specific variables drive the GDP nowcast during different economic regimes",
    x = "Timeline", y = "Predictor Variables"
  )

print(p_historical_heatmap)

# FIXED Plot 2: Stacked Bar Chart of Structural Sector Contributions
# Uses geom_col() to cleanly stack positive contributions upward and negative ones downward
p_historical_bar <- ggplot(historical_sector_df, aes(x = Date, y = Sector_Impact, fill = Sector)) +
  geom_col(width = 20, alpha = 0.85, color = NA) + # width=20 ensures clean gaps between monthly bars on a Date axis
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.6) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray30")
  ) +
  labs(
    title = "Historical GDP Growth Structural Breakdown",
    subtitle = "Net structural contribution of macro sectors (Positives stack up, Negatives stack down)",
    x = "Timeline", y = "Contribution to Quarterly Growth Rate",
    fill = "Macro Sector"
  )

print(p_historical_bar)

# Save processed structural panel data to disk
dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
write.csv(historical_news_df, "data/clean/historical_variable_impacts.csv", row.names = FALSE)
write.csv(historical_sector_df, "data/clean/historical_sector_impacts.csv", row.names = FALSE)

cat("======================================================================\n")
cat("✓ SUCCESS: Historical structural attribution run completed successfully.\n")
cat("Data exports saved to: data/clean/historical_[variable/sector]_impacts.csv\n")
cat("======================================================================\n")