df_evya <- read_excel("data/clean/combined_monthly_panel_Q_refined_evyatar.xlsx")

df_elad <- read_excel("data/clean/combined_monthly_panel_Q_refined_elad.xlsx")

df_diff <- setdiff(df_elad, df_evya)

# Find columns that are in Elad's data, but missing from Evyatar's
extra_in_elad <- setdiff(names(df_elad), names(df_evya))
print(extra_in_elad)

# Find columns that are in Evyatar's data, but missing from Elad's
extra_in_evya <- setdiff(names(df_evya), names(df_elad))
print(extra_in_evya)

# Find the common columns they both share
common_cols <- intersect(names(df_elad), names(df_evya))

library(dplyr)

# Remove the extra columns and save to a new dataframe
df_evya_reduce <- df_evya %>% 
  dplyr::select(-any_of(extra_in_evya))




# I see zoo is already loaded at the top of your script!
# library(zoo) 

# 1. Clean the original dataset to remove duplicate months
df_clean <- df_evya_reduce %>% distinct(Date, .keep_all = TRUE)

# 2. Extract factors and align to the dataset's dates
all_factors <- data.frame(dfm_curr$F_qml)
colnames(all_factors) <- c("f1", "f2", "f3", "f4")
all_factors$Date <- df_clean$Date

# 3. Prepare Predictors (X) - Keep only target factor months (1, 4, 7, 10)
x_train_prep <- all_factors %>%
  filter(as.integer(format(Date, "%m")) %in% c(1, 4, 7, 10)) %>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  distinct(Quarter, .keep_all = TRUE) # Ensure exactly 1 factor row per quarter

# 4. Prepare Target (y) - Keep only actual GDP values
y_train_prep <- df_clean %>%
  filter(!is.na(GDP)) %>%
  dplyr::select(Date, GDP) %>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  distinct(Quarter, .keep_all = TRUE) # Ensure exactly 1 GDP value per quarter

# 5. Join safely by QUARTER, so the exact months don't have to match
train_data <- x_train_prep %>%
  inner_join(y_train_prep, by = "Quarter", suffix = c("_factor", "_gdp")) %>%
  # Apply your original training window cutoff
  filter(Date_factor >= as.Date("1995-07-01") & Date_factor <= as.Date("2020-11-01"))

# 6. Split into matrix and vector
X_mat <- as.matrix(train_data[, c("f1", "f2", "f3", "f4")])
y_vec <- train_data$GDP

# 7. Recalculate weights using the factor dates
max_date <- max(train_data$Date_factor)
time_diff_years <- as.numeric(difftime(max_date, train_data$Date_factor, units = "days")) / 365.25
weights_vec <- 1 / (1 + exp(3.968421 * (time_diff_years - 9.842105)))

# 8. Build the DMatrix safely
dtrain <- xgb.DMatrix(data = X_mat, label = y_vec, weight = weights_vec)
