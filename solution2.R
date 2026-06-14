# 1. Define your out-of-sample testing start date (the same one used in your rolling loop)
test_start_date <- as.Date("2020-12-01") # Change this if your forecast window moved forward


# 1. Clean the original dataset to remove duplicate months
df_clean <- df %>% distinct(Date, .keep_all = TRUE)

# 2. Extract factors and align to the dataset's dates
all_factors <- data.frame(dfm_curr$F_qml)
colnames(all_factors) <- c("f1", "f2", "f3", "f4")
all_factors$Date <- df_sub$Date

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

# 5. Join safely by QUARTER and filter dynamically
train_data <- x_train_prep %>%
  inner_join(y_train_prep, by = "Quarter", suffix = c("_factor", "_gdp")) %>%
  # DYNAMIC FILTER: Keep everything from the absolute beginning of your dataset, 
  # up until the month right before your test_start_date.
  filter(Date_factor < test_start_date)

# 6. Split into matrix and vector
X_mat <- as.matrix(train_data[, c("f1", "f2", "f3", "f4")])
y_vec <- train_data$GDP

# 7. Recalculate weights (This is ALREADY dynamic!)
# Because max_date looks at whatever the new train_data is, the weights 
# will perfectly auto-adjust to your new dates without changing the math.
max_date <- max(train_data$Date_factor)
time_diff_years <- as.numeric(difftime(max_date, train_data$Date_factor, units = "days")) / 365.25
weights_vec <- 1 / (1 + exp(3.968421 * (time_diff_years - 9.842105)))

# 8. Build the DMatrix
dtrain <- xgb.DMatrix(data = X_mat, label = y_vec, weight = weights_vec)
