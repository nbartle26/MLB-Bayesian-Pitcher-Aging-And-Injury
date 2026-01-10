library(tidyverse)
library(rstanarm)
library(changepoint)
library(beepr)
library(readxl)
library(lubridate)
set.seed(42)

stats <- read_csv("C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/stats.csv")

cat("  Raw stats shape:", nrow(stats), "rows,", ncol(stats), "columns\n")
cat("  Years covered:", min(stats$year), "to", max(stats$year), "\n")
cat("  Unique pitchers:", length(unique(stats$player_id)), "\n\n")

# Load Tommy John surgery data
tj_data <- read_excel("C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/CopyofTommyJohnSurgeryList.xlsx", 
                      skip = 1)

# Keep ALL TJ surgeries per pitcher (not just first)
tj_pitchers_all <- tj_data %>%
  filter(Position == "P", !is.na(mlbamid)) %>%
  select(player_id = mlbamid, Player, `TJ Surgery Date`, Team, Age) %>%
  rename(tj_surgery_date = `TJ Surgery Date`, tj_age = Age) %>%
  mutate(
    player_id = as.integer(player_id),
    tj_year = year(tj_surgery_date)
  ) %>%
  arrange(player_id, tj_surgery_date)

# Create summary with TJ number for each pitcher
tj_summary <- tj_pitchers_all %>%
  group_by(player_id) %>%
  mutate(tj_number = row_number()) %>%
  ungroup() %>%
  select(player_id, tj_year, tj_number)

cat("  Total TJ surgeries for pitchers:", nrow(tj_pitchers_all), "\n")
cat("  Unique pitchers with TJ:", length(unique(tj_pitchers_all$player_id)), "\n")
cat("  TJ date range:", min(tj_pitchers_all$tj_surgery_date, na.rm = TRUE), 
    "to", max(tj_pitchers_all$tj_surgery_date, na.rm = TRUE), "\n\n")

# ============================================================
# STEP 2: CLEAN STATS DATA (VELOCITY + COMMAND)
# ============================================================

cat("Cleaning pitcher data...\n")

pitcher_stats <- stats %>%
  filter(
    !is.na(ff_avg_speed),                      # Must have fastball velocity
    !is.na(player_age),                        # Must have age
    !is.na(bb_percent),                        # Must have walk rate
    ff_avg_speed >= 85 & ff_avg_speed <= 105, # Remove velocity outliers
    bb_percent >= 0 & bb_percent <= 40         # Remove command outliers
  ) %>%
  mutate(
    player_name = str_trim(`last_name, first_name`),
    velocity = ff_avg_speed,
    command_proxy = bb_percent,
    age = player_age,
    # Track season quality
    season_type = case_when(
      p_formatted_ip >= 30 ~ "full_season",
      p_formatted_ip > 0 & p_formatted_ip < 30 ~ "partial_season",
      TRUE ~ "no_data"
    )
  ) %>%
  select(
    player_id, player_name, year, age, velocity, command_proxy, season_type, p_formatted_ip,
    whiff_percent, k_percent, bb_percent, woba, xwoba
  ) %>%
  arrange(player_id, year)

cat("  Cleaned stats shape:", nrow(pitcher_stats), "rows\n")
cat("  Season types:\n")
print(table(pitcher_stats$season_type))
cat("\n")

# ============================================================
# STEP 3: MERGE INJURY DATA & CALCULATE TJ_COUNT
# ============================================================

cat("Merging injury data...\n")

# For each pitcher-year, count how many TJs they've had BEFORE that season
pitcher_data_merged <- pitcher_stats %>%
  left_join(
    tj_summary %>%
      expand_grid(year = min(pitcher_stats$year):max(pitcher_stats$year)) %>%
      filter(year > tj_year) %>%
      group_by(player_id, year) %>%  # Group by both player and year
      summarise(tj_count = n(), .groups = 'drop'),
    by = c("player_id", "year")
  ) %>%
  mutate(
    tj_count = replace_na(tj_count, 0),
    had_tommy_john = as.numeric(tj_count > 0)
  )

cat("  Pitchers with TJ history:", 
    length(unique(pitcher_data_merged$player_id[pitcher_data_merged$had_tommy_john > 0])), "\n\n")

# ============================================================
# STEP 4: HANDLE MISSING SEASONS
# ============================================================

# Complete year sequences for each pitcher (fills in missing years)
pitcher_data_complete <- pitcher_data_merged %>%
  group_by(player_id) %>%
  filter(n() >= 3) %>%  # Must have at least 3 observed seasons
  complete(year = min(year):max(year)) %>%
  ungroup()

# Fill in pitcher info for missing rows and identify injury years
pitcher_data_complete <- pitcher_data_complete %>%
  group_by(player_id) %>%
  fill(player_name, .direction = "downup") %>%
  mutate(
    # Mark missing vs observed data
    is_missing = is.na(velocity),
    
    # Use tj_count to detect injury years (when tj_count increases)
    tj_count_filled = replace_na(tj_count, 0),
    tj_this_year = tj_count_filled > lag(tj_count_filled, default = 0),
    # is_injury_year = tj_this_year | lag(tj_this_year, default = FALSE),
    is_injury_year = tj_this_year,
    
    # Classify gap type
    gap_type = case_when(
      !is_missing ~ "data_present",
      is_injury_year ~ "injury_related",
      TRUE ~ "other_absence"),
    
    # Update had_tommy_john for filled rows
    had_tommy_john = as.numeric(tj_count_filled > 0)
  ) %>%
  select(-tj_this_year, -tj_count_filled) %>%
  ungroup()

cat("  Total rows after filling gaps:", nrow(pitcher_data_complete), "\n")
cat("  Gap types:\n")
print(table(pitcher_data_complete$gap_type))
cat("\n")

# ============================================================
# STEP 5: CALCULATE CAREER METRICS + RECOVERY TRAJECTORIES
# ============================================================

cat("Calculating career metrics and recovery trajectories...\n")

# Calculate data-driven decline threshold (remove extreme outliers first)
normal_changes <- pitcher_data_complete %>%
  filter(!is_missing, !is_injury_year) %>%
  group_by(player_id) %>%
  arrange(year) %>%
  mutate(velocity_change = velocity - lag(velocity)) %>%
  ungroup() %>%
  filter(
    !is.na(velocity_change),
    velocity_change > -10 & velocity_change < 5  # Remove extreme outliers
  ) %>%
  pull(velocity_change)

decline_threshold <- -2 * sd(normal_changes, na.rm = TRUE)
cat("  Data-driven decline threshold:", round(decline_threshold, 2), "mph\n\n")

# Add career metrics and recovery trajectories
pitcher_data_final <- pitcher_data_complete %>%
  group_by(player_id, player_name) %>%
  arrange(year) %>%
  mutate(
    # Career year (accounts for ALL years including missing)
    career_year = year - min(year) + 1,
    
    # Command score (inverse of BB%, higher = better)
    command_score = 1 - (command_proxy / 100),
    
    # Create tj_event_year when tj_count increases
    tj_event_year = ifelse(tj_count > lag(tj_count, default = 0), year, NA)
  ) %>%
  fill(tj_event_year, .direction = "down") %>%  # Forward-fill only
  mutate(
    # Calculate years since TJ
    years_since_tj = ifelse(!is.na(tj_event_year), year - tj_event_year, 99),
    years_since_tj_capped = pmin(years_since_tj, 5),
    
    # TIME-AWARE CHANGES (only for observed data)
    years_since_last = ifelse(!is_missing, year - lag(year[!is_missing]), NA),
    
    # Velocity change per year (normalized by time gap)
    velocity_change = ifelse(
      !is_missing & !is.na(lag(velocity[!is_missing])),
      (velocity - lag(velocity[!is_missing])) / years_since_last,
      NA
    ),
    
    # Command change per year
    command_change = ifelse(
      !is_missing & !is.na(lag(command_score[!is_missing])),
      (command_score - lag(command_score[!is_missing])) / years_since_last,
      NA
    ),
    
    # Decline indicator (velocity drop > threshold/2)
    decline_indicator = ifelse(
      !is.na(velocity_change) & velocity_change < decline_threshold / 2,
      1, 0
    )
  ) %>%
  ungroup()

cat("  Final dataset shape:", nrow(pitcher_data_final), "rows\n")
cat("  Total pitchers:", length(unique(pitcher_data_final$player_id)), "\n")
cat("  Average career span:", 
    round(mean(pitcher_data_final %>% 
                 group_by(player_id) %>% 
                 summarise(span = max(year) - min(year) + 1) %>% 
                 pull(span)), 1), "years\n\n")

# Save processed data
cat("Saving processed data...\n")
saveRDS(pitcher_data_final, 
        "C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/pitcher_data_final.rds")

# DIAGNOSTIC: Check command data quality
cat("\n========== COMMAND DATA DIAGNOSTICS ==========\n")
cat("Command proxy (BB%) summary:\n")
print(summary(pitcher_stats$command_proxy))

cat("\nCommand score summary:\n")
print(summary(pitcher_data_final$command_score[!pitcher_data_final$is_missing]))

cat("\nCommand score variance:\n")
print(sd(pitcher_data_final$command_score[!pitcher_data_final$is_missing], na.rm = TRUE))

# Look at a few pitchers' command trajectories
sample_pitchers <- pitcher_data_final %>%
  filter(!is_missing) %>%
  group_by(player_id) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  distinct(player_id) %>%
  head(3)

cat("\nSample pitcher command trajectories:\n")
for(pid in sample_pitchers$player_id) {
  pitcher_data <- pitcher_data_final %>%
    filter(player_id == pid, !is_missing) %>%
    select(player_name, year, command_proxy, command_score) %>%
    head(10)
  print(pitcher_data)
  cat("\n")
}

# ============================================================
# STEP 6: CHANGEPOINT DETECTION
# ============================================================

cat("\n========== CHANGEPOINT DETECTION ==========\n\n")

detect_changepoints <- function(pitcher_data, metric = "velocity") {
  changepoint_results <- list()
  
  # Only analyze pitchers with observed data
  pitcher_data_observed <- pitcher_data %>%
    filter(!is_missing) %>%
    group_by(player_id) %>%
    filter(n() >= 3) %>%  # Need 3+ seasons minimum
    ungroup()
  
  unique_pitcher_ids <- unique(pitcher_data_observed$player_id)
  
  cat("Detecting changepoints for", length(unique_pitcher_ids), "pitchers...\n")
  pb <- txtProgressBar(min = 0, max = length(unique_pitcher_ids), style = 3)
  
  for(i in seq_along(unique_pitcher_ids)) {
    pitcher_id <- unique_pitcher_ids[i]
    pitcher_subset <- pitcher_data_observed %>% 
      filter(player_id == pitcher_id) %>%
      arrange(year)
    
    pitcher_name <- first(pitcher_subset$player_name)
    
    if(nrow(pitcher_subset) >= 3) {
      ts_data <- pitcher_subset[[metric]]
      
      tryCatch({
        cpt_result <- cpt.mean(ts_data, method = "BinSeg", Q = 1, penalty = "BIC")
        changepoint_index <- cpts(cpt_result)
        
        if(length(changepoint_index) > 0) {
          changepoint_index <- changepoint_index[1]
          
          if(changepoint_index < length(ts_data)) {
            actual_year <- pitcher_subset$year[changepoint_index]
            
            pre_decline_mean <- mean(ts_data[1:changepoint_index], na.rm = TRUE)
            post_decline_mean <- mean(ts_data[(changepoint_index+1):length(ts_data)], na.rm = TRUE)
            decline_magnitude <- pre_decline_mean - post_decline_mean
            
            # Threshold: 0.5 mph for velocity, 0.05 for command (FIXED from 0.02)
            threshold <- ifelse(metric == "velocity", 0.5, 0.05)
            
            if(decline_magnitude > threshold) {
              sd_pre <- sd(ts_data[1:changepoint_index], na.rm = TRUE)
              sd_post <- sd(ts_data[(changepoint_index+1):length(ts_data)], na.rm = TRUE)
              pooled_sd <- sqrt((sd_pre^2 + sd_post^2) / 2)
              
              effect_size <- abs(decline_magnitude) / (pooled_sd + 0.01)
              changepoint_prob <- pmin(0.95, pmax(0.5, effect_size / 3))
              
              changepoint_results[[as.character(pitcher_id)]] <- data.frame(
                player_id = pitcher_id,
                pitcher = pitcher_name,
                changepoint_year = actual_year,
                changepoint_confidence = changepoint_prob,
                pre_decline_performance = pre_decline_mean,
                post_decline_performance = post_decline_mean,
                decline_magnitude = decline_magnitude,
                career_years_to_decline = changepoint_index,
                metric = metric
              )
            }
          }
        }
      }, error = function(e) NULL)
    }
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  return(bind_rows(changepoint_results))
}

cat("Detecting velocity changepoints...\n")
changepoint_velocity <- detect_changepoints(pitcher_data_final, "velocity")

cat("Detecting command changepoints...\n")
changepoint_command <- detect_changepoints(pitcher_data_final, "command_score")

changepoint_results <- bind_rows(changepoint_velocity, changepoint_command)

cat("\nChangepoint Summary:\n")
cat("  Velocity declines detected:", sum(changepoint_results$metric == "velocity"), "\n")
cat("  Command declines detected:", sum(changepoint_results$metric == "command_score"), "\n")
cat("  Average years to decline:", round(mean(changepoint_results$career_years_to_decline), 1), "\n")
cat("  Average decline magnitude:\n")
cat("    Velocity:", round(mean(changepoint_results$decline_magnitude[changepoint_results$metric == "velocity"]), 2), "mph\n")
cat("    Command:", round(mean(changepoint_results$decline_magnitude[changepoint_results$metric == "command_score"], na.rm = TRUE), 3), "\n\n")

# ============================================================
# STEP 7: BAYESIAN HIERARCHICAL MODELS WITH RECOVERY
# ============================================================
# Only use observed, full-season data for modeling
modeling_data <- pitcher_data_final %>%
  filter(!is_missing, season_type == "full_season")

cat("Fitting velocity aging model with recovery trajectories...\n")
velocity_model <- stan_lmer(
  velocity ~ age + I(age^2) + years_since_tj_capped + I(years_since_tj_capped^2) + (age | player_id),
  data = modeling_data,
  cores = 2,
  iter = 1000,
  chains = 2,
  refresh = 100)

cat("Fitting command aging model with recovery trajectories...\n")
command_model <- stan_lmer(
  command_score ~ age + I(age^2) + years_since_tj_capped + (age | player_id),
  data = modeling_data,
  cores = 2,
  iter = 1000,
  chains = 2,
  refresh = 100)

aging_models <- list(
  velocity = velocity_model,
  command = command_model)

cat("\nModels fitted successfully!\n\n")

# ============================================================
# STEP 8: CALCULATE ERA-DEPENDENT BENCHMARKS
# ============================================================

era_benchmarks <- modeling_data %>%
  mutate(
    era = case_when(
      year <= 2014 ~ "2010-2014",
      year <= 2019 ~ "2015-2019",
      TRUE ~ "2020+"
    )
  ) %>%
  group_by(era) %>%
  summarise(
    avg_velocity = mean(velocity, na.rm = TRUE),
    sd_velocity = sd(velocity, na.rm = TRUE),
    .groups = 'drop')

print(era_benchmarks)
cat("\n")

# ============================================================
# STEP 9: DATA-DRIVEN INJURY RISK BY TJ COUNT
# ============================================================

# Calculate ACTUAL decline rates for pitchers with different TJ counts
tj_risk_by_count <- pitcher_data_final %>%
  filter(!is_missing, tj_count > 0) %>%
  group_by(player_id) %>%
  summarise(
    tj_count = last(tj_count),
    had_decline = any(decline_indicator == 1, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(tj_count) %>%
  summarise(
    n_pitchers = n(),
    risk_rate = mean(had_decline, na.rm = TRUE),
    .groups = 'drop'
  )

cat("  TJ Risk by Count:\n")
print(tj_risk_by_count)

# Calculate overall TJ risk rate as fallback
tj_risk_rate_overall <- pitcher_data_final %>%
  filter(!is_missing, had_tommy_john == 1) %>%
  group_by(player_id) %>%
  summarise(had_decline = any(decline_indicator == 1, na.rm = TRUE)) %>%
  summarise(tj_risk_rate = mean(had_decline, na.rm = TRUE)) %>%
  pull(tj_risk_rate)

cat("\n  Overall TJ risk rate:", round(tj_risk_rate_overall * 100, 1), "%\n\n")

# ============================================================
# STEP 10: RISK ASSESSMENT WITH RECOVERY
# ============================================================

cat("Generating risk assessments...\n")

risk_assessments <- pitcher_data_final %>%
  filter(!is_missing) %>%
  group_by(player_id, player_name) %>%
  summarise(
    latest_year = last(year),
    current_velocity = last(velocity),
    current_command = last(command_score),
    current_age = last(age),
    career_years = max(career_year),
    recent_decline = ifelse(n() >= 2,
                            last(velocity) - nth(velocity, -2, default = last(velocity)),
                            0),
    had_tommy_john = first(had_tommy_john),
    tj_count = last(tj_count),
    years_since_tj = last(years_since_tj),
    .groups = 'drop'
  ) %>%
  # Assign era group
  mutate(
    era_group = case_when(
      latest_year <= 2014 ~ "2010-2014",
      latest_year <= 2019 ~ "2015-2019",
      TRUE ~ "2020+"
    )
  ) %>%
  # Join era benchmarks
  left_join(
    era_benchmarks %>% select(era, avg_velocity_era = avg_velocity),
    by = c("era_group" = "era")
  ) %>%
  # Join changepoint probability
  left_join(
    changepoint_results %>%
      filter(metric == "velocity") %>%
      select(player_id, changepoint_confidence),
    by = "player_id"
  ) %>%
  # Join data-driven TJ risk
  left_join(
    tj_risk_by_count %>% select(tj_count, tj_risk = risk_rate),
    by = "tj_count"
  ) %>%
  mutate(
    # Risk factors (0-1 scale)
    
    # Age risk (peak at 26)
    age_risk = pmax(0, pmin(1, (current_age - 26) / 10)),
    
    # Velocity risk (ERA-ADJUSTED)
    velocity_risk = pmax(0, pmin(1, (avg_velocity_era - current_velocity) / 4)),
    
    # Decline risk (DATA-DRIVEN threshold)
    decline_risk = pmax(0, pmin(1, -recent_decline / abs(decline_threshold))),
    
    # Changepoint risk
    changepoint_risk = ifelse(is.na(changepoint_confidence), 0, changepoint_confidence),
    
    # Injury risk (DATA-DRIVEN by TJ count, fallback to overall rate)
    injury_risk = ifelse(is.na(tj_risk), tj_count * tj_risk_rate_overall, tj_risk),
    
    # Recovery risk (year 1-2 post-TJ = high risk)
    recovery_risk = case_when(
      years_since_tj <= 2 ~ 0.6,   # Recent TJ = high risk
      years_since_tj <= 4 ~ 0.3,   # Mid-recovery = moderate
      TRUE ~ 0.0                    # 5+ years or no TJ = baseline
    ),
    
    # Overall risk (weighted combination with recovery)
    overall_risk = (age_risk * 0.18) + 
      (velocity_risk * 0.15) + 
      (decline_risk * 0.20) + 
      (changepoint_risk * 0.25) + 
      (injury_risk * 0.17) +
      (recovery_risk * 0.05),
    
    # Risk categories
    risk_category = case_when(
      overall_risk < 0.3 ~ "Low Risk",
      overall_risk < 0.6 ~ "Moderate Risk",
      TRUE ~ "High Risk"
    )
  )

cat("\nRisk Distribution:\n")
print(table(risk_assessments$risk_category))
cat("\n")

# ============================================================
# STEP 11: VALIDATION & FUTURE PREDICTIONS
# ============================================================

cat("========== MODEL VALIDATION ==========\n\n")

# Split: Train on ≤2021, Test on 2022-2023
validation_data <- modeling_data %>% filter(year <= 2023)
train_data <- validation_data %>% filter(year <= 2021)
test_data <- validation_data %>% filter(year >= 2022)

cat("Training data:", nrow(train_data), "observations\n")
cat("Test data:", nrow(test_data), "observations\n\n")

# Velocity model validation
cat("Validating velocity model...\n")
velocity_model_train <- update(velocity_model, data = train_data, refresh = 100)
predictions <- posterior_predict(velocity_model_train, newdata = test_data)
predicted_means <- apply(predictions, 2, mean)

rmse <- sqrt(mean((test_data$velocity - predicted_means)^2, na.rm = TRUE))
mae <- mean(abs(test_data$velocity - predicted_means), na.rm = TRUE)
r_squared <- cor(predicted_means, test_data$velocity, use = "complete.obs")^2

cat("\nVelocity Model Validation (2022-2023):\n")
cat("  RMSE:", round(rmse, 2), "mph\n")
cat("  MAE:", round(mae, 2), "mph\n")
cat("  R-squared:", round(r_squared, 3), "\n\n")

# Command model validation
cat("Validating command model...\n")
command_model_train <- update(command_model, data = train_data, refresh = 100)
command_predictions <- posterior_predict(command_model_train, newdata = test_data)
command_predicted_means <- apply(command_predictions, 2, mean)

command_rmse <- sqrt(mean((test_data$command_score - command_predicted_means)^2, na.rm = TRUE))
command_mae <- mean(abs(test_data$command_score - command_predicted_means), na.rm = TRUE)
command_r_squared <- cor(command_predicted_means, test_data$command_score, use = "complete.obs")^2

cat("\nCommand Model Validation (2022-2023):\n")
cat("  RMSE:", round(command_rmse, 3), "\n")
cat("  MAE:", round(command_mae, 3), "\n")
cat("  R-squared:", round(command_r_squared, 3), "\n\n")

# Create validation summary
validation_summary <- data.frame(
  model = c("velocity", "velocity", "velocity", "command", "command", "command"),
  metric = rep(c("RMSE", "MAE", "R_squared"), 2),
  value = c(rmse, mae, r_squared, command_rmse, command_mae, command_r_squared)
)

# Prediction error distribution
residuals <- test_data$velocity - predicted_means
cat("  Velocity prediction error distribution:\n")
cat("    Min:", round(min(residuals, na.rm = TRUE), 2), "\n")
cat("    Q1:", round(quantile(residuals, 0.25, na.rm = TRUE), 2), "\n")
cat("    Median:", round(median(residuals, na.rm = TRUE), 2), "\n")
cat("    Q3:", round(quantile(residuals, 0.75, na.rm = TRUE), 2), "\n")
cat("    Max:", round(max(residuals, na.rm = TRUE), 2), "\n\n")

# Calibration check
cat("  Calibration by tercile:\n")
calibration <- data.frame(
  actual = test_data$velocity,
  predicted = predicted_means
) %>%
  mutate(tercile = ntile(predicted, 3)) %>%
  group_by(tercile) %>%
  summarise(
    avg_predicted = mean(predicted),
    avg_actual = mean(actual),
    .groups = 'drop')
print(calibration)
cat("\n")

# Generate future predictions (2024-2026)
cat("Generating future predictions (2024-2026)...\n")
future_years <- 2024:2026
future_predictions <- list()

for(future_year in future_years) {
  current_pitchers <- risk_assessments %>%
    filter(latest_year == max(latest_year)) %>%
    mutate(
      year = future_year,
      age = current_age + (future_year - latest_year),
      years_since_tj_capped = pmin(years_since_tj + (future_year - latest_year), 5))
  
  future_preds <- posterior_predict(velocity_model, newdata = current_pitchers)
  
  future_predictions[[as.character(future_year)]] <- data.frame(
    player_id = current_pitchers$player_id,
    player_name = current_pitchers$player_name,
    prediction_year = future_year,
    predicted_age = current_pitchers$age,
    predicted_velocity = apply(future_preds, 2, mean),
    prediction_lower = apply(future_preds, 2, quantile, 0.025),
    prediction_upper = apply(future_preds, 2, quantile, 0.975),
    current_velocity = current_pitchers$current_velocity
  )
}

future_predictions_df <- bind_rows(future_predictions)

cat("Future predictions generated for", length(unique(future_predictions_df$player_id)), "pitchers\n\n")

# ============================================================
# STEP 12: SAVE ALL OUTPUTS
# ============================================================

output_dir <- "C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/"

saveRDS(changepoint_results, paste0(output_dir, "changepoint_results.rds"))
write_csv(changepoint_results, paste0(output_dir, "changepoint_results.csv"))

saveRDS(risk_assessments, paste0(output_dir, "risk_assessments.rds"))
write_csv(risk_assessments, paste0(output_dir, "risk_assessments.csv"))

saveRDS(aging_models, paste0(output_dir, "aging_models.rds"))

saveRDS(future_predictions_df, paste0(output_dir, "future_predictions.rds"))
write_csv(future_predictions_df, paste0(output_dir, "future_predictions.csv"))

write_csv(validation_summary, paste0(output_dir, "validation_summary.csv"))

write_csv(tj_risk_by_count, paste0(output_dir, "tj_risk_by_count.csv"))

cat("All outputs saved to:", output_dir, "\n\n")

cat("========== ANALYSIS COMPLETE! ==========\n\n")

cat("Results Summary:\n")
cat("  Changepoints detected:", nrow(changepoint_results), "\n")
cat("    - Velocity:", sum(changepoint_results$metric == "velocity"), "\n")
cat("    - Command:", sum(changepoint_results$metric == "command_score"), "\n")
cat("  Pitchers assessed:", nrow(risk_assessments), "\n")
cat("  High risk pitchers:", sum(risk_assessments$risk_category == "High Risk"), "\n")
cat("  Moderate risk pitchers:", sum(risk_assessments$risk_category == "Moderate Risk"), "\n")
cat("  Low risk pitchers:", sum(risk_assessments$risk_category == "Low Risk"), "\n")
cat("  Velocity model RMSE:", round(rmse, 2), "mph\n")
cat("  Command model RMSE:", round(command_rmse, 3), "\n")
cat("  Future predictions: 2024-2026\n\n")

cat("Top 10 Highest Risk Pitchers:\n")
print(risk_assessments %>%
        arrange(desc(overall_risk)) %>%
        select(player_name, latest_year, current_age, current_velocity, 
               tj_count, years_since_tj, overall_risk, risk_category) %>%
        head(10))

beepr::beep(8)






































# library(tidyverse)
# library(brms)
# library(rstanarm)
# library(changepoint)
# library(plotly)
# library(DT)
# library(shiny)
# library(shinydashboard)
# library(beepr)
# library(patchwork)
# library(readxl)
# library(lubridate)
# 
# set.seed(42)
# 
# 
# stats <- read_csv("C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/stats.csv")
# 
# 
# cat("  Raw stats shape:", nrow(stats), "rows,", ncol(stats), "columns\n")
# cat("  Years covered:", min(stats$year), "to", max(stats$year), "\n")
# cat("  Unique pitchers:", length(unique(stats$player_id)), "\n\n")
# 
# tj_data <- read_excel("C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/CopyofTommyJohnSurgeryList.xlsx", 
#                       skip = 1)
# 
# # Clean TJ data - keep only pitchers with MLBAM ID
# tj_pitchers <- tj_data %>%
#   filter(Position == "P", !is.na(mlbamid)) %>%
#   select(player_id = mlbamid, Player, `TJ Surgery Date`, Team, Age) %>%
#   rename(tj_surgery_date = `TJ Surgery Date`,
#          tj_age = Age) %>%
#   mutate(
#     player_id = as.integer(player_id),
#     tj_year = year(tj_surgery_date)
#   ) %>%
#   arrange(player_id, tj_surgery_date)
# 
# cat("  Total TJ surgeries for pitchers:", nrow(tj_pitchers), "\n")
# cat("  Unique pitchers with TJ:", length(unique(tj_pitchers$player_id)), "\n")
# cat("  TJ date range:", min(tj_pitchers$tj_surgery_date, na.rm = TRUE), 
#     "to", max(tj_pitchers$tj_surgery_date, na.rm = TRUE), "\n\n")
# 
# # ============================================================
# # STEP 2: CLEAN AND PREPARE STATS DATA
# # ============================================================
# 
# # Clean stats data
# pitcher_stats <- stats %>%
#   # Keep only starting pitchers with meaningful data
#   filter(
#     !is.na(ff_avg_speed),           # Must have fastball velocity
#     !is.na(player_age),              # Must have age
#     !is.na(whiff_percent),           # Must have performance metrics
#     p_formatted_ip >= 30             # At least 30 IP in the season
#   ) %>%
#   # Create clean name variable
#   mutate(
#     player_name = str_trim(`last_name, first_name`),
#     # Calculate movement proxy (using exit velocity metrics as proxy for stuff)
#     total_movement = (avg_best_speed + avg_hyper_speed) / 2,
#     # Command proxy (lower BB% = better command)
#     command_proxy = bb_percent,
#     # Rename for consistency
#     velocity = ff_avg_speed,
#     age = player_age
#   ) %>%
#   # Select relevant columns
#   select(
#     player_id, 
#     player_name,
#     year,
#     age,
#     velocity,
#     total_movement,
#     command_proxy,
#     whiff_percent,
#     k_percent,
#     bb_percent,
#     woba,
#     xwoba,
#     hard_hit_percent,
#     barrel_batted_rate,
#     p_formatted_ip
#   ) %>%
#   arrange(player_id, year)
# 
# cat("  Cleaned stats shape:", nrow(pitcher_stats), "rows\n")
# cat("  Pitchers with 3+ seasons:", 
#     sum(pitcher_stats %>% count(player_id) %>% pull(n) >= 3), "\n\n")
# 
# # ============================================================
# # STEP 3: MERGE INJURY DATA
# # ============================================================
# 
# cat("Merging injury data...\n")
# 
# # For each pitcher-year, determine if they've had TJ BEFORE that season
# pitcher_data_merged <- pitcher_stats %>%
#   left_join(
#     tj_pitchers %>%
#       select(player_id, tj_year, tj_surgery_date) %>%
#       group_by(player_id) %>%
#       # Get first TJ surgery date
#       arrange(tj_surgery_date) %>%
#       slice(1) %>%
#       ungroup(),
#     by = "player_id"
#   ) %>%
#   mutate(
#     # Did this pitcher have TJ before this season?
#     had_tommy_john = ifelse(is.na(tj_year), 0, ifelse(year > tj_year, 1, 0)),
#     # Injury history count (cumulative TJ surgeries before this season)
#     injury_history = ifelse(is.na(tj_year), 0, ifelse(year > tj_year, 1, 0))
#   )
# 
# cat("  Pitchers with TJ history:", sum(pitcher_data_merged$had_tommy_john > 0), "observations\n")
# cat("  Unique pitchers with TJ:", 
#     length(unique(pitcher_data_merged$player_id[pitcher_data_merged$had_tommy_john > 0])), "\n\n")
# 
# # ============================================================
# # STEP 4: CREATE FINAL DATASET WITH CAREER METRICS
# # ============================================================
# 
# cat("Calculating career-level metrics...\n")
# 
# pitcher_data_final <- pitcher_data_merged %>%
#   # Group by BOTH name and ID to avoid conflicts
#   group_by(player_id, player_name) %>%
#   # Only keep pitchers with 3+ seasons
#   filter(n() >= 3) %>%
#   arrange(year) %>%
#   mutate(
#     # CAREER YEAR (1, 2, 3, etc. for each pitcher)
#     career_year = row_number(),
#     
#     # Command score (higher = better, inverse of BB%)
#     command_score = 1 - (command_proxy / 100),
#     
#     # Year-over-year changes
#     velocity_change = velocity - lag(velocity, default = first(velocity)),
#     movement_change = total_movement - lag(total_movement, default = first(total_movement)),
#     command_change = command_score - lag(command_score, default = first(command_score)),
#     
#     # Stuff+ proxy (standardized velocity + movement)
#     stuff_plus = scale(velocity)[,1] + scale(total_movement)[,1],
#     
#     # Decline indicator (velocity drop > 0.5 mph)
#     decline_indicator = ifelse(velocity_change < -0.5, 1, 0),
#     
#     # Additional injury indicators (for future expansion)
#     had_shoulder_injury = 0,  # Placeholder - can add shoulder data later
#     had_elbow_injury = had_tommy_john  # TJ is elbow injury
#   ) %>%
#   ungroup() %>%
#   # Drop old command_proxy, then rename command_score
#   select(-command_proxy) %>%
#   rename(command_proxy = command_score)
# 
# cat("  Final dataset shape:", nrow(pitcher_data_final), "rows\n")
# cat("  Total pitchers:", length(unique(pitcher_data_final$player_id)), "\n")
# cat("  Average career length:", 
#     round(mean(pitcher_data_final %>% count(player_id) %>% pull(n)), 1), "seasons\n")
# cat("  Age range:", min(pitcher_data_final$age), "to", max(pitcher_data_final$age), "\n\n")
# 
# # Save intermediate data
# cat("Saving merged data...\n")
# saveRDS(pitcher_data_final, 
#         "C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/pitcher_data_merged_with_injuries.rds")
# 
# write_csv(pitcher_data_final, 
#           "C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/pitcher_data_merged_with_injuries.csv")
# 
# cat("✓ Data preparation complete!\n\n")
# 
# # ============================================================
# # STEP 5: RUN MODELS (Same as before)
# # ============================================================
# 
# # --- CHANGEPOINT DETECTION FUNCTION ---
# detect_changepoints <- function(pitcher_data, metric = "velocity") {
#   changepoint_results <- list()
#   unique_pitcher_ids <- unique(pitcher_data$player_id)
#   
#   cat("Detecting changepoints for", length(unique_pitcher_ids), "pitchers...\n")
#   pb <- txtProgressBar(min = 0, max = length(unique_pitcher_ids), style = 3)
#   
#   for(i in seq_along(unique_pitcher_ids)) {
#     pitcher_id <- unique_pitcher_ids[i]
#     pitcher_subset <- pitcher_data %>% 
#       filter(player_id == pitcher_id) %>%
#       arrange(year)
#     
#     pitcher_name <- first(pitcher_subset$player_name)
#     
#     # Requires at least 4 seasons
#     if(nrow(pitcher_subset) >= 4) {
#       ts_data <- pitcher_subset[[metric]]
#       
#       tryCatch({
#         cpt_result <- cpt.mean(ts_data, method = "BinSeg", Q = 1, penalty = "BIC")
#         changepoint_index <- cpts(cpt_result)
#         
#         if(length(changepoint_index) > 0) {
#           changepoint_index <- changepoint_index[1]
#           
#           if(changepoint_index < length(ts_data)) {
#             actual_year <- pitcher_subset$year[changepoint_index]
#             
#             pre_decline_mean <- mean(ts_data[1:changepoint_index], na.rm = TRUE)
#             post_decline_mean <- mean(ts_data[(changepoint_index+1):length(ts_data)], na.rm = TRUE)
#             decline_magnitude <- pre_decline_mean - post_decline_mean
#             
#             if(decline_magnitude > 0.5) {
#               sd_pre <- sd(ts_data[1:changepoint_index], na.rm = TRUE)
#               sd_post <- sd(ts_data[(changepoint_index+1):length(ts_data)], na.rm = TRUE)
#               pooled_sd <- sqrt((sd_pre^2 + sd_post^2) / 2)
#               
#               effect_size <- abs(decline_magnitude) / (pooled_sd + 0.01)
#               changepoint_prob <- pmin(0.95, pmax(0.5, effect_size / 3))
#               
#               changepoint_results[[as.character(pitcher_id)]] <- data.frame(
#                 player_id = pitcher_id,
#                 pitcher = pitcher_name,
#                 changepoint_year = actual_year,
#                 changepoint_probability = changepoint_prob,
#                 pre_decline_performance = pre_decline_mean,
#                 post_decline_performance = post_decline_mean,
#                 decline_magnitude = decline_magnitude,
#                 career_years_to_decline = changepoint_index,
#                 metric = metric
#               )
#             }
#           }
#         }
#       }, error = function(e) NULL)
#     }
#     setTxtProgressBar(pb, i)
#   }
#   
#   close(pb)
#   return(bind_rows(changepoint_results))
# }
# 
# # --- HIERARCHICAL BAYESIAN MODEL FUNCTION ---
# fit_hierarchical_aging_model <- function(data, metric = "velocity", injury_vars = TRUE) {
#   
#   cat("\nFitting hierarchical model for", metric, "...\n")
#   
#   if(injury_vars && "injury_history" %in% names(data)) {
#     formula_str <- paste0(metric, " ~ age + I(age^2) + injury_history + 
#                           had_tommy_john + had_shoulder_injury + (age | player_id)")
#   } else {
#     formula_str <- paste0(metric, " ~ age + I(age^2) + (age | player_id)")
#   }
#   
#   aging_model <- stan_lmer(
#     as.formula(formula_str),
#     data = data,
#     cores = 4,
#     iter = 2000,
#     chains = 4
#   )
#   
#   return(aging_model)
# }
# 
# # --- RISK ASSESSMENT FUNCTION ---
# generate_risk_assessment <- function(changepoint_data, aging_models, current_data) {
#   risk_scores <- current_data %>%
#     group_by(player_id, player_name) %>%
#     summarise(
#       current_velocity = last(velocity),
#       current_movement = last(total_movement),
#       current_command = last(command_proxy),
#       current_age = last(age),  # Already have this!
#       career_years = max(career_year), 
#       recent_decline = last(velocity) - nth(velocity, -2, default = last(velocity)),
#       injury_history = first(injury_history),
#       had_tommy_john = first(had_tommy_john),
#       had_shoulder_injury = first(had_shoulder_injury),
#       had_elbow_injury = first(had_elbow_injury),
#       .groups = 'drop'
#     ) %>%
#     left_join(
#       changepoint_data %>% 
#         filter(metric == "velocity") %>% 
#         select(player_id, changepoint_probability),
#       by = "player_id"
#     ) %>%
#     mutate(
#       # Risk factors (0-1 scale)
#       age_risk = pmax(0, pmin(1, (current_age - 28) / 10)),  # ← Age-based!
#       velocity_risk = pmax(0, pmin(1, (93 - current_velocity) / 6)),
#       decline_risk = pmax(0, pmin(1, -recent_decline / 2.5)),
#       changepoint_risk = ifelse(is.na(changepoint_probability), 0, changepoint_probability),
#       injury_risk = pmin(1, (injury_history * 0.15 + had_tommy_john * 0.25 + 
#                                had_shoulder_injury * 0.2 + had_elbow_injury * 0.15)),
#       
#       # Overall risk score
#       overall_risk = (age_risk * 0.2 + velocity_risk * 0.15 + decline_risk * 0.2 + 
#                         changepoint_risk * 0.25 + injury_risk * 0.2),
#       
#       # Risk categories
#       risk_category = case_when(
#         overall_risk < 0.3 ~ "Low Risk",
#         overall_risk < 0.6 ~ "Moderate Risk", 
#         TRUE ~ "High Risk"
#       )
#     )
#   
#   return(risk_scores)
# }
# 
# # --- VALIDATION FUNCTION ---
# validate_model_performance <- function(historical_data, model) {
#   validation_years <- unique(historical_data$year)
#   
#   if(length(validation_years) < 4) {
#     return(list(rmse = NA, mae = NA, calibration = NA))
#   }
#   
#   cutoff_year <- validation_years[length(validation_years) - 2]
#   
#   train_data <- historical_data %>% filter(year <= cutoff_year)
#   test_data <- historical_data %>% filter(year > cutoff_year)
#   
#   if(nrow(test_data) < 10) {
#     return(list(rmse = NA, mae = NA, calibration = NA))
#   }
#   
#   tryCatch({
#     validation_model <- update(model, newdata = train_data, refresh = 0)
#     predictions <- posterior_predict(validation_model, newdata = test_data)
#     predicted_means <- apply(predictions, 2, mean)
#     
#     rmse <- sqrt(mean((test_data$velocity - predicted_means)^2, na.rm = TRUE))
#     mae <- mean(abs(test_data$velocity - predicted_means), na.rm = TRUE)
#     calibration <- cor(predicted_means, test_data$velocity, use = "complete.obs")
#     
#     return(list(rmse = rmse, mae = mae, calibration = calibration))
#   }, error = function(e) {
#     return(list(rmse = NA, mae = NA, calibration = NA))
#   })
# }
# 
# # ============================================================
# # STEP 6: RUN THE MODELS
# # ============================================================
# 
# cat("\n========== RUNNING AGING & DECLINE MODELS ==========\n\n")
# 
# cat("Step 1: Detecting changepoints...\n")
# changepoint_velocity <- detect_changepoints(pitcher_data_final, "velocity")
# changepoint_movement <- detect_changepoints(pitcher_data_final, "total_movement")
# changepoint_command <- detect_changepoints(pitcher_data_final, "command_proxy")
# 
# changepoint_results <- bind_rows(changepoint_velocity, changepoint_movement, changepoint_command)
# 
# cat("\nStep 2: Fitting hierarchical aging models (15-20 minutes)...\n")
# velocity_model <- fit_hierarchical_aging_model(pitcher_data_final, "velocity", injury_vars = TRUE)
# movement_model <- fit_hierarchical_aging_model(pitcher_data_final, "total_movement", injury_vars = TRUE)
# command_model <- fit_hierarchical_aging_model(pitcher_data_final, "command_proxy", injury_vars = FALSE)
# 
# aging_models <- list(
#   velocity = velocity_model,
#   movement = movement_model,
#   command = command_model
# )
# 
# cat("\nStep 3: Generating risk assessments...\n")
# risk_assessments <- generate_risk_assessment(changepoint_results, aging_models, pitcher_data_final)
# 
# cat("\nStep 4: Validating models...\n")
# validation_velocity <- validate_model_performance(pitcher_data_final, velocity_model)
# 
# # ============================================================
# # STEP 7: RESULTS & VISUALIZATION
# # ============================================================
# 
# cat("\n========== MODEL RESULTS ==========\n\n")
# 
# cat("Velocity Model Validation:\n")
# cat("  RMSE:", round(validation_velocity$rmse, 2), "MPH\n")
# cat("  MAE:", round(validation_velocity$mae, 2), "MPH\n")
# cat("  Correlation:", round(validation_velocity$calibration, 3), "\n\n")
# 
# cat("Changepoint Detection Summary:\n")
# cat("  Velocity declines detected:", sum(changepoint_results$metric == "velocity"), "pitchers\n")
# cat("  Movement declines detected:", sum(changepoint_results$metric == "total_movement"), "pitchers\n")
# cat("  Command declines detected:", sum(changepoint_results$metric == "command_proxy"), "pitchers\n")
# cat("  Average years to decline:", round(mean(changepoint_results$career_years_to_decline, na.rm = TRUE), 1), "\n")
# cat("  Average decline magnitude:", round(mean(changepoint_results$decline_magnitude, na.rm = TRUE), 2), "\n\n")
# 
# cat("Risk Distribution:\n")
# print(table(risk_assessments$risk_category))
# 
# cat("\n\nHigh Risk Pitchers (Top 10):\n")
# print(head(risk_assessments %>% 
#              arrange(desc(overall_risk)) %>% 
#              select(player_name, player_id, current_velocity, current_age, 
#                     career_years, overall_risk, risk_category), 10))
# 
# cat("\n\nPitchers with TJ History - Risk Assessment:\n")
# tj_risk <- risk_assessments %>%
#   filter(had_tommy_john == 1) %>%
#   arrange(desc(overall_risk)) %>%
#   select(player_name, player_id, current_velocity, current_age, 
#          career_years, overall_risk, risk_category)
# 
# print(head(tj_risk, 10))
# 
# # ============================================================
# # STEP 8: SAVE RESULTS
# # ============================================================
# 
# cat("\n\nSaving models and results...\n")
# output_dir <- "C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/"
# 
# saveRDS(aging_models, paste0(output_dir, "aging_models.rds"))
# saveRDS(pitcher_data_final, paste0(output_dir, "pitcher_data_final.rds"))
# saveRDS(changepoint_results, paste0(output_dir, "changepoint_results.rds"))
# saveRDS(risk_assessments, paste0(output_dir, "risk_assessments.rds"))
# 
# write_csv(changepoint_results, paste0(output_dir, "changepoint_results.csv"))
# write_csv(risk_assessments, paste0(output_dir, "risk_assessments.csv"))
# 
# cat("\n========== MODEL COMPLETE ==========\n")
# beepr::beep(8)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # library(tidyverse)
# # library(brms)
# # library(rstanarm)
# # library(changepoint)
# # library(plotly)
# # library(DT)
# # library(shiny)
# # library(shinydashboard)
# # library(beepr)
# # library(patchwork)
# # 
# # 
# # # data <- read_csv("savant_data_aging.csv")
# # # names(data)
# # set.seed(42)
# # 
# # data_path <- "C:/Users/Owner/Desktop/WakeForest/WFUBaseball/AgingModel/pitcher_data_for_model_real_tj.csv"
# # pitcher_data_raw <- read_csv(data_path)
# # 
# # 
# # # --- 1. Data Preparation Function ---
# # # This function calculates year-over-year changes and final features needed for the models.
# # 
# # prepare_pitcher_data <- function(data) {
# #   pitcher_careers <- data %>%
# #     group_by(player_name) %>%
# #     filter(n() >= 3) %>%  #only include pitchers with at least 3 years of career data
# #     arrange(year) %>%
# #     mutate(
# #       # CRITICAL: Create career_year variable (1, 2, 3, etc. for each pitcher)
# #       career_year = row_number(),
# #       
# #       # Command Proxy Conversion: Convert the loaded 'command_proxy' (which is BB% in the 
# #       # pre-processed data) to a 'command_score' where a higher value is better command.
# #       command_score = 1 - (command_proxy / 100),
# #       
# #       # Time-Series Change Metrics (Year-over-Year Decline/Improvement)
# #       # lag() calculates the value from the previous row within the current grouping (pitcher).
# #       velocity_change = velocity - lag(velocity, default = first(velocity)),
# #       movement_change = total_movement - lag(total_movement, default = first(total_movement)),
# #       command_change = command_score - lag(command_score, default = first(command_score)),
# #       
# #       # Simplified Stuff+ proxy: Standardized velocity plus standardized movement.
# #       stuff_plus = scale(velocity)[,1] + scale(total_movement)[,1],
# #       
# #       # Binary indicator for significant year-over-year velocity decline (e.g., > 0.5 MPH)
# #       decline_indicator = ifelse(velocity_change < -0.5, 1, 0)
# #     ) %>%
# #     ungroup()
# #   
# #   # Rename for consistency with original script variables used in other functions
# #   pitcher_careers <- pitcher_careers %>% rename(command_proxy = command_score)
# #   
# #   return(pitcher_careers)
# # }
# # 
# # 
# # # --- 2. Change Point Detection Function (Using changepoint package) ---
# # detect_changepoints <- function(pitcher_data, metric = "velocity") {
# #   changepoint_results <- list()
# #   unique_pitchers <- unique(pitcher_data$player_name)
# #   
# #   cat("Detecting changepoints for", length(unique_pitchers), "pitchers...\n")
# #   pb <- txtProgressBar(min = 0, max = length(unique_pitchers), style = 3)
# #   
# #   for(i in seq_along(unique_pitchers)) {
# #     pitcher <- unique_pitchers[i]
# #     pitcher_subset <- pitcher_data %>% 
# #       filter(player_name == pitcher) %>%
# #       arrange(year)
# #     
# #     # Requires at least 4 seasons to detect a changepoint
# #     if(nrow(pitcher_subset) >= 4) {
# #       ts_data <- pitcher_subset[[metric]]
# #       
# #       tryCatch({
# #         # Use changepoint::cpt.mean() for changepoint detection
# #         # This detects changes in the mean of the series
# #         cpt_result <- cpt.mean(ts_data, method = "BinSeg", Q = 1, penalty = "BIC")
# #         
# #         # Get the changepoint location
# #         changepoint_index <- cpts(cpt_result)
# #         
# #         # If a changepoint was detected
# #         if(length(changepoint_index) > 0) {
# #           changepoint_index <- changepoint_index[1]  # Take the first one if multiple
# #           
# #           # Only proceed if the changepoint is not at the very end
# #           if(changepoint_index < length(ts_data)) {
# #             actual_year <- pitcher_subset$year[changepoint_index]
# #             
# #             # Calculate pre- and post-decline means
# #             pre_decline_mean <- mean(ts_data[1:changepoint_index], na.rm = TRUE)
# #             post_decline_mean <- mean(ts_data[(changepoint_index+1):length(ts_data)], na.rm = TRUE)
# #             
# #             # Calculate decline magnitude
# #             decline_magnitude <- pre_decline_mean - post_decline_mean
# #             
# #             # Only include if there's a meaningful decline (> 0.5 units)
# #             if(decline_magnitude > 0.5) {
# #               # Estimate confidence based on the size of the change relative to variance
# #               sd_pre <- sd(ts_data[1:changepoint_index], na.rm = TRUE)
# #               sd_post <- sd(ts_data[(changepoint_index+1):length(ts_data)], na.rm = TRUE)
# #               pooled_sd <- sqrt((sd_pre^2 + sd_post^2) / 2)
# #               
# #               # Simple confidence measure: larger effect sizes = higher confidence
# #               # This is a pseudo-probability that mimics what bcp would have given us
# #               effect_size <- abs(decline_magnitude) / (pooled_sd + 0.01)  # Add small constant to avoid division by zero
# #               changepoint_prob <- pmin(0.95, pmax(0.5, effect_size / 3))
# #               
# #               changepoint_results[[pitcher]] <- data.frame(
# #                 pitcher = pitcher,
# #                 changepoint_year = actual_year,
# #                 changepoint_probability = changepoint_prob,
# #                 pre_decline_performance = pre_decline_mean,
# #                 post_decline_performance = post_decline_mean,
# #                 decline_magnitude = decline_magnitude,
# #                 career_years_to_decline = changepoint_index,
# #                 metric = metric
# #               )
# #             }
# #           }
# #         }
# #       }, error = function(e) {
# #         # Catch errors from changepoint function
# #         NULL
# #       })
# #     }
# #     setTxtProgressBar(pb, i)
# #   }
# #   
# #   close(pb)
# #   return(bind_rows(changepoint_results))
# # }
# # 
# # 
# # # --- 3. Hierarchical Bayesian Modeling Function (brms) ---
# # # UPDATED WITH REALISTIC PRIORS
# # fit_hierarchical_aging_model <- function(data, metric = "velocity", injury_vars = TRUE) {
# #   
# #   cat("\nFitting hierarchical Bayesian model for", metric, "...\n")
# #   
# #   # Construct the regression formula dynamically
# #   if(injury_vars && "injury_history" %in% names(data)) {
# #     # Full model with quadratic aging curve, injury terms, and random effects for pitcher
# #     formula_str <- paste0(metric, " ~ career_year + I(career_year^2) + injury_history + 
# #                           had_tommy_john + had_shoulder_injury + (career_year | player_name)")
# #   } else {
# #     # Simpler model without injury variables
# #     formula_str <- paste0(metric, " ~ career_year + I(career_year^2) + (career_year | player_name)")
# #   }
# #   
# #   # UPDATED PRIORS - More realistic for modern MLB (2015-2024)
# #   aging_model <- brm(
# #     as.formula(formula_str),
# #     data = data,
# #     family = gaussian(),
# #     prior = c(
# #       # Intercept: Average velocity for a pitcher in their FIRST year
# #       # Modern MLB: 94-95 MPH for starting pitchers
# #       prior(normal(94.5, 3), class = Intercept), 
# #       
# #       # Linear term: Initially slight decline, allows for individual variation
# #       prior(normal(-0.3, 0.3), class = b, coef = career_year),
# #       
# #       # Quadratic term: NEGATIVE (decline accelerates over time)
# #       # This creates the "stable early, then accelerating decline" pattern
# #       prior(normal(-0.05, 0.05), class = b, coef = Icareer_year2),
# #       
# #       # Standard deviations: Exponential priors (positive values)
# #       prior(exponential(0.5), class = sd),
# #       prior(exponential(0.5), class = sigma)
# #     ),
# #     chains = 4,
# #     iter = 2000,
# #     cores = 4,
# #     control = list(adapt_delta = 0.95),  # Helps with convergence
# #     silent = 2,
# #     refresh = 0
# #   )
# #   
# #   return(aging_model)
# # }
# # 
# # # --- 4. Future Decline Probability Calculation ---
# # calculate_decline_probabilities <- function(model, data, pitcher_name, future_years = 3) {
# #   pitcher_data <- data %>% filter(player_name == pitcher_name)
# #   
# #   if(nrow(pitcher_data) == 0) {
# #     return(NULL)
# #   }
# #   
# #   current_career_year <- max(pitcher_data$career_year)
# #   
# #   # Create data frame for prediction years
# #   future_data <- data.frame(
# #     career_year = (current_career_year + 1):(current_career_year + future_years),
# #     player_name = pitcher_name
# #   )
# #   
# #   # Carry forward the pitcher's injury status
# #   if("injury_history" %in% names(pitcher_data)) {
# #     future_data$injury_history <- first(pitcher_data$injury_history)
# #     future_data$had_tommy_john <- first(pitcher_data$had_tommy_john)
# #     future_data$had_shoulder_injury <- first(pitcher_data$had_shoulder_injury)
# #   }
# #   
# #   # Generate posterior predictive samples
# #   predictions <- posterior_predict(model, newdata = future_data, allow_new_levels = FALSE)
# #   
# #   # Get the most recent performance value as baseline
# #   current_performance <- tail(pitcher_data[[all.vars(formula(model))[1]]], 1)
# #   
# #   # Calculate probability of decline > 1 unit
# #   decline_probs <- apply(predictions, 2, function(x) {
# #     mean(x < current_performance - 1)  
# #   })
# #   
# #   return(data.frame(
# #     player_name = pitcher_name,
# #     future_career_year = future_data$career_year,
# #     predicted_mean = apply(predictions, 2, mean),
# #     decline_probability = decline_probs,
# #     uncertainty_lower_10 = apply(predictions, 2, quantile, 0.1),
# #     uncertainty_upper_90 = apply(predictions, 2, quantile, 0.9)
# #   ))
# # }
# # 
# # 
# # # --- 5. Risk Assessment Function ---
# # generate_risk_assessment <- function(changepoint_data, aging_models, current_data) {
# #   risk_scores <- current_data %>%
# #     group_by(player_name) %>%
# #     summarise(
# #       current_velocity = last(velocity),
# #       current_movement = last(total_movement),
# #       current_command = last(command_proxy),
# #       career_years = max(career_year), 
# #       recent_decline = last(velocity) - nth(velocity, -2, default = last(velocity)),
# #       injury_history = first(injury_history),
# #       had_tommy_john = first(had_tommy_john),
# #       had_shoulder_injury = first(had_shoulder_injury),
# #       had_elbow_injury = first(had_elbow_injury),
# #       .groups = 'drop'
# #     ) %>%
# #     left_join(
# #       changepoint_data %>% filter(metric == "velocity") %>% select(pitcher, changepoint_probability),
# #       by = c("player_name" = "pitcher")
# #     ) %>%
# #     mutate(
# #       # Risk factors (0-1 scale)
# #       age_risk = pmax(0, pmin(1, (career_years - 6) / 9)),
# #       velocity_risk = pmax(0, pmin(1, (93 - current_velocity) / 6)),
# #       decline_risk = pmax(0, pmin(1, -recent_decline / 2.5)),
# #       changepoint_risk = ifelse(is.na(changepoint_probability), 0, changepoint_probability),
# #       injury_risk = pmin(1, (injury_history * 0.15 + had_tommy_john * 0.25 + 
# #                                had_shoulder_injury * 0.2 + had_elbow_injury * 0.15)),
# #       
# #       # Overall risk score (weighted average)
# #       overall_risk = (age_risk * 0.2 + velocity_risk * 0.15 + decline_risk * 0.2 + 
# #                         changepoint_risk * 0.25 + injury_risk * 0.2),
# #       
# #       # Risk categories
# #       risk_category = case_when(
# #         overall_risk < 0.3 ~ "Low Risk",
# #         overall_risk < 0.6 ~ "Moderate Risk", 
# #         TRUE ~ "High Risk"
# #       )
# #     )
# #   
# #   return(risk_scores)
# # }
# # 
# # # --- 6. Model Validation Function ---
# # validate_model_performance <- function(historical_data, model) {
# #   validation_years <- unique(historical_data$year)
# #   
# #   if(length(validation_years) < 4) {
# #     return(list(rmse = NA, mae = NA, calibration = NA))
# #   }
# #   
# #   cutoff_year <- validation_years[length(validation_years) - 2]
# #   
# #   train_data <- historical_data %>% filter(year <= cutoff_year)
# #   test_data <- historical_data %>% filter(year > cutoff_year)
# #   
# #   if(nrow(test_data) < 10) {
# #     return(list(rmse = NA, mae = NA, calibration = NA))
# #   }
# #   
# #   tryCatch({
# #     validation_model <- update(model, newdata = train_data, refresh = 0)
# #     predictions <- posterior_predict(validation_model, newdata = test_data)
# #     predicted_means <- apply(predictions, 2, mean)
# #     
# #     rmse <- sqrt(mean((test_data$velocity - predicted_means)^2, na.rm = TRUE))
# #     mae <- mean(abs(test_data$velocity - predicted_means), na.rm = TRUE)
# #     calibration <- cor(predicted_means, test_data$velocity, use = "complete.obs")
# #     
# #     return(list(
# #       rmse = rmse,
# #       mae = mae,
# #       calibration = calibration
# #     ))
# #   }, error = function(e) {
# #     return(list(rmse = NA, mae = NA, calibration = NA))
# #   })
# # }
# # 
# # # ========== MAIN EXECUTION BLOCK ==========
# # 
# # cat("========== BAYESIAN PITCHER AGING & DECLINE DETECTION MODEL ==========\n\n")
# # 
# # cat("Step 1: Loading and preparing data...\n")
# # pitcher_data <- prepare_pitcher_data(pitcher_data_raw) 
# # 
# # cat("Total pitchers:", length(unique(pitcher_data$player_name)), "\n")
# # cat("Total observations:", nrow(pitcher_data), "\n\n")
# # 
# # cat("Step 2: Detecting changepoints...\n")
# # changepoint_velocity <- detect_changepoints(pitcher_data, "velocity")
# # changepoint_movement <- detect_changepoints(pitcher_data, "total_movement")
# # changepoint_command <- detect_changepoints(pitcher_data, "command_proxy")
# # 
# # changepoint_results <- bind_rows(changepoint_velocity, changepoint_movement, changepoint_command)
# # 
# # cat("\nStep 3: Fitting hierarchical aging models (This will take 15-20 minutes)...\n")
# # cat("NOTE: Updated priors for modern MLB:\n")
# # cat("  - Starting velocity: ~94.5 MPH\n")
# # cat("  - Decline pattern: Stable early career, accelerating after year 6-7\n\n")
# # 
# # velocity_model <- fit_hierarchical_aging_model(pitcher_data, "velocity", injury_vars = TRUE)
# # movement_model <- fit_hierarchical_aging_model(pitcher_data, "total_movement", injury_vars = TRUE)
# # command_model <- fit_hierarchical_aging_model(pitcher_data, "command_proxy", injury_vars = FALSE)
# # 
# # aging_models <- list(
# #   velocity = velocity_model,
# #   movement = movement_model,
# #   command = command_model
# # )
# # 
# # cat("\nStep 4: Generating risk assessments...\n")
# # risk_assessments <- generate_risk_assessment(changepoint_results, aging_models, pitcher_data)
# # 
# # cat("\nStep 5: Validating models...\n")
# # validation_velocity <- validate_model_performance(pitcher_data, velocity_model)
# # 
# # cat("\n========== MODEL RESULTS ==========\n\n")
# # 
# # cat("Velocity Model Validation (Predictive Accuracy on Held-Out Data):\n")
# # cat("  RMSE:", round(validation_velocity$rmse, 2), "MPH\n")
# # cat("  MAE:", round(validation_velocity$mae, 2), "MPH\n")
# # cat("  Correlation:", round(validation_velocity$calibration, 3), "\n\n")
# # 
# # cat("Changepoint Detection Summary:\n")
# # cat("  Velocity declines detected:", sum(changepoint_results$metric == "velocity"), "pitchers\n")
# # cat("  Movement declines detected:", sum(changepoint_results$metric == "total_movement"), "pitchers\n")
# # cat("  Command declines detected:", sum(changepoint_results$metric == "command_proxy"), "pitchers\n")
# # cat("  Average years to decline:", round(mean(changepoint_results$career_years_to_decline, na.rm = TRUE), 1), "\n")
# # cat("  Average decline magnitude:", round(mean(changepoint_results$decline_magnitude, na.rm = TRUE), 2), "\n\n")
# # 
# # cat("Risk Distribution:\n")
# # print(table(risk_assessments$risk_category))
# # 
# # cat("\n\nHigh Risk Pitchers (Top 10):\n")
# # print(head(risk_assessments %>% 
# #              arrange(desc(overall_risk)) %>% 
# #              select(player_name, current_velocity, career_years, overall_risk, risk_category), 10))
# # 
# # cat("\n\nSaving models and results...\n")
# # saveRDS(aging_models, "C:/Users/Owner/Desktop/WakeForest/WFUBaseball/AgingModel/aging_models.rds")
# # saveRDS(pitcher_data, "C:/Users/Owner/Desktop/WakeForest/WFUBaseball/AgingModel/pitcher_data.rds")
# # saveRDS(changepoint_results, "C:/Users/Owner/Desktop/WakeForest/WFUBaseball/AgingModel/changepoint_results.rds")
# # saveRDS(risk_assessments, "C:/Users/Owner/Desktop/WakeForest/WFUBaseball/AgingModel/risk_assessments.rds")
# # 
# # cat("\n========== MODEL COMPLETE ==========\n")
# # beepr::beep(8)