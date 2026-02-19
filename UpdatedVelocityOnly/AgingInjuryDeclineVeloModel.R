library(tidyverse)
library(rstanarm)   #bayesian modeling engine (wraps stan for R essentially)
library(changepoint)   #PELT algorithm for detecting structural breaks
library(beepr)
library(readxl)
library(lubridate)
library(reshape2)
set.seed(42)

output_dir <- "C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/UpdatedVelocityOnly/"
stats <- read_csv("C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/stats.csv")
tj_data <- read_excel("C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/CopyofTommyJohnSurgeryList.xlsx", skip = 1)

cat("Raw stats shape:", nrow(stats), "rows,", ncol(stats), "columns\n")
cat("Years covered:", min(stats$year), "to", max(stats$year), "\n")
cat("Unique pitchers:", length(unique(stats$player_id)), "\n\n")


# Process TJ data - keep all surgeries per pitcher
tj_pitchers_all <- tj_data %>%
  filter(Position == "P", !is.na(mlbamid)) %>%
  select(player_id = mlbamid, Player, `TJ Surgery Date`, Team, Age) %>%
  rename(tj_surgery_date = `TJ Surgery Date`, tj_age = Age) %>%
  mutate(
    player_id = as.integer(player_id),
    tj_year = year(tj_surgery_date)
  ) %>%
  arrange(player_id, tj_surgery_date)

#create TJ summary with surgery number for each pitcher
tj_summary <- tj_pitchers_all %>%
  group_by(player_id) %>%
  mutate(tj_number = row_number()) %>%
  ungroup() %>%
  select(player_id, tj_year, tj_number)

cat("Total TJ surgeries for pitchers:", nrow(tj_pitchers_all), "\n")
cat("Unique pitchers with TJ:", length(unique(tj_pitchers_all$player_id)), "\n")
cat("TJ date range:", min(tj_pitchers_all$tj_surgery_date, na.rm = TRUE), "to", max(tj_pitchers_all$tj_surgery_date, na.rm = TRUE), "\n\n")



#Cleaning and filtering the stats data
pitcher_stats <- stats %>%
  filter(
    !is.na(ff_avg_speed),                      # Must have fastball velocity
    !is.na(player_age),                        # Must have age
    ff_avg_speed >= 85 & ff_avg_speed <= 105  # Remove velocity outliers
  ) %>%
  mutate(
    player_name = str_trim(`last_name, first_name`),
    velocity = ff_avg_speed,
    age = player_age,
    # Track season quality
    season_type = case_when(
      p_formatted_ip >= 30 ~ "full_season",
      p_formatted_ip > 0 & p_formatted_ip < 30 ~ "partial_season",TRUE ~ "no_data")
  ) %>%
  select(
    player_id, player_name, year, age, velocity, season_type, p_formatted_ip
  ) %>%
  arrange(player_id, year)

cat("Cleaned stats shape:", nrow(pitcher_stats), "rows\n")
cat("Season types:\n")
print(table(pitcher_stats$season_type))
cat("\n")

#Merging Injury data and calculating TJ metrics
pitcher_data_merged <- pitcher_stats %>%
  left_join(
    tj_summary %>%
      expand_grid(year = min(pitcher_stats$year):max(pitcher_stats$year)) %>%
      filter(year > tj_year) %>%  # Only years AFTER TJ surgery
      group_by(player_id, year) %>%
      summarise(
        tj_count = n(),
        most_recent_tj_year = max(tj_year), 
        .groups = 'drop'),
    by = c("player_id", "year")
  ) %>%
  mutate(
    tj_count = replace_na(tj_count, 0),
    had_tommy_john = as.numeric(tj_count > 0)
    )
cat("Pitchers with TJ history:", length(unique(pitcher_data_merged$player_id[pitcher_data_merged$had_tommy_john > 0])), "\n\n")


# handling missing seasons and identifying gaps
# Complete year sequences for each pitcher (minimum 3 seasons)
pitcher_data_complete <- pitcher_data_merged %>%
  group_by(player_id) %>%
  filter(n() >= 3) %>%
  complete(year = min(year):max(year)) %>%
  ungroup()

# Fill pitcher info and classify gap types
pitcher_data_complete <- pitcher_data_complete %>%
  group_by(player_id) %>%
  fill(player_name, .direction = "downup") %>%
  mutate(
    is_missing = is.na(velocity),
    # Use tj_count to detect injury years
    tj_count_filled = replace_na(tj_count, 0),
    tj_this_year = tj_count_filled > lag(tj_count_filled, default = 0),
    is_injury_year = tj_this_year,
    
    # Classify gap type
    gap_type = case_when(
      !is_missing ~ "data_present",
      is_injury_year ~ "injury_related",
      TRUE ~ "other_absence"
    ),
    
    # Update had_tommy_john for filled rows
    had_tommy_john = as.numeric(tj_count_filled > 0)
  ) %>%
  select(-tj_this_year, -tj_count_filled) %>%
  ungroup()

cat("Total rows after filling gaps:", nrow(pitcher_data_complete), "\n")
cat("Gap types:\n")
print(table(pitcher_data_complete$gap_type))
cat("\n")





# establish/calcualte the decline threshold
# calibrating my definition of "abnormal decline" to the actual distribution of normal velocity changes in the MLB. Finding the SD and set the threshold to 2 SD's.
#used later for "decline_indicator", flags pitcher seasons where velo dropped more than expected
# used as 1 of the 6 components of the risk score
normal_changes <- pitcher_data_complete %>%
  filter(!is_missing, !is_injury_year) %>%
  group_by(player_id) %>%
  arrange(year) %>%
  mutate(velocity_change = velocity - lag(velocity)) %>%
  ungroup() %>%
  filter(
    !is.na(velocity_change),
    velocity_change > -10 & velocity_change < 5
  ) %>%
  pull(velocity_change)

decline_threshold <- -2 * sd(normal_changes, na.rm = TRUE)
cat("Decline threshold:", round(decline_threshold, 2), "mph\n\n")

# Add career metrics and calculate years since TJ
pitcher_data_final <- pitcher_data_complete %>%
  group_by(player_id, player_name) %>%
  arrange(year) %>%
  mutate(
    career_year = year - min(year) + 1,
    #quantification of TJ effects, compute gap between current year and their most recent surgery year (where had_tommy_john = 1)
    years_since_tj = ifelse(
      had_tommy_john == 1 & !is.na(most_recent_tj_year),
      year - most_recent_tj_year,
      NA
    ),
    #feeds into the bayes model, capping effects at 5yrs
    years_since_tj_capped = ifelse(is.na(years_since_tj), 0, pmin(years_since_tj, 5)),
    #handling for consecutive calendar gaps rather than observed seasons due to variety of possible factors
    years_since_last = ifelse(!is_missing, year - lag(year[!is_missing]), NA),
    
    # Velocity change per year annualized by dividing by year_since_last:
    #without this, a player who lost 3mph over a 3yr absence would look the same as a guy who lost 3mph in one year
    velocity_change = ifelse(
      !is_missing & !is.na(lag(velocity[!is_missing])),
      (velocity - lag(velocity[!is_missing])) / years_since_last,
      NA
    ),
    
    # binary decline indicator: if annualized velo change exceeds half the decline threshold (0 otherwise)
    decline_indicator = ifelse(
      !is.na(velocity_change) & velocity_change < decline_threshold / 2,
      1, 0
    )
  ) %>%
  ungroup()

cat("Final dataset shape:", nrow(pitcher_data_final), "rows\n")
cat("Total pitchers:", length(unique(pitcher_data_final$player_id)), "\n\n")
saveRDS(pitcher_data_final, paste0(output_dir, "pitcher_data_final_velo_only.rds"))



#Changepoint detection
detect_changepoints <- function(pitcher_data) {
  changepoint_results <- list()
  
  # Only analyze pitchers with minimum+ observed data (4+ seasons)
  pitcher_data_observed <- pitcher_data %>%
    filter(!is_missing) %>%
    group_by(player_id) %>%
    filter(n() >= 4) %>%
    ungroup()
  
  unique_pitchers <- unique(pitcher_data_observed$player_id)
  total_pitchers <- length(unique_pitchers)
  for(i in seq_along(unique_pitchers)) {
    pid <- unique_pitchers[i]
    
    pitcher_subset <- pitcher_data_observed %>%
      filter(player_id == pid) %>%
      arrange(year)
    
    if(nrow(pitcher_subset) >= 4) {
      tryCatch({
        # Detect changepoints in velocity time series
        # cpt.mean, cpt.var, cpt.meanvar
        cpt_result <- cpt.mean(
          pitcher_subset$velocity,
          method = "PELT",   #pruned exact linear time
          penalty = "SIC",   #Schwartz Information Criterion, penalizes model complexity. without a penalty, the algorithm would find changepoints everywhere
          minseglen = 2)     #require each segment to have at least 2 data points. essentially- wider minseglen leads to more confident changepoints, but less of them
        cpts <- cpts(cpt_result)
        
        if(length(cpts) > 0) {
          first_cpt <- cpts[1]
          
          # Calculate decline characteristics
          before_mean <- mean(pitcher_subset$velocity[1:first_cpt])
          after_mean <- mean(pitcher_subset$velocity[(first_cpt+1):nrow(pitcher_subset)])
          decline_magnitude <- before_mean - after_mean
          
          # Only keep meaningful declines (0.5mph)
          if(decline_magnitude > 0.5) {
            changepoint_year <- pitcher_subset$year[first_cpt]
            changepoint_age <- pitcher_subset$age[first_cpt]
            
            # Calculate confidence (using segment variance ratio)
            var_before <- var(pitcher_subset$velocity[1:first_cpt])
            var_after <- var(pitcher_subset$velocity[(first_cpt+1):nrow(pitcher_subset)])
            var_ratio <- var_after / (var_before + 1e-6)
            confidence <- pmin(1, decline_magnitude / 3) * (1 - pmin(0.5, var_ratio))
            
            changepoint_results[[as.character(pid)]] <- data.frame(
              player_id = pid,
              player_name = unique(pitcher_subset$player_name),
              changepoint_year = changepoint_year,
              changepoint_age = changepoint_age,
              decline_magnitude = decline_magnitude,
              changepoint_confidence = confidence,
              seasons_before = first_cpt,
              seasons_after = nrow(pitcher_subset) - first_cpt
            )
          }
        }
      }, error = function(e) {
        # Skip pitchers with insufficient variance
      })
    }
    if(i %% 100 == 0) {
      cat("Processed", i, "/", total_pitchers, "pitchers\n")}}
  bind_rows(changepoint_results)
}




# Run changepoint detection
changepoint_results <- detect_changepoints(pitcher_data_final)
cat("Total changepoints detected:", nrow(changepoint_results), "\n")
cat("Median decline:", round(median(changepoint_results$decline_magnitude), 2), "mph\n")
cat("Median age at decline:", round(median(changepoint_results$changepoint_age), 1), "\n\n")

saveRDS(changepoint_results, paste0(output_dir, "changepoint_results_velo_only.rds"))
write_csv(changepoint_results, paste0(output_dir, "changepoint_results_velo_only.csv"))



#calculating era-specific benchmarks
era_benchmarks <- pitcher_data_final %>%
  filter(!is_missing) %>%
  mutate(era = case_when(
    year <= 2014 ~ "2010-2014",
    year <= 2019 ~ "2015-2019",
    TRUE ~ "2020+"
  )) %>%
  group_by(era) %>%
  summarise(
    avg_velocity = mean(velocity, na.rm = TRUE),
    sd_velocity = sd(velocity, na.rm = TRUE),
    n_observations = n(),
    .groups = 'drop')
cat("\nEra Benchmarks:\n")
print(era_benchmarks)
cat("\n")


#TJ count distribution
tj_risk_by_count <- pitcher_data_final %>%
  filter(!is_missing) %>%
  group_by(tj_count) %>%
  summarise(
    n_pitcher_years = n(),
    n_pitchers = n_distinct(player_id),
    .groups = 'drop'
  ) %>%
  mutate(risk_rate = n_pitcher_years / sum(n_pitcher_years))

# Overall TJ risk rate
tj_risk_rate_overall <- tj_risk_by_count %>%
  filter(tj_count > 0) %>%
  summarise(total_risk = sum(risk_rate)) %>%
  pull(total_risk)

cat("Overall TJ risk rate:", round(tj_risk_rate_overall * 100, 2), "%\n")
cat("\nTJ Risk by Count:\n")
print(tj_risk_by_count)
cat("\n")

write_csv(tj_risk_by_count, paste0(output_dir, "tj_risk_by_count.csv"))




#BAYESIAN HIERARCHICAL MODELING
modeling_data <- pitcher_data_final %>%
  filter(!is_missing, year <= 2023) %>%
  select(player_id, player_name, year, age, velocity, 
         had_tommy_john, years_since_tj_capped)

cat("Modeling data:", nrow(modeling_data), "observations\n")
cat("Pitchers:", length(unique(modeling_data$player_id)), "\n")
cat("Years:", min(modeling_data$year), "-", max(modeling_data$year), "\n\n")

#partial pooling - the random intercept shrinks pitchers with less data towards the population mean. I.e. a guy with 8 seasons of data gets an intercept very
#close to his actual average velocity, a gut with 3 seasons gets pulled somewhat toward the league average. big plus is for automatically handling ss weighting.
velocity_model <- stan_glmer(
  #predict velocity from how old you are, whether you've had TJ , and how many years it's been since
  #you've had TJ- while giving each pitcher their own personal baseline velocity.
  velocity ~ age + I(age^2) + had_tommy_john + years_since_tj_capped + (1 | player_id),
  # velocity ~ age + had_tommy_john + years_since_tj_capped + (1 | player_id),
  #logtransformation or squaring of age
  data = modeling_data,
  family = gaussian(),
  prior = normal(0, 2.5),
  #the baseline velocity intercept is centered at 93 with a SD of 10, which is sensible, and the SD means the prior barely constrains the estimate, the data will dominate
  prior_intercept = normal(93, 10),
  #prior on the residual standard deviation. Exponential(1) is a standard weakly informative choice that gently regularizes toward smaller residual variance w/o being restrictive
  prior_aux = exponential(1),
  chains = 4,   #standard practice, allows convergence diagnostics
  iter = 2000,   #2000 iterations per chain
  warmup = 1000,
  seed = 42,
  refresh = 500)   #personal preference to see some progress updates during fitting
cat("Model Summary:\n")
print(summary(velocity_model))
cat("\n")
saveRDS(velocity_model, paste0(output_dir, "velocity_model.rds"))

#Risk assesment framework
risk_assessments <- pitcher_data_final %>%
  filter(!is_missing) %>%
  group_by(player_id, player_name) %>%
  summarise(
    latest_year = last(year),
    current_velocity = last(velocity),
    current_age = last(age),
    career_years = max(career_year),
    recent_decline = ifelse(n() >= 2,
                            last(velocity) - nth(velocity, -2, default = last(velocity)), 0),
    had_tommy_john = as.numeric(last(tj_count) > 0),
    tj_count = last(tj_count),
    years_since_tj = last(years_since_tj),
    years_since_tj_capped = last(years_since_tj_capped),
    .groups = 'drop'
  ) %>%
  mutate(
    era_group = case_when(
      latest_year <= 2014 ~ "2010-2014",
      latest_year <= 2019 ~ "2015-2019",
      TRUE ~ "2020+")
  ) %>%
  # JOIN ALL THE LOOKUPS BEFORE CALCULATING RISK FACTORS
  left_join(
    era_benchmarks %>% select(era, avg_velocity_era = avg_velocity),
    by = c("era_group" = "era")) %>%
  left_join(
    changepoint_results %>% select(player_id, changepoint_confidence),
    by = "player_id") %>%
  left_join(
    tj_risk_by_count %>% select(tj_count, tj_risk = risk_rate),
    by = "tj_count") %>%
  #calculate the risk factors
  mutate(
    #sets between 0 and 1, with 26-36 being the main risk period, and capping at 1 after 36. younger than 26 is low risk, older than 36 is high risk
    age_risk = pmax(0, pmin(1, (current_age - 26) / 10)),
    #set between 0 and 1, with higher risk for pitchers whose current velocity is much lower than the era average for their age
    #A 4mph drop from the era average would max out the risk at 1, while being equal to or above the era average would be 0 risk
    velocity_risk = pmax(0, pmin(1, (avg_velocity_era - current_velocity) / 4)),
    #set between 0 and 1, with higher risk for pitchers who have had a recent significant decline. A decline equal to the decline threshold would be 1,
    # while no decline or an improvement would be 0
    #Captures momentum: a pitcher actively losing velocity right now is higher risk than one who's been stable
    decline_risk = pmax(0, pmin(1, -recent_decline / abs(decline_threshold))),
    #directly uses the changepoint confidence score. If no changepoint was detected the risk is 0, if a strong changepoint was detected the risk approaches 1
    #assigned the highest weight because a signifigant detected changepoint is the strongest evidence that something fundamentally changed with the pitcher
    # age, velocity, and decline are all relatively gradual signals, changepoint says "something broke" -- the most alarming indicatior
    changepoint_risk = ifelse(is.na(changepoint_confidence), 0, changepoint_confidence),
    #pulls from the TJ frequency distribution you computed earlier. Higher TJ count = higher risk
    #The fallback (tj_count * tj_risk_rate_overall) handles edge cases where the join didn't match.
    injury_risk = ifelse(is.na(tj_risk), tj_count * tj_risk_rate_overall, tj_risk),
    #capture recovery window
    recovery_risk = case_when(
      is.na(years_since_tj) ~ 0.0,
      years_since_tj <= 2 ~ 0.6,
      years_since_tj <= 4 ~ 0.3,
      TRUE ~ 0.0),
    overall_risk = (age_risk * 0.18) +    #overall risk calculation
      (velocity_risk * 0.15) + 
      (decline_risk * 0.20) + 
      (changepoint_risk * 0.25) + 
      (injury_risk * 0.17) +
      (recovery_risk * 0.05),
    risk_category = case_when(
      overall_risk < 0.3 ~ "Low Risk",
      overall_risk < 0.6 ~ "Moderate Risk",
      TRUE ~ "High Risk")
  )
cat("Risk Distribution:\n")
print(table(risk_assessments$risk_category))
cat("\n")
saveRDS(risk_assessments, paste0(output_dir, "risk_assessments_velo_only.rds"))
write_csv(risk_assessments, paste0(output_dir, "risk_assessments_velo_only.csv"))


# Model validation 
validation_data <- modeling_data %>% filter(year <= 2023)  # Split data for validation
train_data <- validation_data %>% filter(year <= 2021)
test_data <- validation_data %>% filter(year >= 2022)
cat("Training data:", nrow(train_data), "observations\n")  #initial summary
cat("Test data:", nrow(test_data), "observations\n\n")

# Retrain on training set
velocity_model_train <- update(velocity_model, data = train_data, refresh = 100)
predictions <- posterior_predict(velocity_model_train, newdata = test_data)
predicted_means <- apply(predictions, 2, mean)

rmse <- sqrt(mean((test_data$velocity - predicted_means)^2, na.rm = TRUE))
mae <- mean(abs(test_data$velocity - predicted_means), na.rm = TRUE)
# r_squared <- cor(predicted_means, test_data$velocity, use = "complete.obs")^2
ss_res <- sum((test_data$velocity - predicted_means)^2, na.rm = TRUE)
ss_tot <- sum((test_data$velocity - mean(test_data$velocity, na.rm = TRUE))^2, na.rm = TRUE)
r_squared <- 1 - (ss_res / ss_tot)
# r_squared_true <- 1 - sum((test_data$velocity - predicted_means)^2) / 
#   sum((test_data$velocity - mean(test_data$velocity))^2)

cat("\nVelocity Model Validation (2022-2023):\n")
cat("RMSE:", round(rmse, 2), "mph\n")
cat("MAE:", round(mae, 2), "mph\n")
cat("R-squared:", round(r_squared, 3), "\n\n")

# Prediction error distribution
residuals <- test_data$velocity - predicted_means
cat("Velocity prediction error distribution:\n")
cat("Min:", round(min(residuals, na.rm = TRUE), 2), "\n")
cat("Q1:", round(quantile(residuals, 0.25, na.rm = TRUE), 2), "\n")
cat("Median:", round(median(residuals, na.rm = TRUE), 2), "\n")
cat("Q3:", round(quantile(residuals, 0.75, na.rm = TRUE), 2), "\n")
cat("Max:", round(max(residuals, na.rm = TRUE), 2), "\n\n")

# Calibration check
calibration <- data.frame(
  actual = test_data$velocity,
  predicted = predicted_means) %>%
  mutate(tercile = ntile(predicted, 3)) %>%
  group_by(tercile) %>%
  summarise(
    avg_predicted = mean(predicted),
    avg_actual = mean(actual),
    .groups = 'drop')
print(calibration)

#Future Predictions (2024-2028)
future_years <- 2024:2028
future_predictions <- list()
for(future_year in future_years) {
  current_pitchers <- risk_assessments %>%
    filter(latest_year == max(latest_year)) %>%
    mutate(
      year = future_year,
      age = current_age + (future_year - latest_year),
      years_since_tj_capped = ifelse(
        is.na(years_since_tj), 
        0,
        pmin(years_since_tj + (future_year - latest_year), 5)
      )
    )
  future_preds <- posterior_predict(velocity_model, newdata = current_pitchers)
  
  future_predictions[[as.character(future_year)]] <- data.frame(
    player_id = current_pitchers$player_id,
    player_name = current_pitchers$player_name,
    prediction_year = future_year,
    predicted_age = current_pitchers$age,
    predicted_velocity = apply(future_preds, 2, mean),
    prediction_lower = apply(future_preds, 2, quantile, 0.025),
    prediction_upper = apply(future_preds, 2, quantile, 0.975),
    current_velocity = current_pitchers$current_velocity)}
future_predictions_df <- bind_rows(future_predictions)
cat("Future predictions generated for", length(unique(future_predictions_df$player_id)), "pitchers\n")
saveRDS(future_predictions_df, paste0(output_dir, "future_predictions_velo_only.rds"))
write_csv(future_predictions_df, paste0(output_dir, "future_predictions_velo_only.csv"))



#CASE STUDY EXAMPLES
case_study_names <- c("Hamels, Cole", "Wainwright, Adam", "Taillon, Jameson", "deGrom, Jacob", "Buehler, Walker")
for(name in case_study_names) {
  case_info <- risk_assessments %>% filter(player_name == name)
  if(nrow(case_info) > 0) {
    cat("  -", name, "| Risk:", round(case_info$overall_risk * 100, 1), "%",
        "| Category:", case_info$risk_category, "\n")
  } else {
    cat("  -", name, "| NOT FOUND in dataset\n")
  }}
cat("\n")

# Get case study pitcher IDs
case_study_pitchers <- risk_assessments %>%
  filter(player_name %in% case_study_names) %>%
  pull(player_id)

# Print detailed info for each case study
for(i in seq_along(case_study_pitchers)) {
  pid <- case_study_pitchers[i]
  
  pitcher_info <- risk_assessments %>% filter(player_id == pid)
  pitcher_history <- pitcher_data_final %>% 
    filter(player_id == pid, !is_missing) %>%
    select(player_name, year, age, velocity, tj_count, years_since_tj)
  
  cat(i, ". ", pitcher_info$player_name, "\n", sep = "")
  cat("Current Age:", pitcher_info$current_age, "\n")
  cat("Current Velocity:", round(pitcher_info$current_velocity, 1), "mph\n")
  cat("TJ Count:", pitcher_info$tj_count, "\n")
  cat("Years Since TJ:", ifelse(is.na(pitcher_info$years_since_tj), "N/A", pitcher_info$years_since_tj), "\n")
  cat("Overall Risk:", round(pitcher_info$overall_risk * 100, 1), "%\n")
  cat("Risk Category:", pitcher_info$risk_category, "\n")
  cat("Career Trajectory:\n")
  print(head(pitcher_history, 3))
  cat("   ...\n")
  print(tail(pitcher_history, 3))
  cat("\n")
}


# VISUALIZATIONS

# Overall Risk Distribution
p1 <- ggplot(risk_assessments, aes(x = overall_risk, fill = risk_category)) +
  geom_histogram(bins = 30, alpha = 0.8, color = "black") +
  scale_fill_manual(values = c("Low Risk" = "#2ecc71", 
                               "Moderate Risk" = "#f39c12", 
                               "High Risk" = "#e74c3c")) +
  labs(title = "Pitcher Risk Distribution",
       subtitle = paste0("Total Pitchers Assessed: ", nrow(risk_assessments)),
       x = "Overall Risk Score (0-1)",
       y = "Count",
       fill = "Risk Category") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top",
        text = element_text(size = 12))
ggsave(paste0(output_dir, "overall_risk_distribution.png"), p1, width = 10, height = 6, dpi = 300)

#Velocity Aging Curve
aging_curve_data <- modeling_data %>%
  group_by(age, had_tommy_john) %>%
  summarise(
    mean_velocity = mean(velocity, na.rm = TRUE),
    se_velocity = sd(velocity, na.rm = TRUE) / sqrt(n()),
    n_obs = n(),
    .groups = 'drop'
  ) %>%
  filter(n_obs >= 20)
p2 <- ggplot(aging_curve_data, aes(x = age, y = mean_velocity, 
                                   color = factor(had_tommy_john))) +
  geom_line(size = 1.2) +
  geom_point(aes(size = n_obs), alpha = 0.6) +
  geom_ribbon(aes(ymin = mean_velocity - se_velocity, 
                  ymax = mean_velocity + se_velocity,
                  fill = factor(had_tommy_john)), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values = c("0" = "#3498db", "1" = "#e74c3c"),
                     labels = c("No TJ", "Post-TJ")) +
  scale_fill_manual(values = c("0" = "#3498db", "1" = "#e74c3c"),
                    labels = c("No TJ", "Post-TJ")) +
  labs(title = "Average Velocity by Age - The Aging Curve",
       subtitle = "Population averages by age and TJ status",
       x = "Age",
       y = "Fastball Velocity (mph)",
       color = "TJ Status",
       fill = "TJ Status",
       size = "Sample Size") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top",
        text = element_text(size = 12))
ggsave(paste0(output_dir, "velocity_aging_curve.png"), p2, width = 10, height = 6, dpi = 300)

#Changepoint Confidence Distribution
p3 <- ggplot(changepoint_results, aes(x = changepoint_confidence)) +
  geom_histogram(bins = 20, fill = "#9b59b6", alpha = 0.7, color = "black") +
  labs(title = "Changepoint Detection Confidence Scores",
       subtitle = paste0(nrow(changepoint_results), " velocity declines detected"),
       x = "Confidence Score (0-1)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12))
ggsave(paste0(output_dir, "changepoint_confidence.png"), p3, width = 8, height = 6, dpi = 300)

#Decline Magnitude Distribution
p4 <- ggplot(changepoint_results, aes(x = decline_magnitude)) +
  geom_histogram(bins = 20, fill = "#e67e22", alpha = 0.7, color = "black") +
  geom_vline(xintercept = median(changepoint_results$decline_magnitude), 
             linetype = "dashed", color = "red", size = 1) +
  labs(title = "Velocity Decline Magnitudes",
       subtitle = paste0("Median decline: ", round(median(changepoint_results$decline_magnitude), 2), " mph"),
       x = "Decline Magnitude (mph)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12))
ggsave(paste0(output_dir, "decline_magnitude.png"), p4, width = 8, height = 6, dpi = 300)

#Risk Components Correlation Heatmap
risk_components <- risk_assessments %>%
  select(age_risk, velocity_risk, decline_risk, changepoint_risk, injury_risk, recovery_risk) %>%
  cor(use = "complete.obs")
risk_corr_melted <- melt(risk_components)
p5 <- ggplot(risk_corr_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#3498db", high = "#e74c3c", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  labs(title = "Risk Component Correlation Matrix",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 10))
ggsave(paste0(output_dir, "risk_correlation_heatmap.png"), p5, width = 8, height = 6, dpi = 300)

# #recovery trajectory plot
# recovery_data <- modeling_data %>%
#   filter(had_tommy_john == 1, years_since_tj_capped <= 5, years_since_tj_capped > 0) %>%
#   group_by(years_since_tj_capped) %>%
#   summarise(
#     mean_velocity = mean(velocity, na.rm = TRUE),
#     se_velocity = sd(velocity, na.rm = TRUE) / sqrt(n()),
#     n = n(),
#     .groups = 'drop')
# p_recovery <- ggplot(recovery_data, aes(x = years_since_tj_capped, y = mean_velocity)) +
#   geom_line(color = "#e74c3c", size = 1.5) +
#   geom_point(aes(size = n), color = "#e74c3c", alpha = 0.7) +
#   geom_errorbar(aes(ymin = mean_velocity - 1.96*se_velocity,
#                     ymax = mean_velocity + 1.96*se_velocity),
#                 width = 0.2, color = "#e74c3c", size = 1) +
#   scale_size_continuous(name = "Sample Size", range = c(3, 8)) +
#   labs(title = "Post-TJ Recovery Trajectory",
#        subtitle = "Average velocity by years since Tommy John surgery (±95% CI)",
#        x = "Years Since TJ Surgery",
#        y = "Average Velocity (mph)") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 14, face = "bold"),
#         legend.position = "right",
#         text = element_text(size = 12))
# ggsave(paste0(output_dir, "recovery_trajectory.png"), p_recovery, width = 10, height = 6, dpi = 300)

# Age-Adjusted TJ Recovery Trajectory (-1 to +3 years)
tj_first_surgery <- tj_summary %>%
  filter(tj_number == 1) %>%
  select(player_id, tj_year)

tj_trajectory_data <- tj_first_surgery %>%
  tidyr::crossing(offset = -1:3) %>%
  mutate(year = tj_year + offset) %>%
  inner_join(modeling_data, by = c("player_id", "year"))

tj_preds <- posterior_predict(velocity_model, newdata = tj_trajectory_data)
tj_trajectory_data$predicted <- apply(tj_preds, 2, mean)
tj_trajectory_data$residual <- tj_trajectory_data$velocity - tj_trajectory_data$predicted

recovery_extended <- tj_trajectory_data %>%
  group_by(offset) %>%
  summarise(
    mean_residual = mean(residual, na.rm = TRUE),
    se = sd(residual, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = 'drop')

p_recovery_adjusted <- ggplot(recovery_extended, aes(x = offset, y = mean_residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.8) +
  geom_vline(xintercept = 0.5, linetype = "dotted", color = "#e74c3c", 
             size = 0.8, alpha = 0.5) +
  annotate("text", x = 0.5, y = max(recovery_extended$mean_residual) * 1.4,
           label = "TJ Surgery", color = "#e74c3c", size = 3.5, 
           fontface = "bold", hjust = 0.5) +
  geom_line(color = "#e74c3c", size = 1.5) +
  geom_point(aes(size = n), color = "#e74c3c", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_residual - 1.96 * se,
                    ymax = mean_residual + 1.96 * se),
                width = 0.15, color = "#e74c3c", size = 1) +
  scale_x_continuous(
    breaks = -1:3,
    labels = c("Pre-TJ", "TJ Year", "Year 1", "Year 2", "Year 3")
    ) +
  scale_size_continuous(name = "Sample Size", range = c(3, 8)) +
  labs(title = "TJ Recovery Trajectory (Age-Adjusted)",
       subtitle = "Velocity relative to Bayesian model's age-predicted baseline | First TJ only",
       x = "Timeline Relative to TJ Surgery",
       y = "Velocity vs. Age-Expected (mph)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "right",
    panel.background = element_rect(fill = "#FFF8DC", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
ggsave(paste0(output_dir, "recovery_trajectory_age_adjusted.png"), p_recovery_adjusted, width = 12, height = 7, dpi = 300)

#Changepoint Age Distribution
p8 <- ggplot(changepoint_results, aes(x = changepoint_age)) +
  geom_histogram(bins = 15, fill = "#16a085", alpha = 0.7, color = "black") +
  geom_vline(xintercept = median(changepoint_results$changepoint_age),
             linetype = "dashed", color = "red", size = 1) +
  labs(title = "Age Distribution of Velocity Declines",
       subtitle = paste0("Median age: ", round(median(changepoint_results$changepoint_age), 1)),
       x = "Age at Changepoint",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12))
ggsave(paste0(output_dir, "changepoint_age_distribution.png"), p8, width = 8, height = 6, dpi = 300)

#Risk Distribution by Category -Donut Chart
risk_summary <- risk_assessments %>%
  group_by(risk_category) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = count / sum(count) * 100)
p9 <- ggplot(risk_summary, aes(x = 2, y = count, fill = risk_category)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 2) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = c("Low Risk" = "#2ecc71", 
                               "Moderate Risk" = "#f39c12", 
                               "High Risk" = "#e74c3c")) +
  geom_text(aes(label = paste0(risk_category, "\n", count, " pitchers\n(", 
                               round(percentage, 1), "%)")),
            position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold", color = "white") +
  labs(title = "Pitcher Risk Distribution",
       subtitle = paste0("Total Pitchers Assessed: ", nrow(risk_assessments))) +
  theme_void() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "none")
ggsave(paste0(output_dir, "risk_distribution_donut.png"), p9, width = 8, height = 6, dpi = 300)

#Risk Score Density by Category
p10 <- ggplot(risk_assessments, aes(x = overall_risk, fill = risk_category)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Low Risk" = "#2ecc71", 
                               "Moderate Risk" = "#f39c12", 
                               "High Risk" = "#e74c3c")) +
  labs(title = "Risk Score Density Distribution",
       subtitle = "Smooth distribution of overall risk scores",
       x = "Overall Risk Score (0-1)",
       y = "Density",
       fill = "Risk Category") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top",
        text = element_text(size = 12))
ggsave(paste0(output_dir, "risk_density_by_category.png"), p10, width = 10, height = 6, dpi = 300)


# YOUNG PITCHER PROJECTION FUNCTION
create_projection_final <- function(pitcher_name_input, pitcher_data, future_preds, risk_data) {
  pitcher_id <- risk_data %>% 
    filter(player_name == pitcher_name_input) %>% 
    pull(player_id)
  
  risk_info <- risk_data %>% filter(player_id == pitcher_id)
  
  # Actual data
  actual_data <- pitcher_data %>%
    filter(player_id == pitcher_id, !is_missing) %>%
    select(age, velocity, year, tj_count) %>%
    arrange(year)
  
  # Future predictions
  future_data <- future_preds %>%
    filter(player_id == pitcher_id) %>%
    select(age = predicted_age, velocity = predicted_velocity,
           lower = prediction_lower, upper = prediction_upper)
  
  # TJ markers (color coded)
  tj_in_window <- actual_data %>%
    arrange(year) %>%
    mutate(tj_this_year = tj_count > lag(tj_count) & !is.na(lag(tj_count))) %>%
    filter(tj_this_year) %>%
    mutate(tj_type = "In Window")
  
  tj_before_window <- actual_data %>%
    slice(1) %>%
    filter(tj_count > 0) %>%
    mutate(tj_type = "Before Window")
  
  if(nrow(tj_before_window) > 0 && nrow(tj_in_window) > 0) {
    if(tj_before_window$age[1] %in% tj_in_window$age) {
      tj_before_window <- tj_before_window[0, ]}}
  
  tj_data <- bind_rows(tj_in_window, tj_before_window)
  p <- ggplot() +
    theme(panel.background = element_rect(fill = "#FFF8DC", color = NA),
          plot.background = element_rect(fill = "white", color = NA)) +
    

    geom_ribbon(data = future_data,    # Future ribbon
                aes(x = age, ymin = lower, ymax = upper),
                fill = "#27ae60", alpha = 0.18) +
    
    geom_line(data = actual_data, aes(x = age, y = velocity),    # Actual data
              color = "#2c3e50", size = 2) +
    geom_point(data = actual_data, aes(x = age, y = velocity),
               color = "#2c3e50", size = 3.5) +

    geom_line(data = future_data, aes(x = age, y = velocity),    # Future predictions
              color = "#27ae60", size = 1.5, linetype = "dashed") +
    geom_point(data = future_data, aes(x = age, y = velocity),
               color = "#27ae60", size = 3.5, shape = 17) +
    
    # TJ markers
    {if(nrow(tj_data) > 0) {
      list(
        if(nrow(tj_data %>% filter(tj_type == "In Window")) > 0)
          geom_point(data = tj_data %>% filter(tj_type == "In Window"),
                     aes(x = age, y = velocity),
                     color = "#e74c3c", size = 6, shape = 4, stroke = 2),
        if(nrow(tj_data %>% filter(tj_type == "Before Window")) > 0)
          geom_point(data = tj_data %>% filter(tj_type == "Before Window"),
                     aes(x = age, y = velocity),
                     color = "#f39c12", size = 6, shape = 4, stroke = 2)
      )
    }} +
    
    # annotate("text", 
    #          x = mean(c(actual_data$age, future_data$age)),
    #          y = min(c(actual_data$velocity, future_data$velocity)) * 0.985,
    #          label = "Note: Projections regress toward population mean\n(conservative for elite performers)",
    #          size = 2.8, color = "gray30", fontface = "italic") +
    
    labs(title = paste0(pitcher_name_input, " - Velocity Projection"),
         subtitle = paste0("Age ", risk_info$current_age, " | Current: ",
                           round(risk_info$current_velocity, 1), 
                           " mph | Projected through 2028"),
         x = "Age (years)",
         y = "Fastball Velocity (mph)") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      panel.background = element_rect(fill = "#FFF8DC", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank()
    )
  return(p)}


#Case Studies visuals function
create_case_study_final <- function(pitcher_name_input, pitcher_data, 
                                    model, changepoint_data, risk_data) {
  
  pitcher_id <- risk_data %>% 
    filter(player_name == pitcher_name_input) %>% 
    pull(player_id)
  
  if(length(pitcher_id) == 0) {
    cat("  Pitcher not found:", pitcher_name_input, "\n")
    return(NULL)
  }
  
  actual_data <- pitcher_data %>%
    filter(player_id == pitcher_id, !is_missing) %>%
    select(year, age, velocity, tj_count) %>%
    arrange(year)
  
  cp_info <- changepoint_data %>% filter(player_id == pitcher_id)
  risk_info <- risk_data %>% filter(player_id == pitcher_id)
  
  age_range <- seq(min(actual_data$age), max(actual_data$age), by = 0.5)
  pred_data <- data.frame(
    age = age_range,
    player_id = pitcher_id,
    years_since_tj_capped = ifelse(nrow(risk_info) > 0, 
                                   risk_info$years_since_tj_capped[1], 0),
    had_tommy_john = ifelse(nrow(risk_info) > 0,
                            risk_info$had_tommy_john[1], 0))
  pred_data$years_since_tj_capped[is.na(pred_data$years_since_tj_capped)] <- 0
  
  preds <- posterior_predict(model, newdata = pred_data)
  pred_data$predicted_velocity <- apply(preds, 2, mean)
  pred_data$lower <- apply(preds, 2, quantile, 0.025)
  pred_data$upper <- apply(preds, 2, quantile, 0.975)
  
  first_obs_tj_count <- actual_data$tj_count[1]
  
  if(first_obs_tj_count > 0) {
    tj_before_window <- actual_data[1, ] %>%
      select(age, velocity, year) %>%
      mutate(tj_type = "Before Window")
    if(nrow(actual_data) > 1) {
      tj_in_window <- actual_data[-1, ] %>%
        arrange(year) %>%
        mutate(tj_change = tj_count - lag(tj_count, default = first_obs_tj_count)) %>%
        filter(tj_change > 0) %>%
        select(age, velocity, year) %>%
        mutate(tj_type = "In Window")
    } else {
      tj_in_window <- data.frame(age = numeric(0), velocity = numeric(0), 
                                 year = numeric(0), tj_type = character(0))
    }
  } else {
    tj_before_window <- data.frame(age = numeric(0), velocity = numeric(0), 
                                   year = numeric(0), tj_type = character(0))
    tj_in_window <- actual_data %>%
      arrange(year) %>%
      mutate(tj_change = tj_count - lag(tj_count, default = 0)) %>%
      filter(tj_change > 0) %>%
      select(age, velocity, year) %>%
      mutate(tj_type = "In Window")
  }
  
  tj_data <- bind_rows(tj_before_window, tj_in_window)
  
  p <- ggplot() +
    theme(panel.background = element_rect(fill = "#FFF8DC", color = NA),
          plot.background = element_rect(fill = "white", color = NA)) +
    geom_ribbon(data = pred_data, 
                aes(x = age, ymin = lower, ymax = upper),
                fill = "#3498db", alpha = 0.12) +
    geom_line(data = pred_data,
              aes(x = age, y = predicted_velocity),
              color = "#3498db", size = 0.7, linetype = "dashed") +
    geom_line(data = actual_data,
              aes(x = age, y = velocity),
              color = "#2c3e50", size = 2.5) +
    geom_point(data = actual_data,
               aes(x = age, y = velocity),
               color = "#2c3e50", size = 4.5) +
    labs(
      title = pitcher_name_input,
      subtitle = sprintf(
        "Risk: %s (%d%%) | Current: %.1f mph | Age: %d | TJ: %d",
        risk_info$risk_category,
        round(risk_info$overall_risk * 100),
        risk_info$current_velocity,
        risk_info$current_age,
        risk_info$tj_count),
      x = "Age (years)",
      y = "Fastball Velocity (mph)") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 16, face = "bold", color = "#2c3e50"),
      plot.subtitle = element_text(size = 10, color = "#7f8c8d"),
      axis.title = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#FFF8DC", color = NA),
      plot.background = element_rect(fill = "white", color = NA))
  
  if(nrow(cp_info) > 0) {
    p <- p + 
      geom_vline(xintercept = cp_info$changepoint_age[1],
                 linetype = "dashed", color = "#e74c3c", size = 1.2) +
      annotate("text", x = cp_info$changepoint_age[1], 
               y = max(actual_data$velocity) * 0.99,
               label = paste0("Changepoint\n", cp_info$changepoint_year[1]),
               color = "#e74c3c", size = 3.5, fontface = "bold", vjust = 1)
  }
  
  if(nrow(tj_data) > 0) {
    red_tj <- tj_data %>% filter(tj_type == "In Window")
    if(nrow(red_tj) > 0) {
      p <- p + geom_point(data = red_tj, aes(x = age, y = velocity),
                          color = "#e74c3c", size = 7, shape = 4, stroke = 2.5)
    }
    yellow_tj <- tj_data %>% filter(tj_type == "Before Window")
    if(nrow(yellow_tj) > 0) {
      p <- p + 
        geom_point(data = yellow_tj, aes(x = age, y = velocity),
                   color = "#f39c12", size = 7, shape = 4, stroke = 2.5) +
        annotate("text", x = yellow_tj$age[1], 
                 y = yellow_tj$velocity[1] * 0.985,
                 label = "TJ pre-2015",
                 color = "#f39c12", size = 2.8, fontface = "italic", vjust = 1)
    }
  }
  return(p)}
for(name in case_study_names) {
  cat("Generating case study:", name, "\n")
  p <- create_case_study_final(name, pitcher_data_final, velocity_model, 
                               changepoint_results, risk_assessments)
  if(!is.null(p)) {
    filename <- paste0(output_dir, "case_study_", 
                       gsub(", ", "_", tolower(name)), "_final.png")
    ggsave(filename, p, width = 10, height = 6, dpi = 300)
    cat("  Saved:", filename, "\n")
  }
}

projection_names <- c("Brown, Hunter", "Cabrera, Edward")
for(name in projection_names) {
  cat("Generating projection:", name, "\n")
  p <- create_projection_final(name, pitcher_data_final, 
                               future_predictions_df, risk_assessments)
  if(!is.null(p)) {
    filename <- paste0(output_dir, "projection_", 
                       gsub(", ", "_", tolower(name)), "_final.png")
    ggsave(filename, p, width = 10, height = 6, dpi = 300)
    cat("  Saved:", filename, "\n")
  }
}

#Save validation metrics
validation_summary <- data.frame(
  metric = c("RMSE", "MAE", "R_squared"),
  value = c(rmse, mae, r_squared))
write_csv(validation_summary, paste0(output_dir, "validation_summary_velo_only.csv"))

cat("Results Summary:\n")
cat("Velocity changepoints detected:", nrow(changepoint_results), "\n")
cat("Pitchers assessed:", nrow(risk_assessments), "\n")
cat("High risk pitchers:", sum(risk_assessments$risk_category == "High Risk"), "\n")
cat("Moderate risk pitchers:", sum(risk_assessments$risk_category == "Moderate Risk"), "\n")
cat("Low risk pitchers:", sum(risk_assessments$risk_category == "Low Risk"), "\n")
cat("Velocity model RMSE:", round(rmse, 2), "mph\n")
#sanity check
cat("files all saved\n")

cat("Case Studies:\n")
for(name in case_study_names) {
  case_info <- risk_assessments %>% filter(player_name == name)
  if(nrow(case_info) > 0) {
    cat("  -", name, "| Risk:", round(case_info$overall_risk * 100, 1), "%\n")
  }}
cat("\n")

cat("Top 10 Highest Risk Pitchers:\n")
top_risk <- risk_assessments %>%
  arrange(desc(overall_risk)) %>%
  select(player_name, current_age, current_velocity, 
         tj_count, years_since_tj, overall_risk, risk_category) %>%
  head(10)
print(top_risk)
cat("\n")


performance_data <- data.frame(
  actual = test_data$velocity,
  predicted = predicted_means)

p_performance <- ggplot(performance_data, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.4, color = "#3498db") +
  geom_abline(slope = 1, intercept = 0, color = "#e74c3c", linetype = "dashed") +
  annotate("text", x = 88, y = 96, 
           label = paste0("RMSE: ", round(rmse, 2), " mph\nR² = ", round(r_squared, 3)),
           size = 5, fontface = "bold") +
  labs(title = "Model Validation: Predicted vs Actual",
       subtitle = "Held-out test set (2022-2023)",
       x = "Actual Velocity (mph)",
       y = "Predicted Velocity (mph)") +
  theme_minimal()
ggsave(paste0(output_dir, "model_performance.png"), p_performance, 
       width = 8, height = 6, dpi = 300)


#Save aging curve visualizations
#Smoothed Agng Curve
aging_curve_clean <- modeling_data %>%
  group_by(age) %>%
  summarise(
    mean_velocity = mean(velocity, na.rm = TRUE),
    n_obs = n(),
    se = sd(velocity, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  filter(n_obs >= 20)

loess_fit <- loess(mean_velocity ~ age, data = aging_curve_clean, span = 0.3)
aging_curve_clean$smoothed_velocity <- predict(loess_fit)

peak_age <- aging_curve_clean$age[which.max(aging_curve_clean$smoothed_velocity)]
peak_velocity <- max(aging_curve_clean$smoothed_velocity)

p_smooth <- ggplot(aging_curve_clean, aes(x = age)) +
  geom_ribbon(aes(ymin = mean_velocity - 1.96*se, 
                  ymax = mean_velocity + 1.96*se),
              fill = "#3498db", alpha = 0.15) +
  geom_line(aes(y = smoothed_velocity), 
            color = "#2c3e50", size = 2.5) +
  geom_point(aes(y = mean_velocity, size = n_obs),
             color = "#3498db", alpha = 0.4) +
  geom_vline(xintercept = peak_age, linetype = "dashed", 
             color = "#27ae60", size = 1.2) +
  annotate("text", x = peak_age, y = peak_velocity + 0.3, 
           label = paste0("Peak: Age ", peak_age),
           color = "#27ae60", size = 5, fontface = "bold") +
  scale_size_continuous(name = "Sample Size", range = c(1, 6)) +
  labs(title = "MLB Pitcher Velocity Aging Curve (2015-2024)",
       subtitle = "Smoothed population trajectory with 95% confidence intervals",
       x = "Age (years)",
       y = "Average Fastball Velocity (mph)") +
  coord_cartesian(ylim = c(91, 96)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "bottom",
    panel.background = element_rect(fill = "#FFF8DC", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave(paste0(output_dir, "aging_curve_smoothed_presentation.png"), 
       p_smooth, width = 12, height = 7, dpi = 300)

#Raw aging curve (for appendix)
p_raw <- ggplot(aging_curve_clean, aes(x = age, y = mean_velocity)) +
  geom_ribbon(aes(ymin = mean_velocity - 1.96*se, 
                  ymax = mean_velocity + 1.96*se),
              fill = "#3498db", alpha = 0.2) +
  geom_line(color = "#2c3e50", size = 1.5) +
  geom_point(aes(size = n_obs), color = "#2c3e50", alpha = 0.6) +
  scale_size_continuous(name = "Sample Size", range = c(2, 8)) +
  labs(title = "Raw Year-to-Year Aging Pattern",
       subtitle = "Unsmoothed population averages showing natural variance",
       x = "Age (years)",
       y = "Average Fastball Velocity (mph)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = NA)
  )
ggsave(paste0(output_dir, "aging_curve_raw_appendix.png"), 
       p_raw, width = 10, height = 6, dpi = 300)


case_study_files <- c(
  "case_study_hamels_cole_final.png",
  "case_study_wainwright_adam_final.png", 
  "case_study_taillon_jameson_final.png",
  "case_study_degrom_jacob_final.png",
  "case_study_buehler_walker_final.png")

for(file in case_study_files) {
  src <- paste0("/mnt/user-data/uploads/", file)
  dst <- paste0(output_dir, file)
  if(file.exists(src)) {
    file.copy(src, dst, overwrite = TRUE)
    cat("Copied:", file, "\n")}}



#Universal Aging Curve
aging_curve_clean <- modeling_data %>%
  group_by(age) %>%
  summarise(
    mean_velocity = mean(velocity, na.rm = TRUE),
    n_obs = n(),
    se = sd(velocity, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  filter(n_obs >= 20)

loess_fit <- loess(mean_velocity ~ age, data = aging_curve_clean, span = 0.3)
aging_curve_clean$smoothed_velocity <- predict(loess_fit)

p_aging_final <- ggplot(aging_curve_clean, aes(x = age)) +
  # Confidence band
  geom_ribbon(aes(ymin = mean_velocity - 1.96*se, 
                  ymax = mean_velocity + 1.96*se),
              fill = "#3498db", alpha = 0.15) +
  
  # smoothed line (no sample size bubbles)
  geom_line(aes(y = smoothed_velocity), 
            color = "#2c3e50", size = 2.5) +
  
  # Peak marker at peak_age
  geom_vline(xintercept = peak_age, linetype = "dashed", 
             color = "#27ae60", size = 0.8, alpha = 0.3) +
  annotate("text", x = peak_age, y = 94.8, 
           label = paste0("Peak Age: ", peak_age),
           color = "#27ae60", size = 4.5, fontface = "bold", alpha = 0.7) +
  
  # Phase annotations with arrows
  annotate("segment", x = 22, xend = 25.5,   # Development phase (22-26)
           y = 92.8, yend = 92.8,
           arrow = arrow(length = unit(0.3, "cm")), 
           color = "#27ae60", size = 1) +
  annotate("text", x = 23.5, y = 93.1, 
           label = "Development", color = "#27ae60", size = 4.5, fontface = "bold") +
  
  annotate("segment", x = 26.5, xend = 30.0,   # Prime years (26-30)
           y = 93.6, yend = 93.6,
           arrow = arrow(length = unit(0.3, "cm")), 
           color = "#f39c12", size = 1) +
  annotate("text", x = 29, y = 93.9, 
           label = "Prime Years", color = "#f39c12", size = 4.5, fontface = "bold") +
  
  annotate("segment", x = 30.0, xend = 36,   # Decline phase (30+)
           y = 91.8, yend = 91.8,
           arrow = arrow(length = unit(0.3, "cm")), 
           color = "#e74c3c", size = 1) +
  annotate("text", x = 34, y = 92.1, 
           label = "Decline", color = "#e74c3c", size = 4.5, fontface = "bold") +
  
  labs(title = "Population-Level Velocity Aging Curve",
       subtitle = paste0("Smoothed trajectory with 95% confidence intervals (n = ", 
                         format(sum(aging_curve_clean$n_obs), big.mark = ","), 
                         " pitcher-seasons)"),
       x = "Age (years)",
       y = "Fastball Velocity (mph)") +
  coord_cartesian(ylim = c(91.5, 95.5)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#FFF8DC", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave(paste0(output_dir, "aging_curve_final_presentation.png"), 
       p_aging_final, width = 12, height = 7, dpi = 300)

beepr::beep(8)