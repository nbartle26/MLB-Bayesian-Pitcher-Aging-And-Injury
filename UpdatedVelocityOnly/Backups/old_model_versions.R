






# library(tidyverse)
# library(rstanarm)
# library(changepoint)
# library(beepr)
# library(readxl)
# library(lubridate)
# library(reshape2)
# set.seed(42)
# 
# stats <- read_csv("C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/stats.csv")
# 
# cat("  Raw stats shape:", nrow(stats), "rows,", ncol(stats), "columns\n")
# cat("  Years covered:", min(stats$year), "to", max(stats$year), "\n")
# cat("  Unique pitchers:", length(unique(stats$player_id)), "\n\n")
# 
# tj_data <- read_excel("C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/CopyofTommyJohnSurgeryList.xlsx", 
#                       skip = 1)
# 
# # Keep ALL TJ surgeries per pitcher
# tj_pitchers_all <- tj_data %>%
#   filter(Position == "P", !is.na(mlbamid)) %>%
#   select(player_id = mlbamid, Player, `TJ Surgery Date`, Team, Age) %>%
#   rename(tj_surgery_date = `TJ Surgery Date`, tj_age = Age) %>%
#   mutate(
#     player_id = as.integer(player_id),
#     tj_year = year(tj_surgery_date)
#   ) %>%
#   arrange(player_id, tj_surgery_date)
# 
# # Create summary with TJ number for each pitcher
# tj_summary <- tj_pitchers_all %>%
#   group_by(player_id) %>%
#   mutate(tj_number = row_number()) %>%
#   ungroup() %>%
#   select(player_id, tj_year, tj_number)
# 
# cat("  Total TJ surgeries for pitchers:", nrow(tj_pitchers_all), "\n")
# cat("  Unique pitchers with TJ:", length(unique(tj_pitchers_all$player_id)), "\n")
# cat("  TJ date range:", min(tj_pitchers_all$tj_surgery_date, na.rm = TRUE), 
#     "to", max(tj_pitchers_all$tj_surgery_date, na.rm = TRUE), "\n\n")
# 
# # ============================================================
# # STEP 2: CLEAN STATS DATA
# # ============================================================
# 
# pitcher_stats <- stats %>%
#   filter(
#     !is.na(ff_avg_speed),                      # Must have fastball velocity
#     !is.na(player_age),                        # Must have age
#     ff_avg_speed >= 85 & ff_avg_speed <= 105  # Remove velocity outliers
#   ) %>%
#   mutate(
#     player_name = str_trim(`last_name, first_name`),
#     velocity = ff_avg_speed,
#     age = player_age,
#     # Track season quality
#     season_type = case_when(
#       p_formatted_ip >= 30 ~ "full_season",
#       p_formatted_ip > 0 & p_formatted_ip < 30 ~ "partial_season",
#       TRUE ~ "no_data"
#     )
#   ) %>%
#   select(
#     player_id, player_name, year, age, velocity, season_type, p_formatted_ip
#   ) %>%
#   arrange(player_id, year)
# 
# cat("  Cleaned stats shape:", nrow(pitcher_stats), "rows\n")
# cat("  Season types:\n")
# print(table(pitcher_stats$season_type))
# cat("\n")
# 
# # ============================================================
# # STEP 3: MERGE INJURY DATA & CALCULATE TJ_COUNT
# # ============================================================
# 
# cat("Merging injury data...\n")
# 
# # For each pitcher-year, count how many TJs they've had BEFORE that season
# pitcher_data_merged <- pitcher_stats %>%
#   left_join(
#     tj_summary %>%
#       expand_grid(year = min(pitcher_stats$year):max(pitcher_stats$year)) %>%
#       filter(year > tj_year) %>%
#       group_by(player_id, year) %>%
#       summarise(tj_count = n(), .groups = 'drop'),
#     by = c("player_id", "year")
#   ) %>%
#   mutate(
#     tj_count = replace_na(tj_count, 0),
#     had_tommy_john = as.numeric(tj_count > 0))
# 
# cat("  Pitchers with TJ history:", 
#     length(unique(pitcher_data_merged$player_id[pitcher_data_merged$had_tommy_john > 0])), "\n\n")
# 
# # ============================================================
# # STEP 4: HANDLE MISSING SEASONS
# # ============================================================
# 
# # Complete year sequences for each pitcher
# pitcher_data_complete <- pitcher_data_merged %>%
#   group_by(player_id) %>%
#   filter(n() >= 3) %>%
#   complete(year = min(year):max(year)) %>%
#   ungroup()
# 
# # Fill in pitcher info for missing rows and identify injury years
# pitcher_data_complete <- pitcher_data_complete %>%
#   group_by(player_id) %>%
#   fill(player_name, .direction = "downup") %>%
#   mutate(
#     # Mark missing vs observed data
#     is_missing = is.na(velocity),
#     
#     # Use tj_count to detect injury years
#     tj_count_filled = replace_na(tj_count, 0),
#     tj_this_year = tj_count_filled > lag(tj_count_filled, default = 0),
#     is_injury_year = tj_this_year,
#     
#     # Classify gap type
#     gap_type = case_when(
#       !is_missing ~ "data_present",
#       is_injury_year ~ "injury_related",
#       TRUE ~ "other_absence"),
#     
#     # Update had_tommy_john for filled rows
#     had_tommy_john = as.numeric(tj_count_filled > 0)
#   ) %>%
#   select(-tj_this_year, -tj_count_filled) %>%
#   ungroup()
# 
# cat("  Total rows after filling gaps:", nrow(pitcher_data_complete), "\n")
# cat("  Gap types:\n")
# print(table(pitcher_data_complete$gap_type))
# cat("\n")
# 
# # ============================================================
# # STEP 5: CALCULATE CAREER METRICS + RECOVERY TRAJECTORIES
# # ============================================================
# 
# cat("Calculating career metrics and recovery trajectories...\n")
# 
# # Calculate data-driven decline threshold
# normal_changes <- pitcher_data_complete %>%
#   filter(!is_missing, !is_injury_year) %>%
#   group_by(player_id) %>%
#   arrange(year) %>%
#   mutate(velocity_change = velocity - lag(velocity)) %>%
#   ungroup() %>%
#   filter(
#     !is.na(velocity_change),
#     velocity_change > -10 & velocity_change < 5
#   ) %>%
#   pull(velocity_change)
# 
# decline_threshold <- -2 * sd(normal_changes, na.rm = TRUE)
# cat("  Data-driven decline threshold:", round(decline_threshold, 2), "mph\n\n")
# 
# # Add career metrics and recovery trajectories
# pitcher_data_final <- pitcher_data_complete %>%
#   group_by(player_id, player_name) %>%
#   arrange(year) %>%
#   mutate(
#     # Career year
#     career_year = year - min(year) + 1,
#     
#     # Create tj_event_year when tj_count increases
#     tj_event_year = ifelse(tj_count > lag(tj_count, default = 0), year, NA)
#   ) %>%
#   fill(tj_event_year, .direction = "down") %>%
#   mutate(
#     # Calculate years since TJ
#     years_since_tj = ifelse(!is.na(tj_event_year), year - tj_event_year, NA),
#     years_since_tj_capped = ifelse(is.na(years_since_tj), 0, pmin(years_since_tj, 5)),
#     
#     # TIME-AWARE CHANGES
#     years_since_last = ifelse(!is_missing, year - lag(year[!is_missing]), NA),
#     
#     # Velocity change per year
#     velocity_change = ifelse(
#       !is_missing & !is.na(lag(velocity[!is_missing])),
#       (velocity - lag(velocity[!is_missing])) / years_since_last,
#       NA
#     ),
#     
#     # Decline indicator
#     decline_indicator = ifelse(
#       !is.na(velocity_change) & velocity_change < decline_threshold / 2,
#       1, 0
#     )
#   ) %>%
#   ungroup()
# 
# cat("  Final dataset shape:", nrow(pitcher_data_final), "rows\n")
# cat("  Total pitchers:", length(unique(pitcher_data_final$player_id)), "\n\n")
# 
# saveRDS(pitcher_data_final, 
#         "C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/pitcher_data_final_velo_only.rds")
# 
# # ============================================================
# # STEP 6: CHANGEPOINT DETECTION
# # ============================================================
# 
# cat("\n========== CHANGEPOINT DETECTION ==========\n\n")
# 
# detect_changepoints <- function(pitcher_data) {
#   changepoint_results <- list()
#   
#   pitcher_data_observed <- pitcher_data %>%
#     filter(!is_missing) %>%
#     group_by(player_id) %>%
#     filter(n() >= 3) %>%
#     ungroup()
#   
#   unique_pitcher_ids <- unique(pitcher_data_observed$player_id)
#   
#   cat("Detecting changepoints for", length(unique_pitcher_ids), "pitchers...\n")
#   pb <- txtProgressBar(min = 0, max = length(unique_pitcher_ids), style = 3)
#   
#   for(i in seq_along(unique_pitcher_ids)) {
#     pitcher_id <- unique_pitcher_ids[i]
#     pitcher_subset <- pitcher_data_observed %>% 
#       filter(player_id == pitcher_id) %>%
#       arrange(year)
#     
#     pitcher_name <- first(pitcher_subset$player_name)
#     
#     if(nrow(pitcher_subset) >= 3) {
#       ts_data <- pitcher_subset$velocity
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
#             if(decline_magnitude > 0.5) {  # 0.5 mph threshold
#               sd_pre <- sd(ts_data[1:changepoint_index], na.rm = TRUE)
#               sd_post <- sd(ts_data[(changepoint_index+1):length(ts_data)], na.rm = TRUE)
#               pooled_sd <- sqrt((sd_pre^2 + sd_post^2) / 2)
#               
#               effect_size <- abs(decline_magnitude) / (pooled_sd + 0.01)
#               changepoint_confidence <- pmin(0.95, pmax(0.5, effect_size / 3))
#               
#               changepoint_results[[as.character(pitcher_id)]] <- data.frame(
#                 player_id = pitcher_id,
#                 pitcher = pitcher_name,
#                 changepoint_year = actual_year,
#                 changepoint_confidence = changepoint_confidence,
#                 pre_decline_velocity = pre_decline_mean,
#                 post_decline_velocity = post_decline_mean,
#                 decline_magnitude = decline_magnitude,
#                 career_years_to_decline = changepoint_index
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
# cat("Detecting velocity changepoints...\n")
# changepoint_results <- detect_changepoints(pitcher_data_final)
# 
# cat("\nChangepoint Summary:\n")
# cat("  Velocity declines detected:", nrow(changepoint_results), "\n")
# cat("  Average years to decline:", round(mean(changepoint_results$career_years_to_decline), 1), "\n")
# cat("  Average decline magnitude:", round(mean(changepoint_results$decline_magnitude), 2), "mph\n\n")
# 
# # ============================================================
# # STEP 7: BAYESIAN VELOCITY MODEL WITH RECOVERY
# # ============================================================
# modeling_data <- pitcher_data_final %>%
#   filter(!is_missing, season_type == "full_season")
# 
# cat("Fitting velocity aging model with recovery trajectories...\n")
# 
# velocity_model <- stan_lmer(
#   velocity ~ age + I(age^2) + years_since_tj_capped + I(years_since_tj_capped^2) + (age | player_id),
#   data = modeling_data,
#   cores = 2,
#   iter = 1000,
#   chains = 2,
#   refresh = 100)
# 
# cat("\nModel fitted successfully!\n\n")
# 
# # ============================================================
# # STEP 8: CALCULATE ERA-DEPENDENT BENCHMARKS
# # ============================================================
# era_benchmarks <- modeling_data %>%
#   mutate(
#     era = case_when(
#       year <= 2014 ~ "2010-2014",
#       year <= 2019 ~ "2015-2019",
#       TRUE ~ "2020+")
#   ) %>%
#   group_by(era) %>%
#   summarise(
#     avg_velocity = mean(velocity, na.rm = TRUE),
#     sd_velocity = sd(velocity, na.rm = TRUE),
#     .groups = 'drop')
# 
# print(era_benchmarks)
# cat("\n")
# 
# # ============================================================
# # STEP 9: DATA-DRIVEN INJURY RISK BY TJ COUNT
# # ============================================================
# 
# tj_risk_by_count <- pitcher_data_final %>%
#   filter(!is_missing, tj_count > 0) %>%
#   group_by(player_id) %>%
#   summarise(
#     tj_count = last(tj_count),
#     had_decline = any(decline_indicator == 1, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   group_by(tj_count) %>%
#   summarise(
#     n_pitchers = n(),
#     risk_rate = mean(had_decline, na.rm = TRUE),
#     .groups = 'drop')
# 
# cat("  TJ Risk by Count:\n")
# print(tj_risk_by_count)
# 
# tj_risk_rate_overall <- pitcher_data_final %>%
#   filter(!is_missing, had_tommy_john == 1) %>%
#   group_by(player_id) %>%
#   summarise(had_decline = any(decline_indicator == 1, na.rm = TRUE)) %>%
#   summarise(tj_risk_rate = mean(had_decline, na.rm = TRUE)) %>%
#   pull(tj_risk_rate)
# 
# cat("\n  Overall TJ risk rate:", round(tj_risk_rate_overall * 100, 1), "%\n\n")
# 
# # ============================================================
# # STEP 10: RISK ASSESSMENT WITH RECOVERY
# # ============================================================
# 
# cat("Generating risk assessments...\n")
# 
# risk_assessments <- pitcher_data_final %>%
#   filter(!is_missing) %>%
#   group_by(player_id, player_name) %>%
#   summarise(
#     latest_year = last(year),
#     current_velocity = last(velocity),
#     current_age = last(age),
#     career_years = max(career_year),
#     recent_decline = ifelse(n() >= 2,
#                             last(velocity) - nth(velocity, -2, default = last(velocity)),
#                             0),
#     had_tommy_john = first(had_tommy_john),
#     tj_count = last(tj_count),
#     years_since_tj = ifelse(last(had_tommy_john) == 1, last(years_since_tj), NA),
#     .groups = 'drop'
#   ) %>%
#   mutate(
#     era_group = case_when(
#       latest_year <= 2014 ~ "2010-2014",
#       latest_year <= 2019 ~ "2015-2019",
#       TRUE ~ "2020+")
#   ) %>%
#   left_join(
#     era_benchmarks %>% select(era, avg_velocity_era = avg_velocity),
#     by = c("era_group" = "era")
#   ) %>%
#   left_join(
#     changepoint_results %>%
#       select(player_id, changepoint_confidence),
#     by = "player_id"
#   ) %>%
#   left_join(
#     tj_risk_by_count %>% select(tj_count, tj_risk = risk_rate),
#     by = "tj_count"
#   ) %>%
#   mutate(
#     # Risk factors (0-1 scale)
#     age_risk = pmax(0, pmin(1, (current_age - 26) / 10)),   #assuming 26 is considered peak age
#     velocity_risk = pmax(0, pmin(1, (avg_velocity_era - current_velocity) / 4)),
#     decline_risk = pmax(0, pmin(1, -recent_decline / abs(decline_threshold))),
#     changepoint_risk = ifelse(is.na(changepoint_confidence), 0, changepoint_confidence),
#     injury_risk = ifelse(is.na(tj_risk), tj_count * tj_risk_rate_overall, tj_risk),
#     recovery_risk = case_when(
#       years_since_tj <= 2 ~ 0.6,
#       years_since_tj <= 4 ~ 0.3,
#       TRUE ~ 0.0),
#     
#     # Overall risk (weighted combination)
#     overall_risk = (age_risk * 0.18) + 
#       (velocity_risk * 0.15) + 
#       (decline_risk * 0.20) + 
#       (changepoint_risk * 0.25) + 
#       (injury_risk * 0.17) +
#       (recovery_risk * 0.05),
#     
#     # Risk categories
#     risk_category = case_when(
#       overall_risk < 0.3 ~ "Low Risk",
#       overall_risk < 0.6 ~ "Moderate Risk",
#       TRUE ~ "High Risk"))
# 
# cat("\nRisk Distribution:\n")
# print(table(risk_assessments$risk_category))
# cat("\n")
# had_tommy_john = as.numeric(tj_count > 0)
# 
# # ============================================================
# # STEP 11: VALIDATION & FUTURE PREDICTIONS
# # ============================================================
# 
# cat("========== MODEL VALIDATION ==========\n\n")
# 
# validation_data <- modeling_data %>% filter(year <= 2023)
# train_data <- validation_data %>% filter(year <= 2021)
# test_data <- validation_data %>% filter(year >= 2022)
# 
# cat("Training data:", nrow(train_data), "observations\n")
# cat("Test data:", nrow(test_data), "observations\n\n")
# 
# # Velocity model validation
# cat("Validating velocity model...\n")
# velocity_model_train <- update(velocity_model, data = train_data, refresh = 100)
# predictions <- posterior_predict(velocity_model_train, newdata = test_data)
# predicted_means <- apply(predictions, 2, mean)
# 
# rmse <- sqrt(mean((test_data$velocity - predicted_means)^2, na.rm = TRUE))
# mae <- mean(abs(test_data$velocity - predicted_means), na.rm = TRUE)
# r_squared <- cor(predicted_means, test_data$velocity, use = "complete.obs")^2
# 
# cat("\nVelocity Model Validation (2022-2023):\n")
# cat("  RMSE:", round(rmse, 2), "mph\n")
# cat("  MAE:", round(mae, 2), "mph\n")
# cat("  R-squared:", round(r_squared, 3), "\n\n")
# 
# # Prediction error distribution
# residuals <- test_data$velocity - predicted_means
# cat("  Velocity prediction error distribution:\n")
# cat("    Min:", round(min(residuals, na.rm = TRUE), 2), "\n")
# cat("    Q1:", round(quantile(residuals, 0.25, na.rm = TRUE), 2), "\n")
# cat("    Median:", round(median(residuals, na.rm = TRUE), 2), "\n")
# cat("    Q3:", round(quantile(residuals, 0.75, na.rm = TRUE), 2), "\n")
# cat("    Max:", round(max(residuals, na.rm = TRUE), 2), "\n\n")
# 
# # Calibration check
# cat("  Calibration by tercile:\n")
# calibration <- data.frame(
#   actual = test_data$velocity,
#   predicted = predicted_means
# ) %>%
#   mutate(tercile = ntile(predicted, 3)) %>%
#   group_by(tercile) %>%
#   summarise(
#     avg_predicted = mean(predicted),
#     avg_actual = mean(actual),
#     .groups = 'drop')
# print(calibration)
# cat("\n")
# 
# # Generate future predictions (2024-2026)
# cat("Generating future predictions (2024-2026)...\n")
# future_years <- 2024:2026
# future_predictions <- list()
# 
# for(future_year in future_years) {
#   current_pitchers <- risk_assessments %>%
#     filter(latest_year == max(latest_year)) %>%
#     mutate(
#       year = future_year,
#       age = current_age + (future_year - latest_year),
#       years_since_tj_capped = ifelse(
#         is.na(years_since_tj), 
#         0,  # Non-TJ pitchers stay at 0
#         pmin(years_since_tj + (future_year - latest_year), 5)
#       )
#     )  # ← Close mutate HERE!
#   
#   # NOW run predictions (outside of mutate)
#   future_preds <- posterior_predict(velocity_model, newdata = current_pitchers)
#   
#   # NOW save results
#   future_predictions[[as.character(future_year)]] <- data.frame(
#     player_id = current_pitchers$player_id,
#     player_name = current_pitchers$player_name,
#     prediction_year = future_year,
#     predicted_age = current_pitchers$age,
#     predicted_velocity = apply(future_preds, 2, mean),
#     prediction_lower = apply(future_preds, 2, quantile, 0.025),
#     prediction_upper = apply(future_preds, 2, quantile, 0.975),
#     current_velocity = current_pitchers$current_velocity
#   )
# }
# 
# future_predictions_df <- bind_rows(future_predictions)
# 
# cat("Future predictions generated for", length(unique(future_predictions_df$player_id)), "pitchers\n\n")
# 
# # ============================================================
# # STEP 11.5: CASE STUDY EXAMPLES
# # ============================================================
# case_study_pitchers <- risk_assessments %>%
#   arrange(desc(overall_risk)) %>%
#   head(3) %>%
#   pull(player_id)
# 
# cat("Top 3 High-Risk Case Studies:\n")
# for(i in seq_along(case_study_pitchers)) {
#   pid <- case_study_pitchers[i]
#   
#   pitcher_info <- risk_assessments %>% filter(player_id == pid)
#   pitcher_history <- pitcher_data_final %>% 
#     filter(player_id == pid, !is_missing) %>%
#     select(player_name, year, age, velocity, tj_count, years_since_tj)
#   
#   cat("\n", i, ". ", pitcher_info$player_name, "\n", sep = "")
#   cat("   Current Age:", pitcher_info$current_age, "\n")
#   cat("   Current Velocity:", round(pitcher_info$current_velocity, 1), "mph\n")
#   cat("   TJ Surgeries:", pitcher_info$tj_count, "\n")
#   cat("   Years Since TJ:", ifelse(is.na(pitcher_info$years_since_tj), "N/A", pitcher_info$years_since_tj), "\n")
#   cat("   Overall Risk:", round(pitcher_info$overall_risk, 3), "(", pitcher_info$risk_category, ")\n")
#   cat("   Recent Decline:", round(pitcher_info$recent_decline, 2), "mph\n")
#   
#   # Check if changepoint detected
#   if(pitcher_info$player_id %in% changepoint_results$player_id) {
#     cp_info <- changepoint_results %>% filter(player_id == pid)
#     cat("   Changepoint Detected:", cp_info$changepoint_year, 
#         "(Confidence:", round(cp_info$changepoint_confidence, 2), ")\n")
#   }
# }
# cat("\n")
# 
# # ============================================================
# # STEP 12: VISUALIZATIONS
# # ============================================================
# 
# cat("========== GENERATING VISUALIZATIONS ==========\n\n")
# 
# # 1. Risk Distribution
# cat("Creating risk distribution plot...\n")
# p1 <- ggplot(risk_assessments, aes(x = risk_category, fill = risk_category)) +
#   geom_bar() +
#   scale_fill_manual(values = c("Low Risk" = "#2ecc71", 
#                                "Moderate Risk" = "#f39c12", 
#                                "High Risk" = "#e74c3c")) +
#   labs(title = "Pitcher Risk Distribution",
#        subtitle = "Based on 6-component Bayesian risk model",
#        x = "Risk Category",
#        y = "Number of Pitchers") +
#   theme_minimal() +
#   theme(legend.position = "none",
#         plot.title = element_text(size = 14, face = "bold"),
#         text = element_text(size = 12))
# 
# ggsave(paste0(output_dir, "risk_distribution.png"), p1, width = 8, height = 6, dpi = 300)
# 
# # 2. Velocity by Age with TJ Status
# cat("Creating velocity aging curve by TJ status...\n")
# aging_plot_data <- modeling_data %>%
#   mutate(tj_status = ifelse(had_tommy_john == 1, "Post-TJ", "No TJ History"))
# 
# p2 <- ggplot(aging_plot_data, aes(x = age, y = velocity, color = tj_status)) +
#   geom_point(alpha = 0.2, size = 1) +
#   geom_smooth(method = "loess", se = TRUE, linewidth = 1.5) +
#   scale_color_manual(values = c("No TJ History" = "#3498db", "Post-TJ" = "#e74c3c")) +
#   labs(title = "Velocity Aging Curves by TJ Status",
#        subtitle = "Hierarchical Bayesian model predictions",
#        x = "Age",
#        y = "Fastball Velocity (mph)",
#        color = "TJ Status") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 14, face = "bold"),
#         legend.position = "top",
#         text = element_text(size = 12))
# 
# ggsave(paste0(output_dir, "velocity_aging_curve.png"), p2, width = 10, height = 6, dpi = 300)
# 
# # 3. Changepoint Confidence Distribution
# cat("Creating changepoint confidence distribution...\n")
# p3 <- ggplot(changepoint_results, aes(x = changepoint_confidence)) +
#   geom_histogram(bins = 20, fill = "#9b59b6", alpha = 0.7, color = "black") +
#   labs(title = "Changepoint Detection Confidence Scores",
#        subtitle = paste0(nrow(changepoint_results), " velocity declines detected"),
#        x = "Confidence Score (0-1)",
#        y = "Count") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 14, face = "bold"),
#         text = element_text(size = 12))
# 
# ggsave(paste0(output_dir, "changepoint_confidence.png"), p3, width = 8, height = 6, dpi = 300)
# 
# # 4. Decline Magnitude Distribution
# cat("Creating decline magnitude distribution...\n")
# p4 <- ggplot(changepoint_results, aes(x = decline_magnitude)) +
#   geom_histogram(bins = 20, fill = "#e67e22", alpha = 0.7, color = "black") +
#   geom_vline(xintercept = median(changepoint_results$decline_magnitude), 
#              linetype = "dashed", color = "red", size = 1) +
#   labs(title = "Velocity Decline Magnitudes",
#        subtitle = paste0("Median decline: ", round(median(changepoint_results$decline_magnitude), 2), " mph"),
#        x = "Decline Magnitude (mph)",
#        y = "Count") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 14, face = "bold"),
#         text = element_text(size = 12))
# 
# ggsave(paste0(output_dir, "decline_magnitude.png"), p4, width = 8, height = 6, dpi = 300)
# 
# # 5. Risk Components Correlation Heatmap
# cat("Creating risk components correlation matrix...\n")
# risk_components <- risk_assessments %>%
#   select(age_risk, velocity_risk, decline_risk, changepoint_risk, injury_risk, recovery_risk) %>%
#   cor(use = "complete.obs")
# 
# risk_corr_melted <- melt(risk_components)
# 
# p5 <- ggplot(risk_corr_melted, aes(x = Var1, y = Var2, fill = value)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient2(low = "#3498db", high = "#e74c3c", mid = "white", 
#                        midpoint = 0, limit = c(-1, 1), name = "Correlation") +
#   geom_text(aes(label = round(value, 2)), size = 3) +
#   labs(title = "Risk Component Correlation Matrix",
#        x = "", y = "") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(size = 14, face = "bold"),
#         text = element_text(size = 10))
# 
# ggsave(paste0(output_dir, "risk_correlation_heatmap.png"), p5, width = 8, height = 6, dpi = 300)
# 
# # 6. Recovery Trajectory (Years Since TJ vs Velocity)
# cat("Creating recovery trajectory plot...\n")
# recovery_data <- modeling_data %>%
#   filter(had_tommy_john == 1, years_since_tj <= 5) %>%
#   group_by(years_since_tj) %>%
#   summarise(
#     mean_velocity = mean(velocity, na.rm = TRUE),
#     se_velocity = sd(velocity, na.rm = TRUE) / sqrt(n()),
#     n = n(),
#     .groups = 'drop'
#   )
# 
# p6 <- ggplot(recovery_data, aes(x = years_since_tj, y = mean_velocity)) +
#   geom_line(color = "#e74c3c", size = 1.5) +
#   geom_point(size = 3, color = "#c0392b") +
#   geom_errorbar(aes(ymin = mean_velocity - se_velocity, 
#                     ymax = mean_velocity + se_velocity), 
#                 width = 0.2, color = "#c0392b") +
#   geom_text(aes(label = paste0("n=", n)), vjust = -1, size = 3) +
#   labs(title = "Velocity Recovery After Tommy John Surgery",
#        subtitle = "Mean velocity by years post-TJ (with SE)",
#        x = "Years Since Tommy John Surgery",
#        y = "Mean Fastball Velocity (mph)") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 14, face = "bold"),
#         text = element_text(size = 12))
# 
# ggsave(paste0(output_dir, "recovery_trajectory.png"), p6, width = 10, height = 6, dpi = 300)
# 
# # 7. Validation: Predicted vs Actual
# cat("Creating validation scatter plot...\n")
# validation_plot_data <- data.frame(
#   actual = test_data$velocity,
#   predicted = predicted_means
# ) %>%
#   filter(!is.na(actual), !is.na(predicted))
# 
# p7 <- ggplot(validation_plot_data, aes(x = actual, y = predicted)) +
#   geom_point(alpha = 0.4, color = "#3498db") +
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
#   geom_smooth(method = "lm", se = TRUE, color = "#2ecc71") +
#   labs(title = "Model Validation: Predicted vs Actual Velocity",
#        subtitle = paste0("RMSE: ", round(rmse, 2), " mph | R² = ", round(r_squared, 3)),
#        x = "Actual Velocity (mph)",
#        y = "Predicted Velocity (mph)") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 14, face = "bold"),
#         text = element_text(size = 12))
# 
# ggsave(paste0(output_dir, "validation_scatter.png"), p7, width = 8, height = 6, dpi = 300)
# 
# # 8. Residual Distribution
# cat("Creating residual distribution plot...\n")
# p8 <- ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
#   geom_histogram(bins = 30, fill = "#9b59b6", alpha = 0.7, color = "black") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
#   labs(title = "Model Residual Distribution",
#        subtitle = paste0("Mean: ", round(mean(residuals, na.rm = TRUE), 3), 
#                          " | SD: ", round(sd(residuals, na.rm = TRUE), 2)),
#        x = "Residual (Actual - Predicted, mph)",
#        y = "Count") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 14, face = "bold"),
#         text = element_text(size = 12))
# 
# ggsave(paste0(output_dir, "residual_distribution.png"), p8, width = 8, height = 6, dpi = 300)
# 
# # 9. TJ Risk by Count
# cat("Creating TJ risk by count plot...\n")
# p9 <- ggplot(tj_risk_by_count, aes(x = factor(tj_count), y = risk_rate)) +
#   geom_col(fill = "#e74c3c", alpha = 0.7, color = "black") +
#   geom_text(aes(label = paste0(round(risk_rate * 100, 1), "%\n(n=", n_pitchers, ")")), 
#             vjust = -0.5, size = 3.5) +
#   labs(title = "Injury Risk by Tommy John Surgery Count",
#        subtitle = "Data-driven risk rates from observed decline patterns",
#        x = "Number of Tommy John Surgeries",
#        y = "Risk Rate (Probability of Decline)") +
#   scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 14, face = "bold"),
#         text = element_text(size = 12))
# 
# ggsave(paste0(output_dir, "tj_risk_by_count.png"), p9, width = 8, height = 6, dpi = 300)
# 
# # 10. Overall Risk Distribution (continuous)
# cat("Creating overall risk density plot...\n")
# p10 <- ggplot(risk_assessments, aes(x = overall_risk, fill = risk_category)) +
#   geom_density(alpha = 0.6) +
#   scale_fill_manual(values = c("Low Risk" = "#2ecc71", 
#                                "Moderate Risk" = "#f39c12", 
#                                "High Risk" = "#e74c3c")) +
#   geom_vline(xintercept = c(0.3, 0.6), linetype = "dashed", alpha = 0.5) +
#   labs(title = "Overall Risk Score Distribution",
#        subtitle = "Weighted combination of 6 risk factors",
#        x = "Overall Risk Score (0-1)",
#        y = "Density",
#        fill = "Risk Category") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 14, face = "bold"),
#         legend.position = "top",
#         text = element_text(size = 12))
# 
# 
# # ============================================================
# # STEP 13: SAVE ALL OUTPUTS
# # ============================================================
# output_dir <- "C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/UpdatedVelocityOnly/"
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
#   cat("Created output directory:", output_dir, "\n")
# }
# 
# saveRDS(changepoint_results, paste0(output_dir, "changepoint_results_velo_only.rds"))
# write_csv(changepoint_results, paste0(output_dir, "changepoint_results_velo_only.csv"))
# 
# saveRDS(risk_assessments, paste0(output_dir, "risk_assessments_velo_only.rds"))
# write_csv(risk_assessments, paste0(output_dir, "risk_assessments_velo_only.csv"))
# 
# saveRDS(velocity_model, paste0(output_dir, "velocity_model.rds"))
# 
# saveRDS(future_predictions_df, paste0(output_dir, "future_predictions_velo_only.rds"))
# write_csv(future_predictions_df, paste0(output_dir, "future_predictions_velo_only.csv"))
# 
# write_csv(tj_risk_by_count, paste0(output_dir, "tj_risk_by_count.csv"))
# 
# ggsave(paste0(output_dir, "overall_risk_density.png"), p10, width = 10, height = 6, dpi = 300)
# cat("\n✅All visualizations saved to:", output_dir, "\n\n")
# 
# 
# validation_summary <- data.frame(
#   metric = c("RMSE", "MAE", "R_squared"),
#   value = c(rmse, mae, r_squared)
# )
# write_csv(validation_summary, paste0(output_dir, "validation_summary_velo_only.csv"))
# 
# cat("All outputs saved to:", output_dir, "\n\n")
# 
# library(ggplot2)
# 
# # Function to create individual pitcher trajectory plot
# create_pitcher_plot <- function(pitcher_name_input, pitcher_data, model, changepoint_data, risk_data) {
#   
#   # Get pitcher ID
#   pitcher_id <- risk_data %>% 
#     filter(player_name == pitcher_name_input) %>% 
#     pull(player_id)
#   
#   # Get actual data
#   actual_data <- pitcher_data %>%
#     filter(player_name == pitcher_name_input, !is_missing) %>%
#     select(year, age, velocity, years_since_tj_capped)
#   
#   # Get changepoint info
#   cp_info <- changepoint_data %>%
#     filter(player_id == pitcher_id)
#   
#   # Get risk info
#   risk_info <- risk_data %>%
#     filter(player_id == pitcher_id)
#   
#   # Create prediction grid
#   age_range <- seq(min(actual_data$age), max(actual_data$age) + 3, by = 0.5)
#   pred_data <- data.frame(
#     age = age_range,
#     player_id = pitcher_id,
#     years_since_tj_capped = risk_info$years_since_tj_capped[1]
#   )
#   pred_data$years_since_tj_capped[is.na(pred_data$years_since_tj_capped)] <- 0
#   
#   # Get predictions
#   preds <- posterior_predict(model, newdata = pred_data)
#   pred_data$velocity <- apply(preds, 2, mean)
#   pred_data$lower <- apply(preds, 2, quantile, 0.025)
#   pred_data$upper <- apply(preds, 2, quantile, 0.975)
#   
#   # Create plot
#   p <- ggplot() +
#     # Predicted curve with uncertainty
#     geom_ribbon(data = pred_data, 
#                 aes(x = age, ymin = lower, ymax = upper),
#                 fill = "steelblue", alpha = 0.2) +
#     geom_line(data = pred_data,
#               aes(x = age, y = velocity),
#               color = "steelblue", size = 1.2) +
#     # Actual data points
#     geom_point(data = actual_data,
#                aes(x = age, y = velocity),
#                size = 3, color = "darkred", alpha = 0.7) +
#     # Changepoint marker
#     {if(nrow(cp_info) > 0) {
#       cp_age <- actual_data %>% 
#         filter(year == cp_info$changepoint_year) %>% 
#         pull(age)
#       geom_vline(xintercept = cp_age, 
#                  linetype = "dashed", 
#                  color = "red", 
#                  size = 1)
#     }} +
#     labs(
#       title = paste0(pitcher_name_input, " - Velocity Aging Trajectory"),
#       subtitle = paste0("Risk: ", round(risk_info$overall_risk * 100, 1), 
#                         "% (", risk_info$risk_category, ")"),
#       x = "Age",
#       y = "Fastball Velocity (mph)"
#     ) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(size = 14, face = "bold"),
#       plot.subtitle = element_text(size = 11),
#       text = element_text(size = 12)
#     )
#   
#   return(p)
# }
# 
# # Create plots for 3 case studies
# output_dir <- "C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/UpdatedVelocityOnly/"
# 
# cat("Creating case study plots...\n")
# 
# p_strasburg <- create_pitcher_plot("Strasburg, Stephen", pitcher_data_final, 
#                                    velocity_model, changepoint_results, risk_assessments)
# ggsave(paste0(output_dir, "case_study_strasburg.png"), p_strasburg, 
#        width = 10, height = 6, dpi = 300)
# 
# p_verlander <- create_pitcher_plot("Verlander, Justin", pitcher_data_final,
#                                    velocity_model, changepoint_results, risk_assessments)
# ggsave(paste0(output_dir, "case_study_verlander.png"), p_verlander,
#        width = 10, height = 6, dpi = 300)
# 
# p_hill <- create_pitcher_plot("Hill, Rich", pitcher_data_final,
#                               velocity_model, changepoint_results, risk_assessments)
# ggsave(paste0(output_dir, "case_study_hill.png"), p_hill,
#        width = 10, height = 6, dpi = 300)
# 
# cat("Case study plots saved!\n")
# 
# # ============================================================
# # FINAL SUMMARY & DOCUMENTATION
# # ============================================================
# 
# cat("========== ANALYSIS COMPLETE! ==========\n\n")
# 
# cat("📊 RESULTS SUMMARY:\n")
# cat("  Velocity changepoints detected:", nrow(changepoint_results), "\n")
# cat("  Pitchers assessed:", nrow(risk_assessments), "\n")
# cat("  - High risk pitchers:", sum(risk_assessments$risk_category == "High Risk"), "\n")
# cat("  - Moderate risk pitchers:", sum(risk_assessments$risk_category == "Moderate Risk"), "\n")
# cat("  - Low risk pitchers:", sum(risk_assessments$risk_category == "Low Risk"), "\n\n")
# 
# cat("🎯 MODEL PERFORMANCE:\n")
# cat("  Velocity model RMSE:", round(rmse, 2), "mph\n")
# cat("  Velocity model MAE:", round(mae, 2), "mph\n")
# cat("  Velocity model R²:", round(r_squared, 3), "\n")
# cat("  Future predictions: 2024-2026\n\n")
# 
# cat("📁 OUTPUT FILES CREATED:\n")
# cat("  1. changepoint_results_velo_only.csv - All detected velocity declines\n")
# cat("  2. risk_assessments_velo_only.csv - Risk scores for all pitchers\n")
# cat("  3. future_predictions_velo_only.csv - 2024-2026 velocity projections\n")
# cat("  4. tj_risk_by_count.csv - Data-driven TJ risk rates\n")
# cat("  5. validation_summary_velo_only.csv - Model performance metrics\n")
# cat("  6. velocity_model.rds - Saved Bayesian model object\n\n")
# 
# cat("📈 VISUALIZATIONS CREATED:\n")
# cat("  1. risk_distribution.png - Pitcher risk categories\n")
# cat("  2. velocity_aging_curve.png - Aging trajectories by TJ status\n")
# cat("  3. changepoint_confidence.png - Changepoint detection quality\n")
# cat("  4. decline_magnitude.png - Velocity drop distribution\n")
# cat("  5. risk_correlation_heatmap.png - Risk component relationships\n")
# cat("  6. recovery_trajectory.png - Post-TJ velocity recovery\n")
# cat("  7. validation_scatter.png - Predicted vs actual velocity\n")
# cat("  8. residual_distribution.png - Model error distribution\n")
# cat("  9. tj_risk_by_count.png - Risk by number of TJ surgeries\n")
# cat("  10. overall_risk_density.png - Continuous risk distribution\n\n")
# 
# cat("📋 KEY FINDINGS:\n")
# cat("  • Average velocity decline at changepoint:", 
#     round(mean(changepoint_results$decline_magnitude), 2), "mph\n")
# cat("  • Average years to decline:", 
#     round(mean(changepoint_results$career_years_to_decline), 1), "years\n")
# cat("  • TJ risk rate (overall):", 
#     round(tj_risk_rate_overall * 100, 1), "% of post-TJ pitchers experience measurable decline\n")
# cat("  • Validation RMSE:", round(rmse, 2), "mph indicates strong predictive accuracy\n\n")
# 
# cat("🎓 SABR PRESENTATION READY:\n")
# cat("  ✅ Bayesian hierarchical aging model with recovery trajectories\n")
# cat("  ✅ Changepoint detection for injury timing identification\n")
# cat("  ✅ Probabilistic risk assessment framework\n")
# cat("  ✅ Validation on 2022-2023 holdout data\n")
# cat("  ✅ Future predictions with uncertainty quantification\n")
# cat("  ✅ 10 publication-quality visualizations\n")
# cat("  ✅ Case studies of high-risk pitchers\n\n")
# 
# cat("🔑 MODEL INTERPRETATION:\n")
# cat("  • age_risk: Distance from peak age (26)\n")
# cat("  • velocity_risk: Current velo vs era-adjusted benchmark\n")
# cat("  • decline_risk: Recent velocity momentum\n")
# cat("  • changepoint_risk: Confidence in past performance regime shift\n")
# cat("  • injury_risk: Data-driven rate by TJ count\n")
# cat("  • recovery_risk: Uncertainty during post-TJ recovery (years 0-4)\n\n")
# 
# cat("📝 NEXT STEPS FOR SABR:\n")
# cat("  1. Review top 10 high-risk pitchers (below)\n")
# cat("  2. Examine case study details\n")
# cat("  3. Check all visualizations in output folder\n")
# cat("  4. Prepare 2-3 example narratives\n")
# cat("  5. Draft presentation slides\n\n")
# 
# cat("🎯 TOP 10 HIGHEST RISK PITCHERS:\n")
# print(risk_assessments %>%
#         arrange(desc(overall_risk)) %>%
#         select(player_name, latest_year, current_age, current_velocity, 
#                tj_count, years_since_tj, overall_risk, risk_category) %>%
#         head(10))
# 
# cat("\n")
# 
# beepr::beep(8)
# 
# 
# 
# # ============================================================
# # FINAL FIXES AND VISUALIZATION SCRIPT
# # ============================================================
# # This script fixes remaining issues and creates final outputs
# # Run this AFTER the main velocity model has completed
# 
# library(tidyverse)
# library(ggplot2)
# 
# output_dir <- "C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/UpdatedVelocityOnly/"
# 
# cat("\n========== FIXING RISK ASSESSMENTS TABLE ==========\n\n")
# 
# # FIX 1: Recalculate TJ flags correctly
# risk_assessments_fixed <- risk_assessments %>%
#   mutate(
#     # Recalculate had_tommy_john based on tj_count
#     had_tommy_john = as.numeric(tj_count > 0),
#     
#     # Fix years_since_tj: should be NA if no TJ, otherwise keep value
#     years_since_tj = ifelse(tj_count > 0, years_since_tj, NA)
#   )
# 
# # Verify the fix
# cat("Checking Michael Pineda example:\n")
# print(risk_assessments_fixed %>% 
#         filter(grepl("Pineda", player_name)) %>%
#         select(player_name, tj_count, had_tommy_john, years_since_tj))
# 
# # Save fixed version
# write_csv(risk_assessments_fixed, paste0(output_dir, "risk_assessments_FIXED.csv"))
# cat("\n✅ Fixed risk_assessments saved!\n\n")
# 
# # ============================================================
# # FIX 2: PIVOT FUTURE PREDICTIONS TO WIDE FORMAT
# # ============================================================
# 
# cat("========== PIVOTING FUTURE PREDICTIONS ==========\n\n")
# 
# future_predictions_wide <- future_predictions_df %>%
#   select(player_id, player_name, prediction_year, predicted_velocity, 
#          prediction_lower, prediction_upper, current_velocity) %>%
#   pivot_wider(
#     id_cols = c(player_id, player_name, current_velocity),
#     names_from = prediction_year,
#     values_from = c(predicted_velocity, prediction_lower, prediction_upper),
#     names_sep = "_"
#   ) %>%
#   # Rename columns for clarity
#   rename(
#     pred_velo_2024 = predicted_velocity_2024,
#     pred_velo_2025 = predicted_velocity_2025,
#     pred_velo_2026 = predicted_velocity_2026,
#     pred_lower_2024 = prediction_lower_2024,
#     pred_lower_2025 = prediction_lower_2025,
#     pred_lower_2026 = prediction_lower_2026,
#     pred_upper_2024 = prediction_upper_2024,
#     pred_upper_2025 = prediction_upper_2025,
#     pred_upper_2026 = prediction_upper_2026
#   ) %>%
#   # Calculate predicted changes
#   mutate(
#     change_2024_2025 = pred_velo_2025 - pred_velo_2024,
#     change_2025_2026 = pred_velo_2026 - pred_velo_2025,
#     total_change_2024_2026 = pred_velo_2026 - pred_velo_2024
#   )
# 
# write_csv(future_predictions_wide, paste0(output_dir, "future_predictions_WIDE.csv"))
# cat("✅ Wide-format predictions saved!\n\n")
# 
# # ============================================================
# # FIX 3: INDIVIDUAL PITCHER TRAJECTORY PLOTS
# # ============================================================
# 
# cat("========== CREATING CASE STUDY PLOTS ==========\n\n")
# 
# plot_pitcher_trajectory <- function(pitcher_name_input, 
#                                     pitcher_data, 
#                                     changepoint_data, 
#                                     risk_data,
#                                     tj_data_original) {
#   
#   # Get pitcher info
#   risk_info <- risk_data %>% filter(player_name == pitcher_name_input)
#   pitcher_id <- risk_info$player_id
#   
#   # Get actual velocity data
#   actual_data <- pitcher_data %>%
#     filter(player_name == pitcher_name_input, !is_missing) %>%
#     select(year, age, velocity) %>%
#     arrange(year)
#   
#   # Get changepoint info if exists
#   cp_info <- changepoint_data %>% filter(player_id == pitcher_id)
#   has_changepoint <- nrow(cp_info) > 0
#   
#   # Get TJ surgery years
#   tj_years <- tj_data_original %>%
#     filter(player_id == pitcher_id) %>%
#     pull(tj_year)
#   
#   # Create base plot
#   p <- ggplot(actual_data, aes(x = age, y = velocity)) +
#     # Actual data line
#     geom_line(color = "steelblue", size = 1.2, alpha = 0.7) +
#     geom_point(size = 3, color = "darkblue", alpha = 0.8) +
#     
#     # Add changepoint marker if exists
#     {if(has_changepoint) {
#       cp_age <- actual_data %>% 
#         filter(year == cp_info$changepoint_year) %>% 
#         pull(age)
#       list(
#         geom_vline(xintercept = cp_age, linetype = "dashed", 
#                    color = "red", size = 1.2),
#         annotate("text", x = cp_age, y = max(actual_data$velocity) - 0.5,
#                  label = paste0("Changepoint\n", cp_info$changepoint_year),
#                  color = "red", fontface = "bold", size = 3.5)
#       )
#     }} +
#     
#     # Add TJ markers if any
#     {if(length(tj_years) > 0) {
#       tj_ages <- actual_data %>% 
#         filter(year %in% tj_years) %>% 
#         pull(age)
#       list(
#         geom_point(data = actual_data %>% filter(year %in% tj_years),
#                    aes(x = age, y = velocity),
#                    color = "red", size = 5, shape = 4, stroke = 2),
#         annotate("text", x = tj_ages, 
#                  y = min(actual_data$velocity) + 0.5,
#                  label = "TJ", color = "red", fontface = "bold", size = 3)
#       )
#     }} +
#     
#     labs(
#       title = paste0(pitcher_name_input, " - Velocity Trajectory"),
#       subtitle = paste0("Risk: ", round(risk_info$overall_risk * 100, 1), 
#                         "% (", risk_info$risk_category, ") | ",
#                         "Current: ", round(risk_info$current_velocity, 1), " mph | ",
#                         "TJs: ", risk_info$tj_count),
#       x = "Age",
#       y = "Fastball Velocity (mph)",
#       caption = "Red X = Tommy John Surgery | Red dashed line = Detected changepoint"
#     ) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(size = 14, face = "bold"),
#       plot.subtitle = element_text(size = 10),
#       plot.caption = element_text(size = 8, hjust = 0),
#       text = element_text(size = 12),
#       panel.grid.minor = element_blank()
#     )
#   
#   return(p)
# }
# 
# # Create plots for all 4 case studies
# case_studies <- c("Hamels, Cole", "Bush, Matt", "Verlander, Justin", "Hill, Tim")
# 
# for(pitcher in case_studies) {
#   cat("Creating plot for", pitcher, "...\n")
#   
#   p <- plot_pitcher_trajectory(
#     pitcher, 
#     pitcher_data_final, 
#     changepoint_results, 
#     risk_assessments_fixed,
#     tj_pitchers_all
#   )
#   
#   filename <- paste0(output_dir, "case_study_", 
#                      gsub("[, ]", "_", tolower(pitcher)), ".png")
#   ggsave(filename, p, width = 10, height = 6, dpi = 300)
# }
# 
# cat("\n✅ All 4 case study plots created!\n\n")
# 
# # ============================================================
# # FIX 4: BETTER RISK DISTRIBUTION VISUALIZATION
# # ============================================================
# 
# cat("========== CREATING IMPROVED RISK VISUALIZATIONS ==========\n\n")
# 
# # Improved risk distribution - Donut chart
# risk_counts <- risk_assessments_fixed %>%
#   count(risk_category) %>%
#   mutate(
#     percentage = n / sum(n) * 100,
#     risk_category = factor(risk_category, 
#                            levels = c("Low Risk", "Moderate Risk", "High Risk"))
#   )
# 
# p_risk_donut <- ggplot(risk_counts, aes(x = 2, y = n, fill = risk_category)) +
#   geom_col(color = "white", size = 1) +
#   coord_polar(theta = "y") +
#   xlim(0.5, 2.5) +
#   scale_fill_manual(values = c("Low Risk" = "#2ecc71", 
#                                "Moderate Risk" = "#f39c12", 
#                                "High Risk" = "#e74c3c")) +
#   geom_text(aes(label = paste0(risk_category, "\n", n, " pitchers\n(", 
#                                round(percentage, 1), "%)")),
#             position = position_stack(vjust = 0.5),
#             color = "white", fontface = "bold", size = 4) +
#   labs(title = "Pitcher Risk Distribution",
#        subtitle = paste0("Total Pitchers Assessed: ", sum(risk_counts$n))) +
#   theme_void() +
#   theme(
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#     plot.subtitle = element_text(size = 12, hjust = 0.5),
#     legend.position = "none"
#   )
# 
# ggsave(paste0(output_dir, "risk_distribution_IMPROVED.png"), 
#        p_risk_donut, width = 8, height = 8, dpi = 300)
# 
# cat("✅ Improved risk distribution saved!\n\n")
# 
# # ============================================================
# # FIX 5: VELOCITY CHANGE BY AGE BAR GRAPH
# # ============================================================
# 
# cat("Creating velocity by age analysis...\n")
# 
# # Calculate mean velocity and change by age
# velocity_by_age <- modeling_data %>%
#   group_by(age) %>%
#   summarise(
#     mean_velocity = mean(velocity, na.rm = TRUE),
#     n = n(),
#     .groups = 'drop'
#   ) %>%
#   arrange(age) %>%
#   mutate(
#     velocity_change = mean_velocity - lag(mean_velocity),
#     age_group = case_when(
#       age < 26 ~ "Pre-Peak",
#       age >= 26 & age <= 30 ~ "Peak",
#       age > 30 ~ "Post-Peak"
#     )
#   ) %>%
#   filter(!is.na(velocity_change))  # Remove first row with NA
# 
# # Bar graph of velocity change by age
# p_velo_change <- ggplot(velocity_by_age, aes(x = age, y = velocity_change, fill = age_group)) +
#   geom_col(color = "black", alpha = 0.8) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
#   scale_fill_manual(values = c("Pre-Peak" = "#3498db", 
#                                "Peak" = "#2ecc71", 
#                                "Post-Peak" = "#e74c3c"),
#                     name = "Career Stage") +
#   labs(
#     title = "Average Velocity Change by Age",
#     subtitle = "Year-over-year velocity change across all pitchers",
#     x = "Age",
#     y = "Velocity Change (mph)",
#     caption = "Positive = gaining velocity | Negative = losing velocity"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 14, face = "bold"),
#     plot.caption = element_text(size = 9, hjust = 0),
#     legend.position = "top",
#     panel.grid.minor = element_blank()
#   )
# 
# ggsave(paste0(output_dir, "velocity_change_by_age.png"), 
#        p_velo_change, width = 10, height = 6, dpi = 300)
# 
# # Also create mean velocity by age (traditional aging curve)
# p_velo_by_age <- ggplot(velocity_by_age, aes(x = age, y = mean_velocity)) +
#   geom_line(color = "steelblue", size = 1.5) +
#   geom_point(aes(size = n), color = "darkblue", alpha = 0.6) +
#   geom_vline(xintercept = 26, linetype = "dashed", color = "green", size = 1) +
#   annotate("text", x = 26, y = max(velocity_by_age$mean_velocity),
#            label = "Peak Age (26)", color = "green", fontface = "bold") +
#   labs(
#     title = "Average Velocity by Age - The Aging Curve",
#     subtitle = "Based on 4,139 full-season observations",
#     x = "Age",
#     y = "Mean Fastball Velocity (mph)",
#     size = "Sample Size"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 14, face = "bold"),
#     legend.position = "bottom"
#   )
# 
# ggsave(paste0(output_dir, "aging_curve_by_age.png"), 
#        p_velo_by_age, width = 10, height = 6, dpi = 300)
# 
# cat("✅ Velocity by age visualizations created!\n\n")
# 
# # ============================================================
# # SUMMARY OF NEW OUTPUTS
# # ============================================================
# 
# cat("========== SUMMARY OF NEW OUTPUTS ==========\n\n")
# 
# cat("📁 NEW FILES CREATED:\n")
# cat("  1. risk_assessments_FIXED.csv - Corrected TJ logic\n")
# cat("  2. future_predictions_WIDE.csv - 2024/2025/2026 in columns\n")
# cat("  3. case_study_hamels_cole.png\n")
# cat("  4. case_study_bush_matt.png\n")
# cat("  5. case_study_verlander_justin.png\n")
# cat("  6. case_study_hill_tim.png\n")
# cat("  7. risk_distribution_IMPROVED.png - Donut chart\n")
# cat("  8. velocity_change_by_age.png - Bar graph of YoY changes\n")
# cat("  9. aging_curve_by_age.png - Traditional aging curve\n\n")
# 
# cat("📊 CASE STUDY SUMMARY:\n")
# for(pitcher in case_studies) {
#   info <- risk_assessments_fixed %>% filter(player_name == pitcher)
#   cat("\n", pitcher, ":\n", sep = "")
#   cat("  Age:", info$current_age, "| Velocity:", round(info$current_velocity, 1), "mph\n")
#   cat("  TJ Count:", info$tj_count, "| Risk:", round(info$overall_risk * 100, 1), 
#       "% (", info$risk_category, ")\n", sep = "")
# }
# 
# cat("\n\n✨ ALL FIXES COMPLETE! Ready for PowerPoint! ✨\n\n")
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
# 
# 
# 
# 
# 
# 
# 
# 
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
# # library(rstanarm)
# # library(changepoint)
# # library(beepr)
# # library(readxl)
# # library(lubridate)
# # set.seed(42)
# # 
# # cat("\n========== MLB PITCHER AGING MODEL - VELOCITY ONLY ==========\n\n")
# # 
# # stats <- read_csv("C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/stats.csv")
# # 
# # cat("  Raw stats shape:", nrow(stats), "rows,", ncol(stats), "columns\n")
# # cat("  Years covered:", min(stats$year), "to", max(stats$year), "\n")
# # cat("  Unique pitchers:", length(unique(stats$player_id)), "\n\n")
# # 
# # # Load Tommy John surgery data
# # tj_data <- read_excel("C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/CopyofTommyJohnSurgeryList.xlsx", 
# #                       skip = 1)
# # 
# # # Keep ALL TJ surgeries per pitcher
# # tj_pitchers_all <- tj_data %>%
# #   filter(Position == "P", !is.na(mlbamid)) %>%
# #   select(player_id = mlbamid, Player, `TJ Surgery Date`, Team, Age) %>%
# #   rename(tj_surgery_date = `TJ Surgery Date`, tj_age = Age) %>%
# #   mutate(
# #     player_id = as.integer(player_id),
# #     tj_year = year(tj_surgery_date)
# #   ) %>%
# #   arrange(player_id, tj_surgery_date)
# # 
# # # Create summary with TJ number for each pitcher
# # tj_summary <- tj_pitchers_all %>%
# #   group_by(player_id) %>%
# #   mutate(tj_number = row_number()) %>%
# #   ungroup() %>%
# #   select(player_id, tj_year, tj_number)
# # 
# # cat("  Total TJ surgeries for pitchers:", nrow(tj_pitchers_all), "\n")
# # cat("  Unique pitchers with TJ:", length(unique(tj_pitchers_all$player_id)), "\n")
# # cat("  TJ date range:", min(tj_pitchers_all$tj_surgery_date, na.rm = TRUE), 
# #     "to", max(tj_pitchers_all$tj_surgery_date, na.rm = TRUE), "\n\n")
# # 
# # # ============================================================
# # # STEP 2: CLEAN STATS DATA (VELOCITY ONLY)
# # # ============================================================
# # 
# # cat("Cleaning pitcher data...\n")
# # 
# # pitcher_stats <- stats %>%
# #   filter(
# #     !is.na(ff_avg_speed),                      # Must have fastball velocity
# #     !is.na(player_age),                        # Must have age
# #     ff_avg_speed >= 85 & ff_avg_speed <= 105  # Remove velocity outliers
# #   ) %>%
# #   mutate(
# #     player_name = str_trim(`last_name, first_name`),
# #     velocity = ff_avg_speed,
# #     age = player_age,
# #     # Track season quality
# #     season_type = case_when(
# #       p_formatted_ip >= 30 ~ "full_season",
# #       p_formatted_ip > 0 & p_formatted_ip < 30 ~ "partial_season",
# #       TRUE ~ "no_data"
# #     )
# #   ) %>%
# #   select(
# #     player_id, player_name, year, age, velocity, season_type, p_formatted_ip
# #   ) %>%
# #   arrange(player_id, year)
# # 
# # cat("  Cleaned stats shape:", nrow(pitcher_stats), "rows\n")
# # cat("  Season types:\n")
# # print(table(pitcher_stats$season_type))
# # cat("\n")
# # 
# # # ============================================================
# # # STEP 3: MERGE INJURY DATA & CALCULATE TJ_COUNT
# # # ============================================================
# # 
# # cat("Merging injury data...\n")
# # 
# # # For each pitcher-year, count how many TJs they've had BEFORE that season
# # pitcher_data_merged <- pitcher_stats %>%
# #   left_join(
# #     tj_summary %>%
# #       expand_grid(year = min(pitcher_stats$year):max(pitcher_stats$year)) %>%
# #       filter(year > tj_year) %>%
# #       group_by(player_id, year) %>%
# #       summarise(tj_count = n(), .groups = 'drop'),
# #     by = c("player_id", "year")
# #   ) %>%
# #   mutate(
# #     tj_count = replace_na(tj_count, 0),
# #     had_tommy_john = as.numeric(tj_count > 0)
# #   )
# # 
# # cat("  Pitchers with TJ history:", 
# #     length(unique(pitcher_data_merged$player_id[pitcher_data_merged$had_tommy_john > 0])), "\n\n")
# # 
# # # ============================================================
# # # STEP 4: HANDLE MISSING SEASONS
# # # ============================================================
# # 
# # cat("Identifying missing seasons...\n")
# # 
# # # Complete year sequences for each pitcher
# # pitcher_data_complete <- pitcher_data_merged %>%
# #   group_by(player_id) %>%
# #   filter(n() >= 3) %>%
# #   complete(year = min(year):max(year)) %>%
# #   ungroup()
# # 
# # # Fill in pitcher info for missing rows and identify injury years
# # pitcher_data_complete <- pitcher_data_complete %>%
# #   group_by(player_id) %>%
# #   fill(player_name, .direction = "downup") %>%
# #   mutate(
# #     # Mark missing vs observed data
# #     is_missing = is.na(velocity),
# #     
# #     # Use tj_count to detect injury years
# #     tj_count_filled = replace_na(tj_count, 0),
# #     tj_this_year = tj_count_filled > lag(tj_count_filled, default = 0),
# #     is_injury_year = tj_this_year,
# #     
# #     # Classify gap type
# #     gap_type = case_when(
# #       !is_missing ~ "data_present",
# #       is_injury_year ~ "injury_related",
# #       TRUE ~ "other_absence"),
# #     
# #     # Update had_tommy_john for filled rows
# #     had_tommy_john = as.numeric(tj_count_filled > 0)
# #   ) %>%
# #   select(-tj_this_year, -tj_count_filled) %>%
# #   ungroup()
# # 
# # cat("  Total rows after filling gaps:", nrow(pitcher_data_complete), "\n")
# # cat("  Gap types:\n")
# # print(table(pitcher_data_complete$gap_type))
# # cat("\n")
# # 
# # # ============================================================
# # # STEP 5: CALCULATE CAREER METRICS + RECOVERY TRAJECTORIES
# # # ============================================================
# # 
# # cat("Calculating career metrics and recovery trajectories...\n")
# # 
# # # Calculate data-driven decline threshold
# # normal_changes <- pitcher_data_complete %>%
# #   filter(!is_missing, !is_injury_year) %>%
# #   group_by(player_id) %>%
# #   arrange(year) %>%
# #   mutate(velocity_change = velocity - lag(velocity)) %>%
# #   ungroup() %>%
# #   filter(
# #     !is.na(velocity_change),
# #     velocity_change > -10 & velocity_change < 5
# #   ) %>%
# #   pull(velocity_change)
# # 
# # decline_threshold <- -2 * sd(normal_changes, na.rm = TRUE)
# # cat("  Data-driven decline threshold:", round(decline_threshold, 2), "mph\n\n")
# # 
# # # Add career metrics and recovery trajectories
# # pitcher_data_final <- pitcher_data_complete %>%
# #   group_by(player_id, player_name) %>%
# #   arrange(year) %>%
# #   mutate(
# #     # Career year
# #     career_year = year - min(year) + 1,
# #     
# #     # Create tj_event_year when tj_count increases
# #     tj_event_year = ifelse(tj_count > lag(tj_count, default = 0), year, NA)
# #   ) %>%
# #   fill(tj_event_year, .direction = "down") %>%
# #   mutate(
# #     # Calculate years since TJ
# #     years_since_tj = ifelse(!is.na(tj_event_year), year - tj_event_year, NA),
# #     years_since_tj_capped = ifelse(is.na(years_since_tj), 0, pmin(years_since_tj, 5)),
# #     
# #     
# #     # TIME-AWARE CHANGES
# #     years_since_last = ifelse(
# #       !is_missing,
# #       year - lag(year),
# #       NA
# #     ),
# #     
# #     # Velocity change per year
# #     velocity_change = ifelse(
# #       !is_missing & !is.na(lag(velocity)),
# #       (velocity - lag(velocity)) / years_since_last,
# #       NA
# #     ),
# #     
# #     # Decline indicator
# #     decline_indicator = ifelse(
# #       !is.na(velocity_change) & velocity_change < decline_threshold / 2,
# #       1, 0
# #     )
# #   ) %>%
# #   ungroup()
# # 
# # cat("  Final dataset shape:", nrow(pitcher_data_final), "rows\n")
# # cat("  Total pitchers:", length(unique(pitcher_data_final$player_id)), "\n\n")
# # 
# # # Save processed data
# # saveRDS(pitcher_data_final, 
# #         "C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/pitcher_data_final_velo_only.rds")
# # 
# # # ============================================================
# # # STEP 6: CHANGEPOINT DETECTION (VELOCITY ONLY)
# # # ============================================================
# # 
# # cat("\n========== CHANGEPOINT DETECTION ==========\n\n")
# # 
# # detect_changepoints <- function(pitcher_data) {
# #   changepoint_results <- list()
# #   
# #   pitcher_data_observed <- pitcher_data %>%
# #     filter(!is_missing) %>%
# #     group_by(player_id) %>%
# #     filter(n() >= 3) %>%
# #     ungroup()
# #   
# #   unique_pitcher_ids <- unique(pitcher_data_observed$player_id)
# #   
# #   cat("Detecting changepoints for", length(unique_pitcher_ids), "pitchers...\n")
# #   pb <- txtProgressBar(min = 0, max = length(unique_pitcher_ids), style = 3)
# #   
# #   for(i in seq_along(unique_pitcher_ids)) {
# #     pitcher_id <- unique_pitcher_ids[i]
# #     pitcher_subset <- pitcher_data_observed %>% 
# #       filter(player_id == pitcher_id) %>%
# #       arrange(year)
# #     
# #     pitcher_name <- first(pitcher_subset$player_name)
# #     
# #     if(nrow(pitcher_subset) >= 3) {
# #       ts_data <- pitcher_subset$velocity
# #       
# #       tryCatch({
# #         cpt_result <- cpt.mean(ts_data, method = "BinSeg", Q = 1, penalty = "BIC")
# #         changepoint_index <- cpts(cpt_result)
# #         
# #         if(length(changepoint_index) > 0) {
# #           changepoint_index <- changepoint_index[1]
# #           
# #           if(changepoint_index < length(ts_data)) {
# #             actual_year <- pitcher_subset$year[changepoint_index + 1]
# #             
# #             pre_decline_mean <- mean(ts_data[1:changepoint_index], na.rm = TRUE)
# #             post_decline_mean <- mean(ts_data[(changepoint_index+1):length(ts_data)], na.rm = TRUE)
# #             decline_magnitude <- pre_decline_mean - post_decline_mean
# #             
# #             if(decline_magnitude > 0.5) {  # 0.5 mph threshold
# #               sd_pre <- sd(ts_data[1:changepoint_index], na.rm = TRUE)
# #               sd_post <- sd(ts_data[(changepoint_index+1):length(ts_data)], na.rm = TRUE)
# #               pooled_sd <- sqrt((sd_pre^2 + sd_post^2) / 2)
# #               
# #               effect_size <- abs(decline_magnitude) / (pooled_sd + 0.01)
# #               changepoint_confidence <- pmin(0.95, pmax(0.5, effect_size / 3))
# #               
# #               changepoint_results[[as.character(pitcher_id)]] <- data.frame(
# #                 player_id = pitcher_id,
# #                 pitcher = pitcher_name,
# #                 changepoint_year = actual_year,
# #                 changepoint_confidence = changepoint_confidence,
# #                 pre_decline_velocity = pre_decline_mean,
# #                 post_decline_velocity = post_decline_mean,
# #                 decline_magnitude = decline_magnitude,
# #                 career_years_to_decline = changepoint_index
# #               )
# #             }
# #           }
# #         }
# #       }, error = function(e) NULL)
# #     }
# #     setTxtProgressBar(pb, i)
# #   }
# #   
# #   close(pb)
# #   return(bind_rows(changepoint_results))
# # }
# # 
# # cat("Detecting velocity changepoints...\n")
# # changepoint_results <- detect_changepoints(pitcher_data_final)
# # 
# # cat("\nChangepoint Summary:\n")
# # cat("  Velocity declines detected:", nrow(changepoint_results), "\n")
# # cat("  Average years to decline:", round(mean(changepoint_results$career_years_to_decline), 1), "\n")
# # cat("  Average decline magnitude:", round(mean(changepoint_results$decline_magnitude), 2), "mph\n\n")
# # 
# # # ============================================================
# # # STEP 7: BAYESIAN VELOCITY MODEL WITH RECOVERY
# # # ============================================================
# # 
# # cat("========== FITTING BAYESIAN MODEL ==========\n\n")
# # 
# # modeling_data <- pitcher_data_final %>%
# #   filter(!is_missing, season_type == "full_season")
# # 
# # cat("Fitting velocity aging model with recovery trajectories...\n")
# # cat("This will take ~10-12 minutes...\n\n")
# # 
# # velocity_model <- stan_lmer(
# #   velocity ~ age + I(age^2) + years_since_tj_capped + I(years_since_tj_capped^2) + (age | player_id),
# #   data = modeling_data,
# #   cores = 2,
# #   iter = 1000,
# #   chains = 2,
# #   refresh = 100)
# # 
# # cat("\nModel fitted successfully!\n\n")
# # 
# # # ============================================================
# # # STEP 8: CALCULATE ERA-DEPENDENT BENCHMARKS
# # # ============================================================
# # 
# # cat("Calculating era-dependent benchmarks...\n")
# # 
# # era_benchmarks <- modeling_data %>%
# #   mutate(
# #     era = case_when(
# #       year <= 2014 ~ "2010-2014",
# #       year <= 2019 ~ "2015-2019",
# #       TRUE ~ "2020+"
# #     )
# #   ) %>%
# #   group_by(era) %>%
# #   summarise(
# #     avg_velocity = mean(velocity, na.rm = TRUE),
# #     sd_velocity = sd(velocity, na.rm = TRUE),
# #     .groups = 'drop')
# # 
# # print(era_benchmarks)
# # cat("\n")
# # 
# # # ============================================================
# # # STEP 9: DATA-DRIVEN INJURY RISK BY TJ COUNT
# # # ============================================================
# # 
# # cat("Calculating data-driven injury risk by TJ count...\n")
# # 
# # tj_risk_by_count <- pitcher_data_final %>%
# #   filter(!is_missing, tj_count > 0) %>%
# #   group_by(player_id) %>%
# #   summarise(
# #     tj_count = last(tj_count),
# #     had_decline = any(decline_indicator == 1, na.rm = TRUE),
# #     .groups = 'drop'
# #   ) %>%
# #   group_by(tj_count) %>%
# #   summarise(
# #     n_pitchers = n(),
# #     risk_rate = mean(had_decline, na.rm = TRUE),
# #     .groups = 'drop'
# #   )
# # 
# # cat("  TJ Risk by Count:\n")
# # print(tj_risk_by_count)
# # 
# # tj_risk_rate_overall <- pitcher_data_final %>%
# #   filter(!is_missing, had_tommy_john == 1) %>%
# #   group_by(player_id) %>%
# #   summarise(had_decline = any(decline_indicator == 1, na.rm = TRUE)) %>%
# #   summarise(tj_risk_rate = mean(had_decline, na.rm = TRUE)) %>%
# #   pull(tj_risk_rate)
# # 
# # cat("\n  Overall TJ risk rate:", round(tj_risk_rate_overall * 100, 1), "%\n\n")
# # 
# # # ============================================================
# # # STEP 10: RISK ASSESSMENT WITH RECOVERY
# # # ============================================================
# # 
# # cat("Generating risk assessments...\n")
# # 
# # risk_assessments <- pitcher_data_final %>%
# #   filter(!is_missing) %>%
# #   group_by(player_id, player_name) %>%
# #   summarise(
# #     latest_year = last(year),
# #     current_velocity = last(velocity),
# #     current_age = last(age),
# #     career_years = max(career_year),
# #     recent_decline = ifelse(n() >= 2,
# #                             last(velocity) - nth(velocity, -2, default = last(velocity)),
# #                             0),
# #     had_tommy_john = first(had_tommy_john),
# #     tj_count = last(tj_count),
# #     years_since_tj = last(years_since_tj),
# #     .groups = 'drop'
# #   ) %>%
# #   mutate(
# #     era_group = case_when(
# #       latest_year <= 2014 ~ "2010-2014",
# #       latest_year <= 2019 ~ "2015-2019",
# #       TRUE ~ "2020+"
# #     )
# #   ) %>%
# #   left_join(
# #     era_benchmarks %>% select(era, avg_velocity_era = avg_velocity),
# #     by = c("era_group" = "era")
# #   ) %>%
# #   left_join(
# #     changepoint_results %>%
# #       select(player_id, changepoint_confidence),
# #     by = "player_id"
# #   ) %>%
# #   left_join(
# #     tj_risk_by_count %>% select(tj_count, tj_risk = risk_rate),
# #     by = "tj_count"
# #   ) %>%
# #   mutate(
# #     # Risk factors (0-1 scale)
# #     age_risk = pmax(0, pmin(1, (current_age - 26) / 10)),
# #     velocity_risk = pmax(0, pmin(1, (avg_velocity_era - current_velocity) / 4)),
# #     decline_risk = pmax(0, pmin(1, -recent_decline / abs(decline_threshold))),
# #     changepoint_risk = ifelse(is.na(changepoint_confidence), 0, changepoint_confidence),
# #     injury_risk = ifelse(is.na(tj_risk), tj_count * tj_risk_rate_overall, tj_risk),
# #     recovery_risk = case_when(
# #       years_since_tj <= 2 ~ 0.6,
# #       years_since_tj <= 4 ~ 0.3,
# #       TRUE ~ 0.0
# #     ),
# #     
# #     # Overall risk (weighted combination)
# #     overall_risk = (age_risk * 0.18) + 
# #       (velocity_risk * 0.15) + 
# #       (decline_risk * 0.20) + 
# #       (changepoint_risk * 0.25) + 
# #       (injury_risk * 0.17) +
# #       (recovery_risk * 0.05),
# #     
# #     # Risk categories
# #     risk_category = case_when(
# #       overall_risk < 0.3 ~ "Low Risk",
# #       overall_risk < 0.6 ~ "Moderate Risk",
# #       TRUE ~ "High Risk"
# #     )
# #   )
# # 
# # cat("\nRisk Distribution:\n")
# # print(table(risk_assessments$risk_category))
# # cat("\n")
# # 
# # # ============================================================
# # # STEP 11: VALIDATION & FUTURE PREDICTIONS
# # # ============================================================
# # 
# # cat("========== MODEL VALIDATION ==========\n\n")
# # 
# # validation_data <- modeling_data %>% filter(year <= 2023)
# # train_data <- validation_data %>% filter(year <= 2021)
# # test_data <- validation_data %>% filter(year >= 2022)
# # 
# # cat("Training data:", nrow(train_data), "observations\n")
# # cat("Test data:", nrow(test_data), "observations\n\n")
# # 
# # # Velocity model validation
# # cat("Validating velocity model...\n")
# # velocity_model_train <- update(velocity_model, data = train_data, refresh = 100)
# # predictions <- posterior_predict(velocity_model_train, newdata = test_data)
# # predicted_means <- apply(predictions, 2, mean)
# # 
# # rmse <- sqrt(mean((test_data$velocity - predicted_means)^2, na.rm = TRUE))
# # mae <- mean(abs(test_data$velocity - predicted_means), na.rm = TRUE)
# # r_squared <- cor(predicted_means, test_data$velocity, use = "complete.obs")^2
# # 
# # cat("\nVelocity Model Validation (2022-2023):\n")
# # cat("  RMSE:", round(rmse, 2), "mph\n")
# # cat("  MAE:", round(mae, 2), "mph\n")
# # cat("  R-squared:", round(r_squared, 3), "\n\n")
# # 
# # # Prediction error distribution
# # residuals <- test_data$velocity - predicted_means
# # cat("  Velocity prediction error distribution:\n")
# # cat("    Min:", round(min(residuals, na.rm = TRUE), 2), "\n")
# # cat("    Q1:", round(quantile(residuals, 0.25, na.rm = TRUE), 2), "\n")
# # cat("    Median:", round(median(residuals, na.rm = TRUE), 2), "\n")
# # cat("    Q3:", round(quantile(residuals, 0.75, na.rm = TRUE), 2), "\n")
# # cat("    Max:", round(max(residuals, na.rm = TRUE), 2), "\n\n")
# # 
# # # Calibration check
# # cat("  Calibration by tercile:\n")
# # calibration <- data.frame(
# #   actual = test_data$velocity,
# #   predicted = predicted_means
# # ) %>%
# #   mutate(tercile = ntile(predicted, 3)) %>%
# #   group_by(tercile) %>%
# #   summarise(
# #     avg_predicted = mean(predicted),
# #     avg_actual = mean(actual),
# #     .groups = 'drop')
# # print(calibration)
# # cat("\n")
# # 
# # # Generate future predictions (2024-2026)
# # cat("Generating future predictions (2024-2026)...\n")
# # future_years <- 2024:2026
# # future_predictions <- list()
# # 
# # for(future_year in future_years) {
# #   current_pitchers <- risk_assessments %>%
# #     filter(latest_year == max(latest_year)) %>%
# #     mutate(
# #       year = future_year,
# #       age = current_age + (future_year - latest_year),
# #       years_since_tj_capped = pmin(years_since_tj + (future_year - latest_year), 5))
# #   
# #   future_preds <- posterior_predict(velocity_model, newdata = current_pitchers)
# #   
# #   future_predictions[[as.character(future_year)]] <- data.frame(
# #     player_id = current_pitchers$player_id,
# #     player_name = current_pitchers$player_name,
# #     prediction_year = future_year,
# #     predicted_age = current_pitchers$age,
# #     predicted_velocity = apply(future_preds, 2, mean),
# #     prediction_lower = apply(future_preds, 2, quantile, 0.025),
# #     prediction_upper = apply(future_preds, 2, quantile, 0.975),
# #     current_velocity = current_pitchers$current_velocity
# #   )
# # }
# # 
# # future_predictions_df <- bind_rows(future_predictions)
# # 
# # cat("Future predictions generated for", length(unique(future_predictions_df$player_id)), "pitchers\n\n")
# # 
# # # ============================================================
# # # STEP 12: SAVE ALL OUTPUTS
# # # ============================================================
# # 
# # cat("========== SAVING OUTPUTS ==========\n\n")
# # 
# # output_dir <- "C:/Users/Owner/Desktop/PostGrad/DataProjects/BaseballAnalyticsMLB/MLB-Bayesian-Pitcher-Aging-And-Injury/"
# # 
# # saveRDS(changepoint_results, paste0(output_dir, "changepoint_results_velo_only.rds"))
# # write_csv(changepoint_results, paste0(output_dir, "changepoint_results_velo_only.csv"))
# # 
# # saveRDS(risk_assessments, paste0(output_dir, "risk_assessments_velo_only.rds"))
# # write_csv(risk_assessments, paste0(output_dir, "risk_assessments_velo_only.csv"))
# # 
# # saveRDS(velocity_model, paste0(output_dir, "velocity_model.rds"))
# # 
# # saveRDS(future_predictions_df, paste0(output_dir, "future_predictions_velo_only.rds"))
# # write_csv(future_predictions_df, paste0(output_dir, "future_predictions_velo_only.csv"))
# # 
# # write_csv(tj_risk_by_count, paste0(output_dir, "tj_risk_by_count.csv"))
# # 
# # validation_summary <- data.frame(
# #   metric = c("RMSE", "MAE", "R_squared"),
# #   value = c(rmse, mae, r_squared)
# # )
# # write_csv(validation_summary, paste0(output_dir, "validation_summary_velo_only.csv"))
# # 
# # cat("All outputs saved to:", output_dir, "\n\n")
# # 
# # # ============================================================
# # # FINAL SUMMARY
# # # ============================================================
# # 
# # cat("========== ANALYSIS COMPLETE! ==========\n\n")
# # 
# # cat("Results Summary:\n")
# # cat("  Velocity changepoints detected:", nrow(changepoint_results), "\n")
# # cat("  Pitchers assessed:", nrow(risk_assessments), "\n")
# # cat("  High risk pitchers:", sum(risk_assessments$risk_category == "High Risk"), "\n")
# # cat("  Moderate risk pitchers:", sum(risk_assessments$risk_category == "Moderate Risk"), "\n")
# # cat("  Low risk pitchers:", sum(risk_assessments$risk_category == "Low Risk"), "\n")
# # cat("  Velocity model RMSE:", round(rmse, 2), "mph\n")
# # cat("  Future predictions: 2024-2026\n\n")
# # 
# # cat("Top 10 Highest Risk Pitchers:\n")
# # print(risk_assessments %>%
# #         arrange(desc(overall_risk)) %>%
# #         select(player_name, latest_year, current_age, current_velocity, 
# #                tj_count, years_since_tj, overall_risk, risk_category) %>%
# #         head(10))
# # 
# # beepr::beep(8)
# # cat("\n🎯 DONE! Velocity-only analysis complete.\n\n")