library(tidyverse)
library(brms)
library(rstanarm)
library(changepoint)
library(plotly)
library(DT)
library(shiny)
library(shinydashboard)
library(beepr)
library(patchwork)

set.seed(42)

# --- Configuration & Data Loading ---
data_path <- "C:/Users/Owner/Desktop/WakeForest/WFUBaseball/AgingModel/pitcher_data_for_model_real_tj.csv"
pitcher_data_raw <- read_csv(data_path)


# --- 1. Data Preparation Function (Feature Engineering for Time-Series) ---
prepare_pitcher_data <- function(data) {
  # This function calculates year-over-year changes and final features needed for the models.
  pitcher_careers <- data %>%
    group_by(player_name) %>%
    # Filter: Only include pitchers with at least 3 years of career data
    filter(n() >= 3) %>%  
    arrange(year) %>%
    mutate(
      # CRITICAL: Create career_year variable (1, 2, 3, etc. for each pitcher)
      career_year = row_number(),
      
      # Command Proxy Conversion: Convert the loaded 'command_proxy' (which is BB% in the 
      # pre-processed data) to a 'command_score' where a higher value is better command.
      command_score = 1 - (command_proxy / 100),
      
      # Time-Series Change Metrics (Year-over-Year Decline/Improvement)
      # lag() calculates the value from the previous row within the current grouping (pitcher).
      velocity_change = velocity - lag(velocity, default = first(velocity)),
      movement_change = total_movement - lag(total_movement, default = first(total_movement)),
      command_change = command_score - lag(command_score, default = first(command_score)),
      
      # Simplified Stuff+ proxy: Standardized velocity plus standardized movement.
      stuff_plus = scale(velocity)[,1] + scale(total_movement)[,1],
      
      # Binary indicator for significant year-over-year velocity decline (e.g., > 0.5 MPH)
      decline_indicator = ifelse(velocity_change < -0.5, 1, 0)
    ) %>%
    ungroup()
  
  # Rename for consistency with original script variables used in other functions
  pitcher_careers <- pitcher_careers %>% rename(command_proxy = command_score)
  
  return(pitcher_careers)
}


# --- 2. Change Point Detection Function (Using changepoint package - FIXED!) ---
detect_changepoints <- function(pitcher_data, metric = "velocity") {
  changepoint_results <- list()
  unique_pitchers <- unique(pitcher_data$player_name)
  
  cat("Detecting changepoints for", length(unique_pitchers), "pitchers...\n")
  pb <- txtProgressBar(min = 0, max = length(unique_pitchers), style = 3)
  
  for(i in seq_along(unique_pitchers)) {
    pitcher <- unique_pitchers[i]
    pitcher_subset <- pitcher_data %>% 
      filter(player_name == pitcher) %>%
      arrange(year)
    
    # Requires at least 4 seasons to detect a changepoint
    if(nrow(pitcher_subset) >= 4) {
      ts_data <- pitcher_subset[[metric]]
      
      tryCatch({
        # Use changepoint::cpt.mean() for changepoint detection
        # This detects changes in the mean of the series
        cpt_result <- cpt.mean(ts_data, method = "BinSeg", Q = 1, penalty = "BIC")
        
        # Get the changepoint location
        changepoint_index <- cpts(cpt_result)
        
        # If a changepoint was detected
        if(length(changepoint_index) > 0) {
          changepoint_index <- changepoint_index[1]  # Take the first one if multiple
          
          # Only proceed if the changepoint is not at the very end
          if(changepoint_index < length(ts_data)) {
            actual_year <- pitcher_subset$year[changepoint_index]
            
            # Calculate pre- and post-decline means
            pre_decline_mean <- mean(ts_data[1:changepoint_index], na.rm = TRUE)
            post_decline_mean <- mean(ts_data[(changepoint_index+1):length(ts_data)], na.rm = TRUE)
            
            # Calculate decline magnitude
            decline_magnitude <- pre_decline_mean - post_decline_mean
            
            # Only include if there's a meaningful decline (> 0.5 units)
            if(decline_magnitude > 0.5) {
              # Estimate confidence based on the size of the change relative to variance
              sd_pre <- sd(ts_data[1:changepoint_index], na.rm = TRUE)
              sd_post <- sd(ts_data[(changepoint_index+1):length(ts_data)], na.rm = TRUE)
              pooled_sd <- sqrt((sd_pre^2 + sd_post^2) / 2)
              
              # Simple confidence measure: larger effect sizes = higher confidence
              # This is a pseudo-probability that mimics what bcp would have given us
              effect_size <- abs(decline_magnitude) / (pooled_sd + 0.01)  # Add small constant to avoid division by zero
              changepoint_prob <- pmin(0.95, pmax(0.5, effect_size / 3))
              
              changepoint_results[[pitcher]] <- data.frame(
                pitcher = pitcher,
                changepoint_year = actual_year,
                changepoint_probability = changepoint_prob,
                pre_decline_performance = pre_decline_mean,
                post_decline_performance = post_decline_mean,
                decline_magnitude = decline_magnitude,
                career_years_to_decline = changepoint_index,
                metric = metric
              )
            }
          }
        }
      }, error = function(e) {
        # Catch errors from changepoint function
        NULL
      })
    }
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  return(bind_rows(changepoint_results))
}


# --- 3. Hierarchical Bayesian Modeling Function (brms) ---
# UPDATED WITH REALISTIC PRIORS
fit_hierarchical_aging_model <- function(data, metric = "velocity", injury_vars = TRUE) {
  
  cat("\nFitting hierarchical Bayesian model for", metric, "...\n")
  
  # Construct the regression formula dynamically
  if(injury_vars && "injury_history" %in% names(data)) {
    # Full model with quadratic aging curve, injury terms, and random effects for pitcher
    formula_str <- paste0(metric, " ~ career_year + I(career_year^2) + injury_history + 
                          had_tommy_john + had_shoulder_injury + (career_year | player_name)")
  } else {
    # Simpler model without injury variables
    formula_str <- paste0(metric, " ~ career_year + I(career_year^2) + (career_year | player_name)")
  }
  
  # UPDATED PRIORS - More realistic for modern MLB (2015-2024)
  aging_model <- brm(
    as.formula(formula_str),
    data = data,
    family = gaussian(),
    prior = c(
      # Intercept: Average velocity for a pitcher in their FIRST year
      # Modern MLB: 94-95 MPH for starting pitchers
      prior(normal(94.5, 3), class = Intercept), 
      
      # Linear term: Initially slight decline, allows for individual variation
      prior(normal(-0.3, 0.3), class = b, coef = career_year),
      
      # Quadratic term: NEGATIVE (decline accelerates over time)
      # This creates the "stable early, then accelerating decline" pattern
      prior(normal(-0.05, 0.05), class = b, coef = Icareer_year2),
      
      # Standard deviations: Exponential priors (positive values)
      prior(exponential(0.5), class = sd),
      prior(exponential(0.5), class = sigma)
    ),
    chains = 4,
    iter = 2000,
    cores = 4,
    control = list(adapt_delta = 0.95),  # Helps with convergence
    silent = 2,
    refresh = 0
  )
  
  return(aging_model)
}

# --- 4. Future Decline Probability Calculation ---
calculate_decline_probabilities <- function(model, data, pitcher_name, future_years = 3) {
  pitcher_data <- data %>% filter(player_name == pitcher_name)
  
  if(nrow(pitcher_data) == 0) {
    return(NULL)
  }
  
  current_career_year <- max(pitcher_data$career_year)
  
  # Create data frame for prediction years
  future_data <- data.frame(
    career_year = (current_career_year + 1):(current_career_year + future_years),
    player_name = pitcher_name
  )
  
  # Carry forward the pitcher's injury status
  if("injury_history" %in% names(pitcher_data)) {
    future_data$injury_history <- first(pitcher_data$injury_history)
    future_data$had_tommy_john <- first(pitcher_data$had_tommy_john)
    future_data$had_shoulder_injury <- first(pitcher_data$had_shoulder_injury)
  }
  
  # Generate posterior predictive samples
  predictions <- posterior_predict(model, newdata = future_data, allow_new_levels = FALSE)
  
  # Get the most recent performance value as baseline
  current_performance <- tail(pitcher_data[[all.vars(formula(model))[1]]], 1)
  
  # Calculate probability of decline > 1 unit
  decline_probs <- apply(predictions, 2, function(x) {
    mean(x < current_performance - 1)  
  })
  
  return(data.frame(
    player_name = pitcher_name,
    future_career_year = future_data$career_year,
    predicted_mean = apply(predictions, 2, mean),
    decline_probability = decline_probs,
    uncertainty_lower_10 = apply(predictions, 2, quantile, 0.1),
    uncertainty_upper_90 = apply(predictions, 2, quantile, 0.9)
  ))
}


# --- 5. Risk Assessment Function ---
generate_risk_assessment <- function(changepoint_data, aging_models, current_data) {
  risk_scores <- current_data %>%
    group_by(player_name) %>%
    summarise(
      current_velocity = last(velocity),
      current_movement = last(total_movement),
      current_command = last(command_proxy),
      career_years = max(career_year), 
      recent_decline = last(velocity) - nth(velocity, -2, default = last(velocity)),
      injury_history = first(injury_history),
      had_tommy_john = first(had_tommy_john),
      had_shoulder_injury = first(had_shoulder_injury),
      had_elbow_injury = first(had_elbow_injury),
      .groups = 'drop'
    ) %>%
    left_join(
      changepoint_data %>% filter(metric == "velocity") %>% select(pitcher, changepoint_probability),
      by = c("player_name" = "pitcher")
    ) %>%
    mutate(
      # Risk factors (0-1 scale)
      age_risk = pmax(0, pmin(1, (career_years - 6) / 9)),
      velocity_risk = pmax(0, pmin(1, (93 - current_velocity) / 6)),
      decline_risk = pmax(0, pmin(1, -recent_decline / 2.5)),
      changepoint_risk = ifelse(is.na(changepoint_probability), 0, changepoint_probability),
      injury_risk = pmin(1, (injury_history * 0.15 + had_tommy_john * 0.25 + 
                               had_shoulder_injury * 0.2 + had_elbow_injury * 0.15)),
      
      # Overall risk score (weighted average)
      overall_risk = (age_risk * 0.2 + velocity_risk * 0.15 + decline_risk * 0.2 + 
                        changepoint_risk * 0.25 + injury_risk * 0.2),
      
      # Risk categories
      risk_category = case_when(
        overall_risk < 0.3 ~ "Low Risk",
        overall_risk < 0.6 ~ "Moderate Risk", 
        TRUE ~ "High Risk"
      )
    )
  
  return(risk_scores)
}

# --- 6. Model Validation Function ---
validate_model_performance <- function(historical_data, model) {
  validation_years <- unique(historical_data$year)
  
  if(length(validation_years) < 4) {
    return(list(rmse = NA, mae = NA, calibration = NA))
  }
  
  cutoff_year <- validation_years[length(validation_years) - 2]
  
  train_data <- historical_data %>% filter(year <= cutoff_year)
  test_data <- historical_data %>% filter(year > cutoff_year)
  
  if(nrow(test_data) < 10) {
    return(list(rmse = NA, mae = NA, calibration = NA))
  }
  
  tryCatch({
    validation_model <- update(model, newdata = train_data, refresh = 0)
    predictions <- posterior_predict(validation_model, newdata = test_data)
    predicted_means <- apply(predictions, 2, mean)
    
    rmse <- sqrt(mean((test_data$velocity - predicted_means)^2, na.rm = TRUE))
    mae <- mean(abs(test_data$velocity - predicted_means), na.rm = TRUE)
    calibration <- cor(predicted_means, test_data$velocity, use = "complete.obs")
    
    return(list(
      rmse = rmse,
      mae = mae,
      calibration = calibration
    ))
  }, error = function(e) {
    return(list(rmse = NA, mae = NA, calibration = NA))
  })
}

# ========== MAIN EXECUTION BLOCK ==========

cat("========== BAYESIAN PITCHER AGING & DECLINE DETECTION MODEL ==========\n\n")

cat("Step 1: Loading and preparing data...\n")
pitcher_data <- prepare_pitcher_data(pitcher_data_raw) 

cat("Total pitchers:", length(unique(pitcher_data$player_name)), "\n")
cat("Total observations:", nrow(pitcher_data), "\n\n")

cat("Step 2: Detecting changepoints...\n")
changepoint_velocity <- detect_changepoints(pitcher_data, "velocity")
changepoint_movement <- detect_changepoints(pitcher_data, "total_movement")
changepoint_command <- detect_changepoints(pitcher_data, "command_proxy")

changepoint_results <- bind_rows(changepoint_velocity, changepoint_movement, changepoint_command)

cat("\nStep 3: Fitting hierarchical aging models (This will take 15-20 minutes)...\n")
cat("NOTE: Updated priors for modern MLB:\n")
cat("  - Starting velocity: ~94.5 MPH\n")
cat("  - Decline pattern: Stable early career, accelerating after year 6-7\n\n")

velocity_model <- fit_hierarchical_aging_model(pitcher_data, "velocity", injury_vars = TRUE)
movement_model <- fit_hierarchical_aging_model(pitcher_data, "total_movement", injury_vars = TRUE)
command_model <- fit_hierarchical_aging_model(pitcher_data, "command_proxy", injury_vars = FALSE)

aging_models <- list(
  velocity = velocity_model,
  movement = movement_model,
  command = command_model
)

cat("\nStep 4: Generating risk assessments...\n")
risk_assessments <- generate_risk_assessment(changepoint_results, aging_models, pitcher_data)

cat("\nStep 5: Validating models...\n")
validation_velocity <- validate_model_performance(pitcher_data, velocity_model)

cat("\n========== MODEL RESULTS ==========\n\n")

cat("Velocity Model Validation (Predictive Accuracy on Held-Out Data):\n")
cat("  RMSE:", round(validation_velocity$rmse, 2), "MPH\n")
cat("  MAE:", round(validation_velocity$mae, 2), "MPH\n")
cat("  Correlation:", round(validation_velocity$calibration, 3), "\n\n")

cat("Changepoint Detection Summary:\n")
cat("  Velocity declines detected:", sum(changepoint_results$metric == "velocity"), "pitchers\n")
cat("  Movement declines detected:", sum(changepoint_results$metric == "total_movement"), "pitchers\n")
cat("  Command declines detected:", sum(changepoint_results$metric == "command_proxy"), "pitchers\n")
cat("  Average years to decline:", round(mean(changepoint_results$career_years_to_decline, na.rm = TRUE), 1), "\n")
cat("  Average decline magnitude:", round(mean(changepoint_results$decline_magnitude, na.rm = TRUE), 2), "\n\n")

cat("Risk Distribution:\n")
print(table(risk_assessments$risk_category))

cat("\n\nHigh Risk Pitchers (Top 10):\n")
print(head(risk_assessments %>% 
             arrange(desc(overall_risk)) %>% 
             select(player_name, current_velocity, career_years, overall_risk, risk_category), 10))

cat("\n\nSaving models and results...\n")
saveRDS(aging_models, "C:/Users/Owner/Desktop/WakeForest/WFUBaseball/AgingModel/aging_models.rds")
saveRDS(pitcher_data, "C:/Users/Owner/Desktop/WakeForest/WFUBaseball/AgingModel/pitcher_data.rds")
saveRDS(changepoint_results, "C:/Users/Owner/Desktop/WakeForest/WFUBaseball/AgingModel/changepoint_results.rds")
saveRDS(risk_assessments, "C:/Users/Owner/Desktop/WakeForest/WFUBaseball/AgingModel/risk_assessments.rds")

cat("\n========== MODEL COMPLETE ==========\n")
beepr::beep(8)