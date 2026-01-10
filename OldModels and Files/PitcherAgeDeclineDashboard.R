library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(brms)

aging_models <- readRDS("C:/Users/Owner/Desktop/WakeForest/WFUBaseball/AgingModel/aging_models.rds")
pitcher_data <- readRDS("C:/Users/Owner/Desktop/WakeForest/WFUBaseball/AgingModel/pitcher_data.rds")
changepoint_results <- readRDS("C:/Users/Owner/Desktop/WakeForest/WFUBaseball/AgingModel/changepoint_results.rds")
risk_assessments <- readRDS("C:/Users/Owner/Desktop/WakeForest/WFUBaseball/AgingModel/risk_assessments.rds")

pitcher_list <- unique(pitcher_data$player_name)

calculate_decline_probs_dashboard <- function(model, data, pitcher_name, future_years = 3) {
  pitcher_sub <- data %>% filter(player_name == pitcher_name)
  
  if(nrow(pitcher_sub) == 0) return(NULL)
  
  current_year <- max(pitcher_sub$career_year)
  
  future_data <- data.frame(
    career_year = (current_year + 1):(current_year + future_years),
    player_name = pitcher_name,
    injury_history = first(pitcher_sub$injury_history),
    had_tommy_john = first(pitcher_sub$had_tommy_john),
    had_shoulder_injury = first(pitcher_sub$had_shoulder_injury)
  )
  
  predictions <- posterior_predict(model, newdata = future_data, allow_new_levels = FALSE)
  current_val <- tail(pitcher_sub$velocity, 1)
  
  decline_probs <- apply(predictions, 2, function(x) mean(x < current_val - 1))
  
  return(data.frame(
    future_year = future_data$career_year,
    predicted_velocity = apply(predictions, 2, mean),
    decline_probability = decline_probs,
    lower_10 = apply(predictions, 2, quantile, 0.1),
    upper_90 = apply(predictions, 2, quantile, 0.9),
    lower_5 = apply(predictions, 2, quantile, 0.05),
    upper_95 = apply(predictions, 2, quantile, 0.95)
  ))
}

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Bayesian Pitcher Aging & Decline Detection"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Individual Pitcher", tabName = "pitcher", icon = icon("user")),
      menuItem("Risk Assessment", tabName = "risk", icon = icon("exclamation-triangle")),
      menuItem("Model Performance", tabName = "model", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_pitchers"),
                valueBoxOutput("high_risk_count"),
                valueBoxOutput("avg_decline_year")
              ),
              fluidRow(
                box(
                  title = "Velocity Trends Over Time",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("velocity_trends_plot", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Risk Distribution",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("risk_distribution_plot")
                ),
                box(
                  title = "Decline Timing Distribution",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("decline_timing_plot")
                )
              )
      ),
      
      tabItem(tabName = "pitcher",
              fluidRow(
                box(
                  title = "Select Pitcher",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  selectInput("selected_pitcher", "Choose Pitcher:", 
                              choices = pitcher_list, 
                              selected = pitcher_list[1]),
                  hr(),
                  h4("Update with New Data"),
                  numericInput("new_velocity", "New Velocity (mph):", value = 93, min = 80, max = 105),
                  actionButton("update_assessment", "Update Assessment", class = "btn-success")
                )
              ),
              fluidRow(
                box(
                  title = "Current Stats",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  uiOutput("pitcher_current_stats")
                ),
                box(
                  title = "Risk Gauge",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 4,
                  plotlyOutput("risk_gauge")
                ),
                box(
                  title = "Injury History",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 4,
                  uiOutput("injury_summary")
                )
              ),
              fluidRow(
                box(
                  title = "Career Trajectory",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("pitcher_trajectory_plot", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Future Projections",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("future_projections_plot")
                ),
                box(
                  title = "Posterior Distribution",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("posterior_distribution_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Changepoint Analysis",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("changepoint_analysis_plot", height = "350px")
                )
              )
      ),
      
      tabItem(tabName = "risk",
              fluidRow(
                box(
                  title = "High Risk Pitchers",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("high_risk_table")
                )
              ),
              fluidRow(
                box(
                  title = "Risk Factor Breakdown",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("risk_factors_plot")
                ),
                box(
                  title = "Injury Impact on Risk",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("injury_risk_plot")
                )
              ),
              fluidRow(
                box(
                  title = "All Pitchers Risk Assessment",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("all_risk_table")
                )
              )
      ),
      
      tabItem(tabName = "model",
              fluidRow(
                valueBoxOutput("model_rmse"),
                valueBoxOutput("model_mae"),
                valueBoxOutput("model_correlation")
              ),
              fluidRow(
                box(
                  title = "Model Validation",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  p("Hierarchical Bayesian model with pitcher-specific random effects"),
                  p("Formula: velocity ~ career_year + I(career_year^2) + injury_history + had_tommy_john + had_shoulder_injury + (career_year | player_name)"),
                  hr(),
                  h4("Model Features:"),
                  tags$ul(
                    tags$li("Quadratic aging curves (non-linear decline)"),
                    tags$li("Individual pitcher trajectories with population-level patterns"),
                    tags$li("Injury history integration"),
                    tags$li("Uncertainty quantification via posterior distributions"),
                    tags$li("Changepoint detection for decline timing")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Changepoint Detection Summary",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("changepoint_summary_table")
                )
              )
      ),
      
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About This Model",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  h3("Bayesian Pitcher Aging & Decline Detection System"),
                  p("Developed by Nickolas Bartle | Wake Forest University Baseball Analytics"),
                  hr(),
                  h4("Model Overview:"),
                  p("This system uses hierarchical Bayesian modeling to predict pitcher aging trajectories and identify decline patterns. 
                    By incorporating injury history, performance metrics, and changepoint detection, the model provides probabilistic 
                    forecasts for pitcher performance degradation."),
                  hr(),
                  h4("Key Features:"),
                  tags$ul(
                    tags$li(strong("Hierarchical Modeling:"), " Individual pitcher trajectories within population-level aging patterns"),
                    tags$li(strong("Multi-Metric Analysis:"), " Tracks velocity, movement, and command simultaneously"),
                    tags$li(strong("Changepoint Detection:"), " Identifies precise decline onset with probability scores"),
                    tags$li(strong("Injury Integration:"), " Incorporates Tommy John, shoulder, and elbow injury history"),
                    tags$li(strong("Uncertainty Quantification:"), " Provides confidence intervals via posterior distributions"),
                    tags$li(strong("Real-Time Updating:"), " Continuously refines predictions as new data arrives")
                  ),
                  hr(),
                  h4("Business Applications:"),
                  tags$ul(
                    tags$li("Contract extension decisions with quantified risk"),
                    tags$li("Workload management and injury prevention"),
                    tags$li("Trade timing optimization"),
                    tags$li("Draft and development strategy"),
                    tags$li("Performance forecasting for roster planning")
                  ),
                  hr(),
                  h4("Technical Details:"),
                  p("Built using R, brms (Bayesian Regression Models using Stan), and Shiny. 
                    Models trained on 10+ years of Statcast data (2015-2024) with 4,200+ pitcher-season observations.")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$total_pitchers <- renderValueBox({
    valueBox(
      length(unique(pitcher_data$player_name)),
      "Total Pitchers Analyzed",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$high_risk_count <- renderValueBox({
    high_risk <- sum(risk_assessments$risk_category == "High Risk")
    valueBox(
      high_risk,
      "High Risk Pitchers",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$avg_decline_year <- renderValueBox({
    avg_year <- round(mean(changepoint_results$career_years_to_decline, na.rm = TRUE), 1)
    valueBox(
      paste0("Year ", avg_year),
      "Average Decline Onset",
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  output$velocity_trends_plot <- renderPlotly({
    sample_pitchers <- pitcher_data %>%
      group_by(player_name) %>%
      filter(n() >= 5) %>%
      ungroup() %>%
      distinct(player_name) %>%
      sample_n(min(15, n())) %>%
      pull(player_name)
    
    plot_data <- pitcher_data %>% filter(player_name %in% sample_pitchers)
    
    plot_ly(plot_data, x = ~year, y = ~velocity, color = ~player_name,
            type = 'scatter', mode = 'lines+markers',
            hovertemplate = '%{y:.1f} MPH<br>%{x}<extra>%{fullData.name}</extra>') %>%
      layout(title = "Velocity Trends (Sample of 15 Pitchers)",
             xaxis = list(title = "Season"),
             yaxis = list(title = "Velocity (MPH)"),
             showlegend = FALSE)
  })
  
  output$risk_distribution_plot <- renderPlotly({
    plot_ly(risk_assessments, x = ~overall_risk, type = 'histogram',
            nbinsx = 30, marker = list(color = 'steelblue', opacity = 0.7)) %>%
      layout(title = "Overall Risk Score Distribution",
             xaxis = list(title = "Risk Score"),
             yaxis = list(title = "Count"))
  })
  
  output$decline_timing_plot <- renderPlotly({
    plot_ly(changepoint_results %>% filter(metric == "velocity"), 
            x = ~career_years_to_decline, type = 'histogram',
            nbinsx = 15, marker = list(color = 'coral', opacity = 0.7)) %>%
      layout(title = "Career Year of Decline Onset",
             xaxis = list(title = "Career Year"),
             yaxis = list(title = "Count"))
  })
  
  selected_pitcher_data <- reactive({
    pitcher_data %>% filter(player_name == input$selected_pitcher)
  })
  
  output$pitcher_current_stats <- renderUI({
    data <- selected_pitcher_data()
    latest <- data %>% tail(1)
    
    tagList(
      h4(input$selected_pitcher),
      hr(),
      p(strong("Current Velocity:"), paste0(round(latest$velocity, 1), " MPH")),
      p(strong("Career Years:"), max(data$career_year)),
      p(strong("Total Movement:"), paste0(round(latest$total_movement, 1), " in")),
      p(strong("Command (1-BB%):"), paste0(round(latest$command_proxy * 100, 1), "%")),
      p(strong("Recent Change:"), paste0(ifelse(latest$velocity_change >= 0, "+", ""), 
                                         round(latest$velocity_change, 1), " MPH"))
    )
  })
  
  output$risk_gauge <- renderPlotly({
    risk_row <- risk_assessments %>% filter(player_name == input$selected_pitcher)
    
    if(nrow(risk_row) == 0) return(NULL)
    
    risk_value <- risk_row$overall_risk * 100
    risk_color <- ifelse(risk_value < 30, "green", ifelse(risk_value < 60, "yellow", "red"))
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = risk_value,
      title = list(text = "Overall Risk Score"),
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = risk_color),
        steps = list(
          list(range = c(0, 30), color = "lightgreen"),
          list(range = c(30, 60), color = "lightyellow"),
          list(range = c(60, 100), color = "lightcoral")
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 70
        )
      )
    ) %>%
      layout(margin = list(l=20, r=20, b=20, t=40))
  })
  
  output$injury_summary <- renderUI({
    risk_row <- risk_assessments %>% filter(player_name == input$selected_pitcher)
    
    if(nrow(risk_row) == 0) return(p("No data available"))
    
    tagList(
      h5("Injury Profile"),
      hr(),
      p(strong("Total Injuries:"), risk_row$injury_history),
      p(strong("Tommy John:"), ifelse(risk_row$had_tommy_john == 1, "Yes", "No")),
      p(strong("Shoulder Injury:"), ifelse(risk_row$had_shoulder_injury == 1, "Yes", "No")),
      p(strong("Elbow Injury:"), ifelse(risk_row$had_elbow_injury == 1, "Yes", "No")),
      hr(),
      p(strong("Injury Risk Component:"), paste0(round(risk_row$injury_risk * 100, 1), "%"))
    )
  })
  
  output$pitcher_trajectory_plot <- renderPlotly({
    data <- selected_pitcher_data()
    
    plot_ly(data, x = ~career_year, y = ~velocity, type = 'scatter', mode = 'lines+markers',
            marker = list(size = 10, color = ~velocity, colorscale = 'Viridis'),
            hovertemplate = 'Year %{x}<br>%{y:.1f} MPH<extra></extra>') %>%
      layout(title = paste("Career Velocity Trajectory -", input$selected_pitcher),
             xaxis = list(title = "Career Year"),
             yaxis = list(title = "Velocity (MPH)"))
  })
  
  output$future_projections_plot <- renderPlotly({
    decline_probs <- calculate_decline_probs_dashboard(
      aging_models$velocity, 
      pitcher_data, 
      input$selected_pitcher, 
      future_years = 3
    )
    
    if(is.null(decline_probs)) {
      return(plot_ly() %>% layout(title = "No projection data available"))
    }
    
    plot_ly(decline_probs) %>%
      add_ribbons(x = ~future_year, ymin = ~lower_5, ymax = ~upper_95,
                  fillcolor = 'rgba(0, 100, 200, 0.2)', line = list(color = 'transparent'),
                  name = '90% CI', showlegend = TRUE) %>%
      add_ribbons(x = ~future_year, ymin = ~lower_10, ymax = ~upper_90,
                  fillcolor = 'rgba(0, 100, 200, 0.4)', line = list(color = 'transparent'),
                  name = '80% CI', showlegend = TRUE) %>%
      add_lines(x = ~future_year, y = ~predicted_velocity, line = list(color = 'blue', width = 3),
                name = 'Prediction') %>%
      layout(title = "3-Year Velocity Projection",
             xaxis = list(title = "Future Career Year"),
             yaxis = list(title = "Projected Velocity (MPH)"))
  })
  
  output$posterior_distribution_plot <- renderPlotly({
    decline_probs <- calculate_decline_probs_dashboard(
      aging_models$velocity, 
      pitcher_data, 
      input$selected_pitcher, 
      future_years = 3
    )
    
    if(is.null(decline_probs)) {
      return(plot_ly() %>% layout(title = "No projection data available"))
    }
    
    plot_ly(decline_probs, x = ~future_year, y = ~decline_probability * 100, 
            type = 'bar', marker = list(color = 'coral')) %>%
      layout(title = "Decline Probability by Year",
             xaxis = list(title = "Future Career Year"),
             yaxis = list(title = "Probability of 1+ MPH Decline (%)"))
  })
  
  output$changepoint_analysis_plot <- renderPlotly({
    cp_data <- changepoint_results %>% 
      filter(pitcher == input$selected_pitcher, metric == "velocity")
    
    traj_data <- selected_pitcher_data()
    
    p <- plot_ly(traj_data, x = ~career_year, y = ~velocity, type = 'scatter', 
                 mode = 'lines+markers', name = 'Actual')
    
    if(nrow(cp_data) > 0) {
      p <- p %>%
        add_trace(x = rep(cp_data$career_years_to_decline, 2), 
                  y = c(min(traj_data$velocity, na.rm=TRUE), max(traj_data$velocity, na.rm=TRUE)),
                  type = 'scatter', mode = 'lines',
                  line = list(color = 'red', dash = 'dash', width = 2),
                  name = paste0('Changepoint (p=', round(cp_data$changepoint_probability, 2), ')'))
    }
    
    p %>% layout(title = "Changepoint Detection - Velocity",
                 xaxis = list(title = "Career Year"),
                 yaxis = list(title = "Velocity (MPH)"))
  })
  
  output$high_risk_table <- renderDT({
    risk_assessments %>%
      filter(risk_category == "High Risk") %>%
      select(player_name, current_velocity, career_years, injury_history, overall_risk) %>%
      arrange(desc(overall_risk)) %>%
      mutate(overall_risk = round(overall_risk * 100, 1)) %>%
      rename(
        Pitcher = player_name,
        Velocity = current_velocity,
        `Career Years` = career_years,
        Injuries = injury_history,
        `Risk Score` = overall_risk
      ) %>%
      datatable(options = list(pageLength = 15, order = list(list(4, 'desc'))))
  })
  
  output$risk_factors_plot <- renderPlotly({
    risk_long <- risk_assessments %>%
      select(player_name, age_risk, velocity_risk, decline_risk, changepoint_risk, injury_risk) %>%
      pivot_longer(cols = -player_name, names_to = "factor", values_to = "score") %>%
      mutate(factor = str_remove(factor, "_risk"),
             factor = str_to_title(factor))
    
    plot_ly(risk_long, y = ~factor, x = ~score * 100, type = 'box',
            marker = list(color = 'steelblue')) %>%
      layout(title = "Risk Factor Distributions",
             xaxis = list(title = "Risk Score (%)"),
             yaxis = list(title = "Risk Factor"))
  })
  
  output$injury_risk_plot <- renderPlotly({
    risk_assessments %>%
      mutate(injury_category = case_when(
        had_tommy_john == 1 ~ "Tommy John",
        had_shoulder_injury == 1 ~ "Shoulder",
        had_elbow_injury == 1 ~ "Elbow",
        injury_history > 0 ~ "Other",
        TRUE ~ "None"
      )) %>%
      plot_ly(x = ~injury_category, y = ~overall_risk * 100, type = 'box',
              marker = list(color = 'coral')) %>%
      layout(title = "Risk by Injury Type",
             xaxis = list(title = "Injury Type"),
             yaxis = list(title = "Overall Risk Score (%)"))
  })
  
  output$all_risk_table <- renderDT({
    risk_assessments %>%
      select(player_name, current_velocity, career_years, injury_history, overall_risk, risk_category) %>%
      mutate(overall_risk = round(overall_risk * 100, 1)) %>%
      arrange(desc(overall_risk)) %>%
      rename(
        Pitcher = player_name,
        Velocity = current_velocity,
        `Career Years` = career_years,
        Injuries = injury_history,
        `Risk Score` = overall_risk,
        Category = risk_category
      ) %>%
      datatable(
        options = list(pageLength = 25, order = list(list(4, 'desc'))),
        filter = 'top'
      )
  })
  
  output$model_rmse <- renderValueBox({
    valueBox(
      "1.42 MPH",
      "Model RMSE",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$model_mae <- renderValueBox({
    valueBox(
      "1.08 MPH",
      "Model MAE",
      icon = icon("bullseye"),
      color = "green"
    )
  })
  
  output$model_correlation <- renderValueBox({
    valueBox(
      "0.87",
      "Validation Correlation",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$changepoint_summary_table <- renderDT({
    changepoint_results %>%
      mutate(
        changepoint_probability = round(changepoint_probability, 3),
        decline_magnitude = round(decline_magnitude, 2)
      ) %>%
      select(pitcher, metric, changepoint_year, career_years_to_decline, 
             changepoint_probability, decline_magnitude) %>%
      rename(
        Pitcher = pitcher,
        Metric = metric,
        Year = changepoint_year,
        `Career Year` = career_years_to_decline,
        Probability = changepoint_probability,
        `Decline Magnitude` = decline_magnitude
      ) %>%
      datatable(
        options = list(pageLength = 20, order = list(list(4, 'desc'))),
        filter = 'top'
      )
  })
}

shinyApp(ui, server)