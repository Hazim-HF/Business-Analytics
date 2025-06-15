library(shiny)
library(shinyjs) # Required for enabling and disabling inputs
library(ggplot2)
library(forecast)
library(plotly)

# Load and preprocess the dataset
data <- read.csv("file_02.csv")

# Data cleaning
cols_to_clean <- c("Thermal.Generation.Actual..in.MU.", 
                   "Thermal.Generation.Estimated..in.MU.")

data[cols_to_clean] <- lapply(data[cols_to_clean], function(x) as.numeric(gsub(",", "", x)))
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
####################################################################################
# UI definition
ui <- fluidPage(
  useShinyjs(), # Include shinyjs
  
  # Title and description inside a styled box with added margin-top
  div(style = "background-color: #003f5c; color:white; padding: 20px; border-radius: 10px; margin-top: 30px; margin-bottom: 20px;",
      titlePanel("Time Series Analysis Dashboard for Regional Power Generation in India (2017-2020)"),
      p("This dashboard provides an interactive visualization of energy generation across different regions and types (Hydro, Thermal, Nuclear, or Total). Users can select the type of generation and region, specify the forecast horizon, and generate forecasts using the ARIMA model."),
      
      p("The plot shows both historical and forecasted data, along with confidence intervals for the forecast. The table below the plot displays the forecast details, including the forecasted values and the lower and upper bounds of the confidence intervals. Click the 'Generate Forecast' button to see the forecasted generation for the selected type and region.")
  ),
  
  # Inputs arranged horizontally inside a div with an ID
  div(id = "input-panel", 
      fluidRow(
        column(4, 
               selectInput("type", "Select Type:", 
                           choices = c("Hydro", "Thermal", "Nuclear", "Total"), 
                           selected = "Total")),
        column(4, 
               selectInput("region", "Select Region:", 
                           choices = unique(data$Region), 
                           selected = "North")),
        column(4, 
               sliderInput("forecastHorizonSlider", "Forecast Days:", 
                           min = 1, max = 365, value = 365))
      ),
      
      # Add space below the input panel and button
      fluidRow(
        column(12, 
               div(style = "text-align: center; margin-top: 20px; margin-bottom: 30px;", 
                   actionButton("generateBtn", "Generate Forecast", class = "btn-primary")))
      )
  ),
  
  # Display Total and Daily Power Generation values with visual styling
  fluidRow(
    column(6, 
           div(style = "background-color: #EA6A47; color: white; padding: 15px; border-radius: 10px; text-align: center;", 
               h4(textOutput("totalPowerGeneration")))),
    column(6, 
           div(style = "background-color: #0091D5; color: white; padding: 15px; border-radius: 10px; text-align: center;", 
               h4(textOutput("dailyPowerGeneration"))))
  ),

  # Add space between boxes and plot
  fluidRow(
    column(12, 
           div(style = "margin-bottom: 30px;"))
  ),

  # Plot and Summary Table inside a combined box with an outline
  div(style = "background-color: #f7f7f7; padding: 20px; border-radius: 10px; border: 2px solid #ccc;",
      fluidRow(
        column(12, 
               plotlyOutput("timeSeriesForecastPlot"),
               textOutput("plotStatusText"))
      ),
      
      fluidRow(
        column(12, 
               div(style = "display: flex; justify-content: center; margin-top: 20px;",
                   tableOutput("forecastTable")))
      )
  )
)
####################################################################################
# Server logic
server <- function(input, output, session) {
  
  # Reactive filtered data
  filteredData <- reactive({
    req(input$type, input$region)
    
    type_col <- switch(input$type,
                        "Hydro" = "Hydro.Generation.Actual..in.MU.",
                        "Thermal" = "Thermal.Generation.Actual..in.MU.",
                        "Nuclear" = "Nuclear.Generation.Actual..in.MU.",
                        "Total" = NULL)
    
    if (!is.null(type_col)) {
      data <- data[!is.na(data[[type_col]]), ]
      data <- data[data$Region == input$region, c("Date", type_col)]
      colnames(data) <- c("Date", "Value")
    } else {
      data <- data[data$Region == input$region, ]
      data$Value <- rowSums(data[, c("Hydro.Generation.Actual..in.MU.",
                                     "Thermal.Generation.Actual..in.MU.",
                                     "Nuclear.Generation.Actual..in.MU.")], 
                             na.rm = TRUE)
      data <- data[, c("Date", "Value")]
    }
    data
  })
  
  # Precompute the ARIMA forecast
  precomputed_forecasts <- reactiveVal(NULL)
  
  observeEvent(input$generateBtn, {
    # Disable all inputs while generating the forecast
    disable("input-panel")
    output$plotStatusText <- renderText("Generating forecast, please wait...")
    
    plot_data <- filteredData()
    
    # Ensure there is data to work with
    if (nrow(plot_data) == 0) {
      output$plotStatusText <- renderText("No data available for the selected region and type.")
      enable("input-panel") # Re-enable all inputs
      return(NULL)
    }
    
    ts_data <- ts(plot_data$Value, 
                  start = c(as.numeric(format(min(plot_data$Date), "%Y")), 
                            as.numeric(format(min(plot_data$Date), "%j"))), 
                  frequency = 365)
    
    fit <- tryCatch({
      auto.arima(ts_data)
    }, error = function(e) {
      output$plotStatusText <- renderText("Error in ARIMA model fitting. Please check the data.")
      enable("input-panel") # Re-enable all inputs
      return(NULL)
    })
    
    if (is.null(fit)) {
      output$plotStatusText <- renderText("ARIMA model fitting failed.")
      enable("input-panel") # Re-enable all inputs
      return(NULL)
    }
    
    forecasted <- forecast(fit, h = input$forecastHorizonSlider)
    forecast_dates <- seq(max(plot_data$Date) + 1, by = "days", length.out = input$forecastHorizonSlider)
    forecast_df <- data.frame(Date = forecast_dates, 
                              Value = as.numeric(forecasted$mean), 
                              Lower = forecasted$lower[, 2], 
                              Upper = forecasted$upper[, 2])
    
    precomputed_forecasts(list(forecast_df = forecast_df, ts_data = ts_data))
    output$plotStatusText <- renderText("The plot has been successfully generated!")
    
    # Re-enable all inputs after completion
    enable("input-panel")
  })
  
  # Total Power Generation (Sum of historical values)
  output$totalPowerGeneration <- renderText({
    plot_data <- filteredData()
    total_power <- sum(plot_data$Value, na.rm = TRUE)
    paste("Total Power Generation: ", round(total_power, 2), " MU")
  })
  
  # Daily Power Generation (Average of historical values)
  output$dailyPowerGeneration <- renderText({
    plot_data <- filteredData()
    avg_daily_power <- mean(plot_data$Value, na.rm = TRUE)
    paste("Daily Power Generation (Average): ", round(avg_daily_power, 2), " MU")
  })
  
  output$timeSeriesForecastPlot <- renderPlotly({
    forecast_data <- precomputed_forecasts()
    if (is.null(forecast_data)) {
      return(NULL)
    }
    
    plot_data <- filteredData()
    forecast_df <- forecast_data$forecast_df
    
    # Combine actual and forecast data
    plot <- ggplot() +
      geom_line(data = plot_data, aes(x = Date, y = Value), color = "blue") +
      geom_line(data = forecast_df, aes(x = Date, y = Value), color = "red") +
      geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower, ymax = Upper), fill = "red", alpha = 0.2) +
      labs(title = paste("Time Series and ARIMA Forecast for", input$type, "Generation in", input$region),
           x = "Date", y = "Generation (in MU)") +
      theme_minimal()
    
    ggplotly(plot)
  })
  
  output$forecastTable <- renderTable({
    forecast_data <- precomputed_forecasts()
    if (is.null(forecast_data)) {
      return(NULL)
    }
    
    forecast_df <- forecast_data$forecast_df
    forecast_df$Date <- format(forecast_df$Date, "%d-%b-%Y")
    head_tail_data <- rbind(head(forecast_df, 5), tail(forecast_df, 5))
    data.frame(
      Date = head_tail_data$Date,
      Forecasted_Value = head_tail_data$Value,
      Lower_Confidence = head_tail_data$Lower,
      Upper_Confidence = head_tail_data$Upper
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
