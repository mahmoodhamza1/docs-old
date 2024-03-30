
library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(shinythemes)
library(rugarch)
library(forecast)
library(lubridate)
library(gridExtra)

#reading data
exchange_rates <- read.csv("exchange_rate_dataset.csv",fileEncoding = "ISO-8859-1")

# Convert the Date column to a Date type
exchange_rates$Date <- as.Date(exchange_rates$Date, format="%d-%m-%y")

# Define the Shiny app
start_date <- min(exchange_rates$Date, na.rm = TRUE)
end_date <- max(exchange_rates$Date, na.rm = TRUE)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Exchange Rates Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("currency", "Select Currency:", choices = names(exchange_rates)[-1]), # Exclude Date column
                    dateRangeInput("dateRange", "Select Date Range:", 
                                   start = start_date, end = end_date, 
                                   min = start_date, max = end_date)
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Time Series", 
                               plotOutput("timeSeriesPlot"),
                               wellPanel(
                                 strong("Exchange Rate Summary:"),
                                 verbatimTextOutput("statsSummary")  # Formatted text box for the exchange rate summary
                               )
                      ),
                      tabPanel("Time Series Decomposition", 
                               plotOutput("decompPlot"),
                               wellPanel(
                                 strong("Seasonality Summary:"),
                                 verbatimTextOutput("seasonalitySummary")
                               ),
                               wellPanel(
                                 strong("Trend Summary:"),
                                 verbatimTextOutput("trendSummary")
                               ),
                               wellPanel(
                                 strong("Remainder Summary:"),
                                 verbatimTextOutput("remainderSummary")
                               )
                      ),
                      tabPanel("GARCH Volatility", 
                               plotOutput("garchVolatilityPlot"),
                               wellPanel(
                                 strong("Volatility Summary:"),
                                 verbatimTextOutput("volatilitySummary")  # Formatted text box for the volatility summary
                               )
                      )
                    )
                  )  
                )
)

server <- function(input, output) {
  # Define 'filtered_data' as a reactive expression to return the filtered dataset
  filtered_data <- reactive({
    req(input$currency, input$dateRange[1], input$dateRange[2])  # Ensure inputs are present
    
    exchange_rates %>%
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
      select(Date, Rate = .data[[input$currency]]) %>%
      na.omit()
  })
  
  # Time Series Plot
  output$timeSeriesPlot <- renderPlot({
    data <- filtered_data()  # Access the reactive filtered data
    req(nrow(data) > 0)  # Ensure there is data to plot
    
    ggplot(data, aes(x = Date, y = Rate)) +
      geom_line(color = "darkblue") +
      labs(x = "Date", y = "Exchange Rate", title = paste("Exchange Rate of", input$currency, "Over Time")) +
      theme_minimal()
  })
  
  # Exchange Rate Summary for the Time Series Plot
  output$statsSummary <- renderText({
    data <- filtered_data()  # Access the reactive filtered data
    req(nrow(data) > 0)  # Ensure there is data to plot
    
    paste("Mean: ", mean(data$Rate), "\n", 
          "Median: ", median(data$Rate), "\n", 
          "Standard Deviation: ", sd(data$Rate), sep = "")
  })
  
  # Time Series Decomposition Plot
  output$decompPlot <- renderPlot({
    data <- filtered_data()  # Access the reactive filtered data
    req(nrow(data) > 0)  # Ensure there is data to plot
    
    exchange_rate_ts <- ts(data$Rate, start = c(year(min(data$Date)), 1), frequency = 365.25)
    
    # Perform STL decomposition
    decomposed <- stl(exchange_rate_ts, s.window = "periodic", robust = TRUE)
    
    # Extract components and plot
    trend <- decomposed$time.series[, "trend"]
    seasonal <- decomposed$time.series[, "seasonal"]
    remainder <- decomposed$time.series[, "remainder"]
    time <- as.numeric(time(decomposed$time.series))
    
    trend_df <- data.frame(Time = time, Trend = trend)
    seasonal_df <- data.frame(Time = time, Seasonal = seasonal)
    remainder_df <- data.frame(Time = time, Remainder = remainder)
    
    trend_plot <- ggplot(trend_df, aes(x = Time, y = Trend)) + geom_line(color = "blue")
    seasonal_plot <- ggplot(seasonal_df, aes(x = Time, y = Seasonal)) + geom_line(color = "green")
    remainder_plot <- ggplot(remainder_df, aes(x = Time, y = Remainder)) + geom_area(fill = "darkorange")
    
    grid.arrange(trend_plot, seasonal_plot, remainder_plot, ncol = 1)
  })
  
  # Seasonality Summary for the Decomposition Plot
  # Creating a reactive expression for the decomposition
  decomposed <- reactive({
    req(input$currency, input$dateRange[1], input$dateRange[2])  # Ensure inputs are present
    data <- filtered_data()  # Access the reactive filtered data
    req(nrow(data) > 0)  # Ensure there is data to plot
    
    exchange_rate_ts <- ts(data$Rate, start = c(year(min(data$Date)), 1), frequency = 365.25)
    stl(exchange_rate_ts, s.window = "periodic", robust = TRUE)
  })
  
  # Seasonality Summary
  output$seasonalitySummary <- renderText({
    dec <- decomposed()  # Access the reactive decomposition
    seasonal_component <- dec$time.series[, "seasonal"]
    paste("Max Seasonal Effect: ", max(seasonal_component), "\n", 
          "Min Seasonal Effect: ", min(seasonal_component), sep = "")
  })
  
  # Trend Summary
  output$trendSummary <- renderText({
    dec <- decomposed()  # Access the reactive decomposition
    trend_component <- dec$time.series[, "trend"]
    paste("Long-term Mean: ", mean(trend_component, na.rm = TRUE),
          "Overall Change: ", diff(range(trend_component, na.rm = TRUE)), sep = "\n")
  })
  
  # Remainder Summary
  output$remainderSummary <- renderText({
    dec <- decomposed()  # Access the reactive decomposition
    remainder_component <- dec$time.series[, "remainder"]
    paste("Standard Deviation: ", sd(remainder_component, na.rm = TRUE),
          "Number of Outliers: ", sum(abs(remainder_component) > 2 * sd(remainder_component, na.rm = TRUE)), sep = "\n")
  })
  
  # GARCH Volatility Plot
  output$garchVolatilityPlot <- renderPlot({
    data <- filtered_data()  # Access the reactive filtered data
    req(nrow(data) > 0)  # Ensure there's enough data
    
    spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
    garch_fit <- ugarchfit(spec = spec, data = data$Rate)
    
    volatility <- sigma(garch_fit)
    volatility_data <- data.frame(Date = data$Date, Volatility = volatility)
    
    ggplot(volatility_data, aes(x = Date, y = Volatility)) +
      geom_line(color = "darkred") +
      labs(x = "Date", y = "Volatility", title = paste("Conditional Volatility of", input$currency)) +
      theme_minimal()
  })
  
  # Volatility Summary for the GARCH Volatility Plot
  output$volatilitySummary <- renderText({
    data <- filtered_data()  # Access the reactive filtered data
    req(nrow(data) > 0)  # Ensure there's enough data
    
    spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
    garch_fit <- ugarchfit(spec = spec, data = data$Rate)
    
    volatility <- sigma(garch_fit)
    paste("Average Volatility: ", mean(volatility), "\n", 
          "Max Volatility: ", max(volatility), "\n", 
          "Min Volatility: ", min(volatility), sep = "")
  })
}
#run the app
shinyApp(ui = ui, server = server)