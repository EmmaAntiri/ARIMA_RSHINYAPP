library(shiny)
library(quantmod)
library(forecast)
library(ggplot2)
library(tseries)
library(lubridate)
library(DT)

# FRED CPI Codes (monthly index)
country_codes <- list(
  "United States" = "CPIAUCNS",
  "United Kingdom" = "GBRCPIALLMINMEI",
  "Brazil" = "BRACPIALLMINMEI",
  "India" = "INDCPALTT01IXOBM",
  "South Africa" = "ZAFCPICORAINMEI"
)

ui <- fluidPage(
  titlePanel("CPI Forecast Dashboard (ARIMA, Monthly)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = names(country_codes)),
      sliderInput("horizon", "Forecast Horizon (Months):", min = 6, max = 36, value = 24)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Time Series", plotOutput("ts_plot")),
        tabPanel("Residual Diagnostics",
                 fluidRow(
                   column(4, plotOutput("acf_plot")),
                   column(4, plotOutput("qq_plot")),
                   column(4, plotOutput("hist_plot"))
                 ),
                 verbatimTextOutput("test_stats")
        ),
        tabPanel("Forecast",
                 plotOutput("forecast_plot"),
                 DTOutput("forecast_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  get_cpi_data <- reactive({
    symbol <- country_codes[[input$country]]
    
    # Get xts from FRED
    raw_xts <- getSymbols(symbol, src = "FRED", auto.assign = FALSE)
    cleaned_xts <- na.omit(raw_xts)
    
    # Get numeric values and time components
    values <- as.numeric(cleaned_xts)
    first_date <- index(cleaned_xts)[1]
    start_year <- year(first_date)
    start_month <- month(first_date)
    
    # Convert to time series
    ts_data <- ts(values, frequency = 12, start = c(start_year, start_month))
    
    # Trim to 1990+
    if (start_year < 1990) {
      ts_data <- window(ts_data, start = c(1990, 1))
    }
    
    return(ts_data)
  })
  
  fit_model <- reactive({
    ts_data <- get_cpi_data()
    
    adf_p <- tryCatch(adf.test(ts_data)$p.value, error = function(e) NA)
    
    # Difference if non-stationary
    if (!is.na(adf_p) && adf_p > 0.05) {
      ts_data <- diff(ts_data)
    }
    
    model <- auto.arima(ts_data, seasonal = TRUE)
    forecasted <- forecast(model, h = input$horizon)
    
    lb_p <- tryCatch(Box.test(residuals(model), lag = 24, type = "Ljung-Box")$p.value, error = function(e) NA)
    
    list(model = model, forecast = forecasted, ts_data = ts_data,
         adf_p = adf_p, lb_p = lb_p)
  })
  
  output$ts_plot <- renderPlot({
    ts_data <- get_cpi_data()
    autoplot(ts_data) +
      ggtitle(paste("Monthly CPI -", input$country)) +
      ylab("CPI Index") + xlab("Year") + theme_minimal()
  })
  
  output$acf_plot <- renderPlot({
    model <- fit_model()$model
    ggAcf(residuals(model)) + ggtitle("ACF of Residuals")
  })
  
  output$qq_plot <- renderPlot({
    residuals <- residuals(fit_model()$model)
    ggplot(data.frame(residuals), aes(sample = residuals)) +
      stat_qq() + stat_qq_line() + theme_minimal() + ggtitle("Q-Q Plot")
  })
  
  output$hist_plot <- renderPlot({
    residuals <- residuals(fit_model()$model)
    ggplot(data.frame(residuals), aes(x = residuals)) +
      geom_histogram(bins = 30, fill = "steelblue") +
      ggtitle("Residual Histogram") + theme_minimal()
  })
  
  output$test_stats <- renderPrint({
    stats <- fit_model()
    cat("ADF Test p-value:", round(stats$adf_p, 4), "\n")
    cat("Ljung-Box p-value:", round(stats$lb_p, 4), "\n")
    cat("ARIMA Model:\n")
    print(stats$model)
  })
  
  output$forecast_plot <- renderPlot({
    fc <- fit_model()$forecast
    autoplot(fc) +
      ggtitle(paste("CPI Forecast -", input$country)) +
      ylab("Forecasted CPI") + xlab("Year") + theme_minimal()
  })
  
  output$forecast_table <- renderDT({
    fc <- fit_model()$forecast
    start_date <- Sys.Date() %m+% months(1)
    dates <- seq(start_date, by = "month", length.out = input$horizon)
    df <- data.frame(
      Date = dates,
      Forecast = round(fc$mean, 2),
      Lo80 = round(fc$lower[, 1], 2),
      Hi80 = round(fc$upper[, 1], 2),
      Lo95 = round(fc$lower[, 2], 2),
      Hi95 = round(fc$upper[, 2], 2)
    )
    datatable(df, options = list(pageLength = 10))
  })
}

shinyApp(ui, server)
