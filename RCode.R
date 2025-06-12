# Load required libraries
library(quantmod)
library(forecast)
library(tseries)
library(ggplot2)
library(gridExtra)
library(lubridate)

# Create output directory
output_dir <- "CPI_Forecast_Plots"
dir.create(output_dir, showWarnings = FALSE)

# FRED CPI codes for 5 countries (monthly CPI index)
country_codes <- list(
  USA = "CPIAUCNS",            # United States
  GBR = "GBRCPIALLMINMEI",     # United Kingdom
  BRA = "BRACPIALLMINMEI",     # Brazil
  ZAF = "ZAFCPICORAINMEI",      # South Africa
  IND = "INDCPALTT01IXOBM",    # India
)

# -----------------------------------------------
# Function to process one country's CPI data
# -----------------------------------------------
process_country_cpi <- function(iso, fred_code) {
  cat("\n--- Processing", iso, "---\n")
  
  # Get CPI data from FRED
  getSymbols(fred_code, src = "FRED", auto.assign = TRUE)
  data <- get(fred_code)
  
  # Subset from 1990 onward using xts syntax
  data <- na.omit(data["1990/"])
  
  # Extract start year/month and convert to ts
  start_year <- year(start(data))
  start_month <- month(start(data))
  ts_data <- ts(as.numeric(data), frequency = 12, start = c(start_year, start_month))
  
  # Plot raw CPI data
  p1 <- autoplot(ts_data) +
    ggtitle(paste("Monthly CPI -", iso)) +
    ylab("CPI Index") + xlab("Year") + theme_minimal()
  ggsave(filename = file.path(output_dir, paste0(iso, "_1_raw_cpi.png")), plot = p1)
  
  # Stationarity check using ADF test
  adf_p <- adf.test(ts_data)$p.value
  if (adf_p > 0.05) {
    ts_data <- diff(ts_data)
    stationarity <- "Differenced"
  } else {
    stationarity <- "Stationary"
  }
  
  # Fit ARIMA model
  model <- auto.arima(ts_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  print(summary(model))
  
  # Residual diagnostics
  residuals <- residuals(model)
  
  p2 <- ggAcf(residuals) + ggtitle("ACF of Residuals")
  p3 <- ggplot(data.frame(residuals), aes(x = residuals)) +
    geom_histogram(bins = 30, fill = "steelblue") +
    ggtitle("Residual Histogram") + theme_minimal()
  p4 <- ggplot(data.frame(residuals), aes(sample = residuals)) +
    stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot") + theme_minimal()
  
  ggsave(filename = file.path(output_dir, paste0(iso, "_2_residuals_acf.png")), plot = p2)
  ggsave(filename = file.path(output_dir, paste0(iso, "_3_residuals_hist.png")), plot = p3)
  ggsave(filename = file.path(output_dir, paste0(iso, "_4_residuals_qq.png")), plot = p4)
  
  # Ljung-Box test for autocorrelation
  lb_p <- Box.test(residuals, lag = 24, type = "Ljung-Box")$p.value
  cat("ADF p:", adf_p, "| Ljung-Box p:", lb_p, "\n")
  
  # Forecast next 24 months
  fc_horizon <- 24
  fc <- forecast(model, h = fc_horizon)
  
  p5 <- autoplot(fc) +
    ggtitle(paste("CPI Forecast -", iso)) +
    ylab("Forecasted CPI") + xlab("Year") + theme_minimal()
  ggsave(filename = file.path(output_dir, paste0(iso, "_5_forecast.png")), plot = p5)
  
  # Export forecast to CSV
  last_date <- as.Date(index(data)[NROW(data)])
  fc_df <- data.frame(
    Date = seq(last_date %m+% months(1), by = "month", length.out = fc_horizon),
    Forecast = as.numeric(fc$mean),
    Lo80 = as.numeric(fc$lower[, 1]),
    Hi80 = as.numeric(fc$upper[, 1]),
    Lo95 = as.numeric(fc$lower[, 2]),
    Hi95 = as.numeric(fc$upper[, 2])
  )
  write.csv(fc_df, file.path(output_dir, paste0("forecast_", iso, ".csv")), row.names = FALSE)
  
  return(list(model = model, adf_p = adf_p, lb_p = lb_p, forecast = fc))
}

# -----------------------------------------------
# Run the process for all countries
# -----------------------------------------------
results <- list()
for (iso in names(country_codes)) {
  code <- country_codes[[iso]]
  results[[iso]] <- process_country_cpi(iso, code)
}
