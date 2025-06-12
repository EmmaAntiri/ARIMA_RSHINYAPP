# Global CPI Forecasting Dashboard

## 📈 Forecasting Monthly Consumer Price Index (CPI) Trends for Multiple Countries Using ARIMA/ARIMAX Models

This project provides a complete R-based solution for analyzing and forecasting **monthly CPI (Consumer Price Index)** data across five countries from 1990 to present. It uses ARIMA and ARIMAX models to generate forecasts and includes a **Shiny dashboard** for interactive exploration and visualization.

---

## 🚀 Features

- ✅ Automatic CPI data retrieval from **FRED** (via `quantmod`)
- 📉 Time series preprocessing and **stationarity testing** (ADF test)
- ♻️ Automatic model selection using `auto.arima()`
- 🔍 Residual diagnostics:
  - ACF plots
  - Histograms
  - Q-Q plots
  - Ljung-Box test
- 📊 Forecasts for 24 months with exportable results:
  - **PNG** plots
  - **CSV** forecast tables
- ⚙️ **ARIMAX** support for incorporating exogenous variables (e.g., GDP, interest rate)
- 💻 **Interactive Shiny dashboard**
- ☁️ Deployable to **shinyapps.io**

---

## 📊 Countries Covered

- 🇺🇸 United States (USA)
- 🇬🇧 United Kingdom (UK)
- 🇧🇷 Brazil (BRA)
- 🇮🇳 India (IND)
- 🇿🇦 South Africa (ZAF)

---

## 📂 Directory Structure

```
/CPI_Forecast_Plots/      <- Auto-saved PNG plots and CSV forecasts
app.R                     <- Full Shiny dashboard script
forecast_utils.R          <- Forecasting functions (ARIMA/ARIMAX)
README.md                 <- Project documentation
```

---

## 📆 Getting Started

### Prerequisites

Make sure you have R and the following packages installed:

```r
install.packages(c("quantmod", "forecast", "tseries", "ggplot2", "shiny", "lubridate"))
```

### Running the App

```r
# From R console or RStudio
shiny::runApp("app.R")
```

---

## 🚤 Deployment

To deploy to [shinyapps.io](https://www.shinyapps.io/):

1. Create an account and install `rsconnect`:
```r
install.packages("rsconnect")
```

2. Set your account info:
```r
rsconnect::setAccountInfo(name='your_name', token='your_token', secret='your_secret')
```

3. Deploy the app:
```r
rsconnect::deployApp('path/to/your/app')
```

---

## 📚 License

MIT License © [Your Name or Organization Here]

---

## ✏️ Contributions

Pull requests, bug reports, and feature suggestions are welcome.

---

## 📅 TODO

- [ ] Improve UI responsiveness on mobile
- [ ] Add more countries
- [ ] Include confidence interval toggles
- [ ] Allow user-uploaded external regressors

---

_This repository is maintained for reproducible forecasting and visualization of CPI using open-source economic data._


