# US Oil Market & GDP Econometric Analysis

An econometric study analyzing the dynamic relationships between US GDP, WTI oil prices, and US oil production using time series analysis techniques including ARMA modeling, VAR estimation, Granger causality tests, and cointegration analysis.

## 📋 Table of Contents

- [Overview](#overview)
- [Data Sources](#data-sources)
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)
- [Methodology](#methodology)
- [Project Structure](#project-structure)
- [Results Summary](#results-summary)
- [References](#references)

## 🎯 Overview

This project investigates the economic relationship between three key US macroeconomic variables:

- **US Real GDP** (quarterly)
- **WTI Oil Prices** (monthly, converted to quarterly)
- **US Oil Production** (monthly, converted to quarterly)

The study period covers **Q1 1995 to Q4 2007**, deliberately excluding the 2008 financial crisis and COVID-19 pandemic to avoid structural breaks.

### Research Questions

1. How do US oil production, oil prices, and GDP mutually influence each other?
2. What form does this relationship take (short-term vs. long-term)?
3. Can we identify Granger causality between these variables?

## 📊 Data Sources

| Variable | Source | Original Frequency | Period |
|----------|--------|-------------------|--------|
| US Real GDP | [FRED - GDPC1](https://fred.stlouisfed.org/series/GDPC1) | Quarterly | 1995-2007 |
| US Oil Production | [FRED - IPG211S](https://fred.stlouisfed.org/series/IPG211S) | Monthly | 1995-2007 |
| WTI Oil Prices | [EIA](https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=F003048623&f=M) | Monthly | 1995-2007 |

All data is seasonally adjusted to remove seasonal effects.

## 📦 Requirements

### R Packages

```r
# Data manipulation
library(readxl)
library(dplyr)
library(lubridate)
library(openxlsx)
library(writexl)
library(tidyr)

# Time series
library(zoo)
library(xts)
library(forecast)

# Unit root & stationarity tests
library(aTSA)
library(urca)
library(tseries)

# VAR modeling
library(vars)

# Diagnostics & output
library(FinTS)
library(texreg)
library(stargazer)
library(performance)
library(ggplot2)

# Data retrieval (optional)
library(fredr)
```

## 🚀 Installation

1. **Clone the repository**
   ```bash
   git clone https://github.com/yourusername/us-oil-gdp-econometrics.git
   cd us-oil-gdp-econometrics
   ```

2. **Install required R packages**
   ```r
   install.packages(c("readxl", "dplyr", "lubridate", "openxlsx", "writexl", 
                      "zoo", "xts", "aTSA", "urca", "forecast", "texreg", 
                      "FinTS", "tseries", "stargazer", "fredr", "performance", 
                      "tidyr", "ggplot2", "vars"))
   ```

3. **Set your working directory**
   
   Open the R script and modify line 26 to point to your data folder:
   ```r
   setwd("/path/to/your/data/folder/")
   ```

4. **Place the required data files** in your working directory:
   - `Prix WTI mensuels.xls`
   - `USA Production pétrolière mensuelle.xlsx`
   - `USA PIB trimestriel.xlsx`

## 💻 Usage

### Running the Analysis

```r
# Source the main script
source("econometrics_analysis.R")
```

### Step-by-Step Execution

The script is organized into logical sections:

1. **Data Preprocessing** (Lines 1-80)
   - Converts monthly data to quarterly frequency
   - Filters data to the study period (1995-2007)
   - Creates processed Excel files

2. **Exploratory Analysis** (Lines 81-180)
   - Plots raw and log-transformed series
   - Generates ACF/PACF plots

3. **Unit Root Testing** (Lines 181-280)
   - Augmented Dickey-Fuller (ADF) tests
   - KPSS confirmation tests
   - ERS tests

4. **ARMA Modeling** (Lines 281-380)
   - Model selection via AIC/BIC
   - Parameter estimation
   - Residual diagnostics

5. **Forecasting** (Lines 381-420)
   - In-sample forecasts
   - Out-of-sample forecasts (h=3)

6. **VAR Analysis** (Lines 421-520)
   - Lag selection
   - Model estimation
   - Diagnostic tests

7. **Causality & IRF** (Lines 521-600)
   - Granger causality tests
   - Impulse response functions

8. **Cointegration** (Lines 601-650)
   - Engle-Granger two-step procedure
   - Phillips-Ouliaris test

## 🔬 Methodology

### 1. Data Transformation

All series are transformed to ensure stationarity:

```
Raw Series → Log Transformation → First Differencing
    Xₜ    →      log(Xₜ)       →    Δlog(Xₜ)
```

### 2. Stationarity Testing

Sequential unit root testing strategy:

```
Model 3 (Trend + Constant) → Model 2 (Constant) → Model 1 (None)
```

Tests applied:
- **ADF** (Augmented Dickey-Fuller)
- **KPSS** (Kwiatkowski-Phillips-Schmidt-Shin)
- **ERS** (Elliott-Rothenberg-Stock)

### 3. Univariate Modeling

ARMA model selection based on:
- Information criteria (AIC, BIC)
- ACF/PACF analysis
- Residual diagnostics

**Selected Model:** ARMA(2,0) for GDP growth

```
xₜ = 0.00474 + 0.1095·xₜ₋₁ + 0.2903·xₜ₋₂ + εₜ
```

### 4. Multivariate Modeling

**VAR(1)** model estimated with three variables:
- GDP growth (Δlog GDP)
- Price change (Δlog WTI)
- Production change (Δlog Production)

### 5. Causality Analysis

Granger causality tests performed:
- 6 bivariate tests
- 3 trivariate tests

### 6. Impulse Response Functions

Orthogonalized IRFs computed using Cholesky decomposition with ordering:
```
Oil Prices → Production → GDP
```

### 7. Cointegration

Engle-Granger two-step procedure:
1. Estimate long-run relationship via OLS
2. Test residuals for unit root

## 📁 Project Structure

```
us-oil-gdp-econometrics/
│
├── README.md                              # This file
├── Project.R                # Main R script
│
├── data/
│   ├── Prix WTI mensuels.xls             # Raw monthly WTI prices
│   ├── Prix WTI trimestriels.xlsx        # Processed quarterly prices
│   ├── USA Production pétrolière mensuelle.xlsx
│   ├── USA Production pétrolière trimestrielle.xlsx
│   ├── USA PIB trimestriel.xlsx
│   └── USA PIB trimestriel def.xlsx
│
├── output/
│   ├── figures/                           # Generated plots
│   └── tables/                            # Regression outputs
│
└── docs/
    └── Econometrics_Project.pdf           # Full report
```

## 📈 Results Summary

### Key Findings

| Analysis | Result |
|----------|--------|
| GDP Stationarity | I(1) with drift → Stationary after first differencing |
| Best Univariate Model | ARMA(2,0) for GDP growth |
| Optimal VAR Lag | p = 1 |
| Granger Causality | GDP → WTI Price (p = 0.031) |
| Cointegration | Production & GDP are cointegrated (Engle-Granger) |

### Diagnostic Tests (VAR Model)

| Test | Result | Interpretation |
|------|--------|----------------|
| Portmanteau | p = 0.51 | No autocorrelation ✓ |
| ARCH | p = 0.43 | Homoscedastic ✓ |
| Jarque-Bera | p < 0.05 | Non-normal residuals ✗ |
| Stability | All roots < 1 | Stable ✓ |

### Economic Interpretation

1. **US GDP Granger-causes WTI prices** - Economic activity influences oil market prices
2. **No direct causality** between production and GDP in the short run
3. **Long-term cointegration** exists between production and GDP, suggesting a stable equilibrium relationship

## 📚 References

- Box, G. E., Jenkins, G. M., Reinsel, G. C., & Ljung, G. M. (2015). *Time Series Analysis: Forecasting and Control*. John Wiley & Sons.
- Engle, R. F., & Granger, C. W. (1987). Co-integration and error correction: Representation, estimation, and testing. *Econometrica*, 55(2), 251-276.
- Hamilton, J. D. (1994). *Time Series Analysis*. Princeton University Press.
- Lütkepohl, H. (2005). *New Introduction to Multiple Time Series Analysis*. Springer.

The detailed project report is available in 

## 👤 Author

**Yanis Calvo**

- GitHub: [@yaniscalvo](https://github.com/yaniscalvo)

