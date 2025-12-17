# Loading required packages
library(readxl) # Package for reading data files
library(dplyr)
library(lubridate)
library(openxlsx)
library(writexl)
library(zoo)
library(xts) # Package for handling time series
library(aTSA)
library(urca)
library(forecast)
library(texreg) # For nice tables with p-values
library(FinTS) # For ARCH tests
library(tseries) # For Jarque-Bera
library(stargazer)
library(fredr)
library(performance)
library(tidyr) # For restructuring datasets
library(ggplot2) # For plotting
library(vars) # For VAR estimation + IRFs

setwd("/Users/yanis.calvo/Desktop/Dauphine/BFA2/semestre 1/Econométrie des series temporelles/projet /Data/")
# Enter the path to the folder where all upcoming actions will be performed inside setwd()

# Conversion of monthly price data to quarterly data

# Step 1: Creating an xlsx file
new_prices <- read_excel("Prix WTI mensuels.xls", sheet = 2, skip = 2) # Reads monthly price data content
new_prices$Date <- as.Date(new_prices$Date)
write_xlsx(new_prices, "Prix WTI trimestriels.xlsx")

# Step 2: Converting data in the new file
new_prices <- read_excel("Prix WTI trimestriels.xlsx", sheet = 1) %>%
  mutate(Date = as.Date(Date))

new_prices_trimestriel <- new_prices %>%
  mutate(Annee = year(Date), Trimestre_num = quarter(Date)) %>%
  filter(Annee >= 1995) %>%
  filter(Annee <= 2007) %>%
  group_by(Annee, Trimestre_num) %>%
  summarise(
    `Prix trimestriel` = mean(`West Texas Intermediate First Purchase Price (Dollars per Barrel)`, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Trimestre = paste0("Q", Trimestre_num, " ", Annee)) %>%
  arrange(Annee, Trimestre_num) %>%
  dplyr::select(Trimestre, `Prix trimestriel`)

trim <- loadWorkbook("Prix WTI trimestriels.xlsx")
if ("Prix trimestriels" %in% names(trim)) removeWorksheet(trim, "Prix trimestriels")
addWorksheet(trim, "Prix trimestriels")
writeData(trim, "Prix trimestriels", new_prices_trimestriel)
saveWorkbook(trim, "Prix WTI trimestriels.xlsx", overwrite = TRUE)
# The saveWorkbook function creates the new file under the specified name and path

# Conversion of monthly production data to quarterly data

new_productions <- read_excel("USA Production pétrolière mensuelle.xlsx", sheet = 2) %>%
  mutate(observation_date = as.Date(observation_date))

new_productions_trimestriel <- new_productions %>% 
  mutate(
    Annee = year(observation_date), Trimestre_num = quarter(observation_date)) %>%
  filter(Annee >= 1995) %>%
  filter(Annee <= 2007) %>%
  group_by(Annee, Trimestre_num) %>%
  summarise(`Production trimestrielle` = mean(`IPG211S`, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Trimestre = paste0("Q", Trimestre_num, " ", Annee)) %>%
  arrange(Annee, Trimestre_num) %>%
  dplyr::select(Trimestre, `Production trimestrielle`)

trim <- loadWorkbook("USA Production pétrolière mensuelle.xlsx")
if ("Production trimestrielle" %in% names(trim)) {removeWorksheet(trim, "Production trimestrielle")}
addWorksheet(trim, "Production trimestrielle")
writeData(trim, sheet = "Production trimestrielle", new_productions_trimestriel)
saveWorkbook(trim,"USA Production pétrolière trimestrielle.xlsx", overwrite = TRUE)
# The saveWorkbook function creates the new file under the specified name and path

# Transformation of GDP data to keep only Q1 1995 to Q4 2007

new_PIB <- read_excel("USA PIB trimestriel.xlsx", sheet = 2)
new_PIB_filtre <- new_PIB %>%
  filter(observation_date >= as.Date("1995-01-01") &
           observation_date <= as.Date("2007-12-31"))

trim <- loadWorkbook ("USA PIB trimestriel.xlsx")
if ("PIB trimestriel" %in% names(trim)) {removeWorksheet(trim, "PIB trimestriel")}
addWorksheet(trim, "PIB trimestriel")
writeData(trim, sheet = "PIB trimestriel", new_PIB_filtre)
saveWorkbook(trim,"USA PIB trimestriel def.xlsx", overwrite = TRUE)

# Visual representations, ACF and PACF

# Quarterly WTI Prices

# Analysis of the series
prix_trimestriels <- read_xlsx("Prix WTI trimestriels.xlsx", sheet = 2)
dates <- as.Date(as.yearqtr(prix_trimestriels$Trimestre, format = "Q%q %Y"), frac = 1)
prix_trim <- xts(prix_trimestriels$'Prix trimestriel', order.by = dates)
plot(prix_trim, main="Quarterly WTI Prices") 
acf(prix_trim, main="Quarterly WTI Prices") 
pacf(prix_trim, main="Quarterly WTI Prices") 

# Analysis of the series in log
log_prix_trimestriels <- xts(log(prix_trimestriels$'Prix trimestriel'), order.by = dates)
plot(log_prix_trimestriels, main="Quarterly WTI Prices (log)")
acf(log_prix_trimestriels, main="Quarterly WTI Prices (log)") 
# The ACF decreases slowly towards 0
pacf(log_prix_trimestriels, main="Quarterly WTI Prices (log)")
# Only the first term of the PACF is significantly different from 0
# -> Suggests an AR(1) process

dlog_prix <- diff(log_prix_trimestriels)
dlog_prix_trim <- dlog_prix[-1,]
plot(dlog_prix_trim, main = "Quarterly WTI Prices (dlog)") # Visually appears stationary
acf(dlog_prix_trim, main = "Quarterly WTI Prices (dlog)") 
pacf(dlog_prix_trim, main = "Quarterly WTI Prices (dlog)")

# USA Quarterly Oil Production

# Analysis of the series
productions_trim <- read.xlsx("USA Production pétrolière trimestrielle.xlsx", sheet = 3)
dates <- as.Date(as.yearqtr(productions_trim$Trimestre, format = "Q%q %Y"), frac = 1)
prod_vals <- as.numeric(productions_trim$Production.trimestrielle)
productions_trimestrielles <- xts(prod_vals, order.by = dates)
plot(productions_trimestrielles, main="USA Quarterly Oil Production")
acf(productions_trimestrielles, main="USA Quarterly Oil Production")
pacf(productions_trimestrielles, main="USA Quarterly Oil Production")

# Analysis of the series in log
log_production_trimestrielle <- xts(log(prod_vals), order.by = dates)
plot(log_production_trimestrielle, main="Quarterly Oil Production (log)")
acf(log_production_trimestrielle, main="Quarterly Oil Production (log)")
# The ACF decreases slowly towards 0
pacf(log_production_trimestrielle, main="Quarterly Oil Production (log)")
# Only the first term of the PACF is significantly different from 0
# -> Suggests an AR(1) process

dlog_prod <- diff(log_production_trimestrielle)
dlog_prod_trim <- dlog_prod[-1,]
plot(dlog_prod_trim, main = "Quarterly Oil Production (dlog)") # Visually appears stationary
acf(dlog_prod_trim, main = "Quarterly Oil Production (dlog)") 
pacf(dlog_prod_trim, main = "Quarterly Oil Production (dlog)")

# Quarterly GDP

# Analysis of the series
PIB_trimestriel <- read.xlsx("USA PIB trimestriel def.xlsx", sheet = 3)
PIB_trimestriel$observation_date <- as.numeric(PIB_trimestriel$observation_date)
PIB_trimestriel$observation_date <- as.Date(PIB_trimestriel$observation_date, origin = "1899-12-30")
dates <- as.Date(as.yearqtr(PIB_trimestriel$observation_date, format = "%Y-Q%q"), frac = 1)
PIB_ts <- xts(PIB_trimestriel$GDPC1, order.by = dates)
plot(PIB_ts, main="USA Quarterly GDP")
acf(PIB_ts, main="USA Quarterly GDP")
pacf(PIB_ts, main="USA Quarterly GDP")

# Analysis of the series in log
log_PIB_trimestriel <- xts(log(PIB_trimestriel$GDPC1), order.by = dates)
plot(log_PIB_trimestriel, main = "USA Quarterly GDP (log)")
acf(log_PIB_trimestriel, main = "USA Quarterly GDP (log)")
# The ACF decreases slowly towards 0
pacf(log_PIB_trimestriel, main = "USA Quarterly GDP (log)")
# Only the first term of the PACF is significantly different from 0
# -> Suggests an AR(1) process

# Unit Root Testing Sequential Strategy: Dickey-Fuller Test

# Testing Model 3 (Trend + Constant)
df3 <- ur.df(log_PIB_trimestriel, type = "trend", selectlags = "BIC") 
summary(df3)
# ADF test stat = -1.5193 > Critical Value (5%) = -3.45 -> Fail to reject H0: non-stationary
# Trend test stat = abs(3.0666) < CV (5%) = 3.14 (per DF tables for trend specification) 
# -> Fail to reject H0: no deterministic trend

df3@lags # 1 lag used

# Testing Model 2 (Constant/Drift)
df2 <- ur.df(log_PIB_trimestriel, type = "drift", selectlags = "BIC")
summary(df2)
# ADF test stat = -2.1325 > CV (5%) = -2.89 -> Fail to reject H0: non-stationary
# Constant test stat = abs(16.0881) > CV (5%) = 2.86 (per DF tables for constant specification)
# -> Reject H0: constant is significant
# Conclusion: The series is non-stationary with drift: Random Walk with Drift X_t ~ I(1) + c

# Confirming Dickey-Fuller result with KPSS test 

# Testing chosen specification only
KPSS2 <- ur.kpss(log_PIB_trimestriel, type="mu", lags="short")
summary(KPSS2) 
# Test stat = 1.3703 > CV (5%) = 0.463 -> Reject H0: non-stationary (confirms ADF result)

# Confirming Dickey-Fuller result with ERS test

# Testing chosen specification only
ERS2 <- ur.ers(log_PIB_trimestriel, type = c("DF-GLS"), model = "constant", lag.max = 3) 
summary(ERS2)
# Test stat = 0.0689 > CV (5%): -1.95 -> Fail to reject H0: non-stationary (confirms ADF result)

# Random Walk with Drift
# -> Stationarize via first differencing 

dlog_PIB <- diff(log_PIB_trimestriel)
dlog_PIB_trim <- dlog_PIB[-1,]
plot(dlog_PIB_trim, main = "USA Quarterly GDP (dlog)") # Visually appears stationary
acf(dlog_PIB_trim, main = "USA Quarterly GDP (dlog)") # Suggests MA(0) or MA(2)
pacf(dlog_PIB_trim, main = "USA Quarterly GDP (dlog)") # Suggests AR(0) or AR(2)

# Verify that the differenced series is stationary 

# Unit Root Testing Sequential Strategy: Dickey-Fuller Test

# Testing Model 3
df3 <- ur.df(dlog_PIB_trim, type = "trend", selectlags = "BIC") 
summary(df3)
# ADF test stat = -3.5875 < CV (5%) = -3.45 -> Reject H0: stationary
# Trend test: p = 0.13 > 0.05 -> Fail to reject H0: no deterministic trend

df3@lags # 1 lag used

# Testing Model 2
df2 <- ur.df(dlog_PIB_trim, type = "drift", selectlags = "BIC")
summary(df2)
# ADF test stat = -3.2032 < CV (5%) = -2.89 -> Reject H0: stationary
# Constant test: p = 0.00582 < 0.05 -> Reject H0: deterministic constant

# Conclusion: The series is stationary I(0): Y_t = ∆X_t ~ I(0) + c 

# Confirming Dickey-Fuller result with KPSS test
KPSS2 <- ur.kpss(dlog_PIB_trim, type="mu", lags="short", use.lag = 1)
summary(KPSS2)
# Test stat = 0.4068 < CV (5%) = 0.463 -> Fail to reject H0: stationary with deterministic constant

# Confirming Dickey-Fuller result with Phillips-Perron/ERS test 
ERS2 <- ur.ers(dlog_PIB_trim, type = c("DF-GLS"), model = "constant", lag.max = 3) 
summary(ERS2)
# Test stat = -1.7086 > CV (5%) = -1.95 -> Fail to reject H0: non-stationary
# But test stat = -1.7086 < CV (10%) = -1.62 -> Reject H0: stationary 

# Search for the optimal ARMA model

# Identification via autocorrelation, testing multiple ARMA models
dlog_PIB_trim_new <- head(dlog_PIB_trim, -3)

mod1 <- Arima(dlog_PIB_trim_new, order = c(1, 0, 0))
mod2 <- Arima(dlog_PIB_trim_new, order = c(2, 0, 0))
mod3 <- Arima(dlog_PIB_trim_new, order = c(3, 0, 0)) 
mod4 <- Arima(dlog_PIB_trim_new, order = c(0, 0, 1)) 
mod5 <- Arima(dlog_PIB_trim_new, order = c(0, 0, 2)) 
mod6 <- Arima(dlog_PIB_trim_new, order = c(0, 0, 3)) 
mod7 <- Arima(dlog_PIB_trim_new, order = c(1, 0, 1)) 
mod8 <- Arima(dlog_PIB_trim_new, order = c(2, 0, 1)) 
mod9 <- Arima(dlog_PIB_trim_new, order = c(3, 0, 1)) 
mod10 <- Arima(dlog_PIB_trim_new, order = c(1, 0, 2)) 
mod11 <- Arima(dlog_PIB_trim_new, order = c(1, 0, 3)) 
mod12 <- Arima(dlog_PIB_trim_new, order = c(2, 0, 2)) 
mod13 <- Arima(dlog_PIB_trim_new, order = c(3, 0, 2))  
mod14 <- Arima(dlog_PIB_trim_new, order = c(2, 0, 3)) 
mod15 <- Arima(dlog_PIB_trim_new, order = c(3, 0, 3)) 
mod16 <- Arima(dlog_PIB_trim_new, order = c(0, 0, 0))

AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod14, mod15, mod16)
BIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod14, mod15, mod16)

# Best model according to AIC is ARMA(2,0)
# Best model according to BIC is ARMA(0,0)

# ARMA(2,0) Criteria
# AIC: -370.4820
# BIC: -362.9972

# ARMA(0,0) Criteria
# AIC: -369.0790
# BIC: -365.3366

# Minimization is generally stronger for ARMA(0,0) than ARMA(2,0)
# Models are very close, with a slight preference for ARMA(0,0).
# We choose ARMA(2,0) here.

# Parameter Tests
ARMA_modele <- arima(dlog_PIB_trim_new, order = c(2,0,0))

summary(ARMA_modele)
screenreg(ARMA_modele)
# AR1 coefficient not significant
# AR2 coefficient barely significant
# Constant c: highly significant 
# Should an AR(2) be prioritized?

# Residual Tests
residus <- residuals(ARMA_modele)
plot(residus)

# Residual Autocorrelation (Ljung-Box Test)
acf(residus)
pacf(residus)
Box.test(residus, lag = 10, type = "Ljung-Box", fitdf = 2)
# p-value = 0.3232 > 5% -> Fail to reject H0 -> No residual autocorrelation OK 

# Residual Homoscedasticity 
ArchTest(residus, lag = 10)
# p-value = 0.4535 > 5% -> Fail to reject H0 -> No ARCH effects -> No heteroscedasticity OK

# Residual Normality (Jarque-Bera Test)
jarque.bera.test(residus)
# p-value = 0.5997 > 5% -> Fail to reject H0 -> Normally distributed residuals OK

checkresiduals(ARMA_modele)
# Model is usable for forecasting

# Forecasting of the stationarized series 

# Static In-Sample Forecasts
dates <- as.Date(as.yearqtr(PIB_trimestriel$observation_date, format = "%Y-Q%q"), frac = 1)
dates <- dates[-1]
dates <- head(dates, -3)

InS_prev_stat <- xts(fitted(ARMA_modele), order.by = dates)
plot(InS_prev_stat, type='l', col="black", ylab="Value", xlab="Time", main="Static In-Sample Forecasts")
lines(dlog_PIB_trim, col="red")
legend("bottomright", legend=c("predictions", "observed"), col=c("black", "red"), lty=1, xpd=TRUE)

# Forecast Evaluation
summary(ARMA_modele)
# MAE: close to 0 OK
# RMSE: close to 0 OK 
# MAPE: close to 80% but errors are exaggerated due to log variations of GDP being close to 0

# Out-of-Sample Forecasts
OutofS_prev <- forecast::forecast(ARMA_modele, h = 3, level = 90)
OutofS_prev$mean
OutofS_prev$lower
OutofS_prev$upper
plot(OutofS_prev, main="Out-of-Sample Forecasts", fcol = "red")
ts_points <- time(dlog_PIB_trim)[49:51]
points(49:51, dlog_PIB_trim[49:51], type = "o", col = "black", pch = 16, lwd = 2)
lines(OutofS_prev$mean, col="red", lwd=2)

# Red: forecasts
# Black: actual data

# VAR Selection
data <- cbind(dlog_PIB_trim, dlog_prix_trim, dlog_prod_trim)
head(data)
VARselect(data, lag.max = 8, type = "both")
# We select lag = 1 according to AIC. Modeling with trend + constant (both) because prices and production are I(0)+c+bt

var_model <- VAR(data, p = 1, type = "both")
summary(var_model)

# Autocorrelation Test
serial.test(var_model, lags.pt = 4, type = "PT.asymptotic") 
# p-value = 0.5121 > 0.05: Accept H0, no autocorrelation

# Heteroscedasticity Test
arch.test(var_model, lags.multi = 4, multivariate.only = TRUE)
# p-value = 0.4332 > 0.05: Accept H0, residuals are homoscedastic

# JB Normality Test
normality.test(var_model) 
# p-value < 0.05: Reject H0, residuals do not follow a normal distribution

# Stability Test
roots(var_model) # All inverse roots are less than 1

# Causality between variables (Granger)

# Bivariate

# Between GDP and Price
var_bi <- VAR(data[, c("dlog_PIB_trim", "dlog_prix_trim")], p = 1, type = "both")
causality(var_bi, cause = "dlog_prix_trim") 
# p-value = 0.6439 > 0.05: Accept H0, price does not cause GDP
causality(var_bi, cause = "dlog_PIB_trim") 
# p-value = 0.03151 > 0.05: Reject H0, GDP causes price
# Only price is caused by GDP

# Between GDP and Production
var_bi <- VAR(data[, c("dlog_PIB_trim", "dlog_prod_trim")], p = 1, type = "both")
causality(var_bi, cause = "dlog_prod_trim")
# p-value = 0.1518 > 0.05: Accept H0, production does not cause GDP
causality(var_bi, cause = "dlog_PIB_trim") 
# p-value = 0.9046 > 0.05: Accept H0, GDP does not cause production
# No causality between GDP and production

# Between Price and Production
var_bi <- VAR(data[, c("dlog_prix_trim", "dlog_prod_trim")], p = 1, type = "both")
causality(var_bi, cause = "dlog_prod_trim")
# p-value = 0.9774 > 0.05: Accept H0, production does not cause price
causality(var_bi, cause = "dlog_prix_trim")
# p-value = 0.9506 > 0.05: Accept H0, price does not cause production
# No causality between production and price

# Trivariate
causality(var_model, cause = "dlog_PIB_trim")
# p-value = 0.09894 > 0.05: GDP does not cause price and production
causality(var_model, cause = "dlog_prix_trim")
# p-value = 0.9049 > 0.05: Price does not cause GDP and production
causality(var_model, cause = "dlog_prod_trim")
# p-value = 0.3616 > 0.05: Production does not cause GDP and price
# No causality in trivariate model

# Impulse Response Analysis (IRF)

# Choice of ordering for Cholesky decomposition
# We place oil prices first (affects GDP), then production, and finally GDP
var_ordre_vars <- VAR(data[, c("dlog_prix_trim", "dlog_prod_trim", "dlog_PIB_trim")], p = 1, type = "const")

# Plotting IRFs

irf_result<-irf(var_ordre_vars, impulse = "dlog_PIB_trim",
                response = "dlog_prix_trim",
                n.ahead = 8, boot = TRUE)
plot(irf_result)

irf_result<-irf(var_ordre_vars, impulse = "dlog_prix_trim",
                response = "dlog_PIB_trim",
                n.ahead = 8, boot = TRUE)
plot(irf_result)

irf_result<-irf(var_ordre_vars, impulse = "dlog_prix_trim",
                response = "dlog_prod_trim",
                n.ahead = 8, boot = TRUE)
plot(irf_result)

irf_result<-irf(var_ordre_vars, impulse = "dlog_prod_trim",
                response = "dlog_prix_trim",
                n.ahead = 8, boot = TRUE)
plot(irf_result)

irf_result<-irf(var_ordre_vars, impulse = "dlog_PIB_trim",
                response = "dlog_prod_trim",
                n.ahead = 8, boot = TRUE)
plot(irf_result)

irf_result<-irf(var_ordre_vars, impulse = "dlog_prod_trim",
                response = "dlog_PIB_trim",
                n.ahead = 8, boot = TRUE)
plot(irf_result)

# Engle-Granger Test

# 1: Long-term relationship between residuals
reg_eg <- lm(log_production_trimestrielle ~ log_PIB_trimestriel)
print(summary(reg_eg))

resid_EG <- reg_eg$residuals

data <- cbind(log_production_trimestrielle, log_PIB_trimestriel)
data <- as.data.frame(data)              
data$date <- as.Date(rownames(data))    
rownames(data) <- NULL                  
head(data)

ggplot(data, aes(x = date, y = resid_EG)) +
  geom_line(color = "darkblue") +
  labs(title = "Residuals of long-term relationship (log quarterly production ~ log quarterly GDP)",
       y = "Residual", x = "Date") +
  theme_minimal()

# 2: Unit root test on residuals
summary(ur.df(resid_EG, type = "none", selectlags = "AIC"))
# t-stat = -3.1622 > -(Engle and Yoo tables): Reject H0, production and GDP are cointegrated

# Verify result with Phillips-Ouliaris test
print(po.test(cbind(data$log_production_trimestrielle, data$log_PIB_trimestriel)))
# p-value > 0.05: Fail to reject H0, no cointegration (contradicts Phillips-Ouliaris)
# Likely cointegration exists, but is weak

#----------------Section not to be integrated------------------
# Proposal for an Error Correction Model (ECM)

data <- cbind(dlog_prod_trim, dlog_PIB_trim)
lag_resid <- xts::lag.xts(resid_EG, k = 1, na.pad = TRUE)
data <- merge(data, lag_resid)  # creates 'lag_resid' column aligned with time index
ecm <- lm(dlog_prod_trim ~ dlog_PIB_trim + lag_resid, data = data)
summary(ecm)