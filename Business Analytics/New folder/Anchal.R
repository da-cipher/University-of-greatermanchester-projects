# Install required package (run once)
install.packages("mFilter")

# Load required libraries
library(readxl)    # for reading Excel
library(dplyr)     # for data manipulation
library(ggplot2)   # for plotting
library(zoo)       # for interpolation (na.approx)
library(forecast)  # for outlier detection and cleaning
library(mFilter)   # for HP filter
library(tseries)   # for adf.test()
library(urca)      # for KPSS via ur.kpss()

# 1. Read in the “Table 1 Weekly” sheet, skipping the first 5 rows of metadata
fuel_raw <- read_excel(
  path      = "fuel_price.xlsx",
  sheet     = "Table 1 Weekly",
  skip      = 5,
  col_names = TRUE
)

# 2. Select only date and average fuel price, then rename
fuel <- fuel_raw %>%
  select(
    week_ending    = `Week ending`,
    avg_fuel_price = `Average fuel price`
  )

# 3. Convert types: ensure week_ending is Date, avg_fuel_price is numeric
fuel <- fuel %>%
  mutate(
    week_ending    = as.Date(week_ending),
    avg_fuel_price = as.numeric(avg_fuel_price)
  )

# 4. Check for missing values
fuel %>%
  summarise(
    missing = sum(is.na(avg_fuel_price))
  )
# 4. Impute interior gaps via linear interpolation
fuel <- fuel %>%
  arrange(week_ending) %>%
  mutate(
    avg_fuel_price = na.approx(avg_fuel_price, na.rm = FALSE)
  )


# ─── 6. OUTLIER DETECTION & HANDLING (IQR METHOD) ───────────────────────────
iqr_vals <- IQR(fuel$avg_fuel_price, na.rm = TRUE)
q1       <- quantile(fuel$avg_fuel_price, 0.25, na.rm = TRUE)
q3       <- quantile(fuel$avg_fuel_price, 0.75, na.rm = TRUE)
lower_f  <- q1 - 1.5 * iqr_vals
upper_f  <- q3 + 1.5 * iqr_vals

# Identify outlier indices
out_idx <- which(fuel$avg_fuel_price < lower_f | 
                   fuel$avg_fuel_price > upper_f)

# Visualise IQR-based outliers on the interpolated series
ggplot(fuel, aes(week_ending, avg_fuel_price)) +
  geom_line() +
  geom_point(data = fuel[out_idx, ], aes(week_ending, avg_fuel_price), colour = "red") +
  labs(title = "Fuel Price with IQR-Based Outliers Highlighted")

# Replace outliers with NA, re-interpolate, then boundary-fill
clean_fuel <- fuel %>%
  mutate(
    avg_fuel_price = replace(avg_fuel_price, out_idx, NA),
    avg_fuel_price = na.approx(avg_fuel_price, na.rm = FALSE)
  ) %>%
  mutate(
    avg_fuel_price = na.locf(avg_fuel_price,       na.rm = FALSE),
    avg_fuel_price = na.locf(avg_fuel_price, fromLast = TRUE, na.rm = FALSE)
  )

# Confirm no NAs remain
clean_fuel %>% summarise(missing_post = sum(is.na(avg_fuel_price)))


# 7. Convert cleaned series to ts → cleaned_ts
start_year   <- as.numeric(format(min(clean_fuel$week_ending), "%Y"))
start_week   <- as.numeric(format(min(clean_fuel$week_ending), "%V"))
cleaned_ts <- ts(
  data      = clean_fuel$avg_fuel_price,
  start     = c(start_year, start_week),
  frequency = 52
)

#visualise the time series
autoplot(cleaned_ts) +
  labs(
    title = "Cleaned Weekly Average Fuel Price",
    x     = "Week Ending",
    y     = "Price (pence per litre)"
  ) +
  theme_minimal()

# 10. Merge the cleaned series back into a data frame
clean_fuel <- fuel %>%
  mutate(avg_fuel_price = as.numeric(cleaned_ts))

# 11. STL decomposition (trend / seasonal / remainder)
decomp_stl <- stl(cleaned_ts, s.window = "periodic")
autoplot(decomp_stl) +
  labs(
    title = "STL Decomposition of Cleaned Fuel Price",
    x     = "Week Ending"
  ) +
  theme_minimal()

# 12. Hodrick–Prescott filter decomposition
hp <- hpfilter(cleaned_ts, type = "lambda", freq = 129600)
autoplot(cleaned_ts, series = "Cleaned") +
  autolayer(hp$trend, series = "HP Trend") +
  autolayer(hp$cycle, series = "HP Cycle") +
  labs(
    title = "HP Filter Decomposition",
    x     = "Week Ending"
  ) +
  theme_minimal()

# 13. Final structure check of cleaned data frame
str(clean_fuel)

# ── Stationarity checks and differencing ─────────────────────────────────────

# 14.1 Augmented Dickey–Fuller test (H₀: non-stationary)
adf_result <- adf.test(cleaned_ts)
print(adf_result)


# 14.3 If non-stationary, difference once
adf_p <- adf.test(cleaned_ts)$p.value
if (adf_p > 0.05) {
  ts_for_model <- diff(cleaned_ts, differences = 1)
  message("Differenced once (ADF p=", round(adf_p,3), ").")
} else {
  ts_for_model <- cleaned_ts
  message("Series stationary (ADF p=", round(adf_p,3), ").")
}

# 14.4 Plot the differenced series
autoplot(fuel_diff1) +
  labs(
    title = "First Difference of Cleaned Fuel Price",
    x     = "Week Ending",
    y     = "Δ Price"
  ) +
  theme_minimal()

# ── ACF and PACF for model order selection ───────────────────────────────────

# 15. ACF plot
Acf(ts_for_model, main = "ACF of Series for Modelling")

# 16. PACF plot
Pacf(ts_for_model, main = "PACF of Series for Modelling")

# ──────────────────────────────────────────────────────────────────────────────
# Check autocorrelation using Ljung–Box test
# ──────────────────────────────────────────────────────────────────────────────

# 1. Plot the ACF with significance bounds
Acf(ts_for_model, main = "ACF of Series for Autocorrelation Check")

# 2. Perform Ljung–Box tests at a few lags
lb_10 <- Box.test(ts_for_model, lag = 10, type = "Ljung-Box", fitdf = 0)
lb_20 <- Box.test(ts_for_model, lag = 20, type = "Ljung-Box", fitdf = 0)

# 3. Print results
cat("Ljung–Box test (lag=10): p-value =", round(lb_10$p.value, 4), "\n")
cat("Ljung–Box test (lag=20): p-value =", round(lb_20$p.value, 4), "\n")

# 16. Define forecast horizon and split into training and test sets
h <- 12
freq <- frequency(ts_for_model)
end_time <- tsp(ts_for_model)[2]
train_end <- end_time - (h / freq)

# Examine what ts_for_model actually contains
print(head(ts_for_model, 20))
print(tail(ts_for_model, 20))
summary(ts_for_model)


train_ts <- window(ts_for_model, end = train_end)
test_ts  <- window(ts_for_model, start = train_end + 1/freq)

# 6. Train/test split
h        <- 12
freq     <- frequency(ts_for_model)
end_time <- tsp(ts_for_model)[2]
train_end<- end_time - (h / freq)

train_ts <- window(ts_for_model, end   = train_end)
test_ts  <- window(ts_for_model, start = train_end + 1/freq)

# 7. Fit at least five univariate models on train set
fit_arima   <- auto.arima(train_ts)
fit_stlf    <- stlf(train_ts, method = "ets")
fit_naive   <- naive(train_ts)
fit_drift   <- rwf(train_ts, drift = TRUE)
fit_theta   <- thetaf(train_ts, h = h)
fit_tbats   <- tbats(train_ts)
fit_nnetar  <- nnetar(train_ts)

# 8. Forecast h steps ahead
fc_arima   <- forecast(fit_arima,  h = h)
fc_stlf    <- forecast(fit_stlf,   h = h)
fc_naive   <- forecast(fit_naive,  h = h)
fc_drift   <- forecast(fit_drift,  h = h)
fc_theta   <- forecast(fit_theta,  h = h)
fc_tbats   <- forecast(fit_tbats,  h = h)
fc_nnetar  <- forecast(fit_nnetar, h = h)

# 9. Plot all forecasts vs actual
autoplot(test_ts, series = "Actual") +
  autolayer(fc_arima,   series = "ARIMA") +
  autolayer(fc_stlf,    series = "STLF(ETS)") +
  autolayer(fc_naive,   series = "NAIVE") +
  autolayer(fc_drift,   series = "DRIFT") +
  autolayer(fc_theta,   series = "THETA") +
  autolayer(fc_tbats,   series = "TBATS") +
  autolayer(fc_nnetar,  series = "NNETAR") +
  labs(
    title = "Forecast Comparison vs Test Data",
    x     = "Week Ending",
    y     = "Avg Fuel Price"
  ) +
  theme_minimal()

# 10. Compute accuracy metrics on test set
acc <- data.frame(
  Model   = c("ARIMA","STLF(ETS)","NAIVE","DRIFT","THETA","TBATS","NNETAR"),
  RMSE    = c(accuracy(fc_arima,  test_ts)[2,"RMSE"],
              accuracy(fc_stlf,   test_ts)[2,"RMSE"],
              accuracy(fc_naive,  test_ts)[2,"RMSE"],
              accuracy(fc_drift,  test_ts)[2,"RMSE"],
              accuracy(fc_theta,  test_ts)[2,"RMSE"],
              accuracy(fc_tbats,  test_ts)[2,"RMSE"],
              accuracy(fc_nnetar, test_ts)[2,"RMSE"]),
  MAE     = c(accuracy(fc_arima,  test_ts)[2,"MAE"],
              accuracy(fc_stlf,   test_ts)[2,"MAE"],
              accuracy(fc_naive,  test_ts)[2,"MAE"],
              accuracy(fc_drift,  test_ts)[2,"MAE"],
              accuracy(fc_theta,  test_ts)[2,"MAE"],
              accuracy(fc_tbats,  test_ts)[2,"MAE"],
              accuracy(fc_nnetar, test_ts)[2,"MAE"])
)

print(acc)






