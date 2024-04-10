# dummies
full_set$IsHoliday <- as.numeric(full_set$IsHoliday)
# full_set$IsWinter <- as.numeric(full_set$season == "Winter")
# full_set$IsSpring <- as.numeric(full_set$season == "Spring")
# full_set$IsSummer <- as.numeric(full_set$season == "Summer")
# full_set$IsFall <- as.numeric(full_set$season == "Autumn")

full_set$Month <- format(full_set$DT_MST, "%m")
full_set$Feb <- as.numeric(full_set$Month == "02")
full_set$Mar <- as.numeric(full_set$Month == "03")
full_set$Apr <- as.numeric(full_set$Month == "04")
full_set$May <- as.numeric(full_set$Month == "05")
full_set$Jun <- as.numeric(full_set$Month == "06")
full_set$Jul <- as.numeric(full_set$Month == "07")
full_set$Aug <- as.numeric(full_set$Month == "08")
full_set$Sep <- as.numeric(full_set$Month == "09")
full_set$Oct <- as.numeric(full_set$Month == "10")
full_set$Nov <- as.numeric(full_set$Month == "11")
full_set$Dec <- as.numeric(full_set$Month == "12")

training_end <- as.Date("2015-12-31")
validation_end <- as.Date("2017-12-31")

# Subset the data into training, validation, and test sets
training_set <- subset(full_set, DT_MST <= training_end)
test_set <- subset(full_set, DT_MST > validation_end)
validation_set <-
  subset(full_set, DT_MST > training_end & DT_MST <= validation_end)


# training_set <-
#   training_set[!is.na(training_set$CDD_lag2) &
#     !is.na(training_set$HDD_lag2), ]
Yt <- timeSeries(training_set$Northwest, training_set$DT_MST)

training_meteo <- subset(meteo_data, date <= training_end)

# External regressors for training
reg_t <- cbind(
  training_set$IsHoliday,
  training_set$IsWeekend,
  training_set$HDD,
  training_set$CDD,
  training_set$HDD_lag1,
  training_set$CDD_lag1,
  training_set$HDD_lag2,
  training_set$CDD_lag2,
  training_set$Feb,
  training_set$Mar,
  training_set$Apr,
  training_set$May,
  training_set$Jun,
  training_set$Jul,
  training_set$Aug,
  training_set$Sep,
  training_set$Oct,
  training_set$Nov,
  training_set$Dec,
  training_meteo$wind_chill,
  training_meteo$avg_temp,
  training_meteo$humidity_avg
)


# Fit the AR model on the training set
ar_model <-
  auto.arima(
    Yt,
    xreg = reg_t,
    d = 0,
    max.p = 3,
    max.q = 0,
    seasonal = FALSE
  )
summary(ar_model)

# Prepare the external regressors for the validation set
reg_f <- cbind(
  full_set$IsHoliday,
  full_set$IsWeekend,
  full_set$HDD,
  full_set$CDD,
  full_set$HDD_lag1,
  full_set$CDD_lag1,
  full_set$HDD_lag2,
  full_set$CDD_lag2,
  full_set$Feb,
  full_set$Mar,
  full_set$Apr,
  full_set$May,
  full_set$Jun,
  full_set$Jul,
  full_set$Aug,
  full_set$Sep,
  full_set$Oct,
  full_set$Nov,
  full_set$Dec,
  meteo_data$wind_chill,
  meteo_data$avg_temp,
  meteo_data$humidity_avg
)

# Pre-allocate the space for ar_forecast
full_set$ar_forecast <- NA
full_set$ar_low_80 <- NA
full_set$ar_low_95 <- NA
full_set$ar_high_80 <- NA
full_set$ar_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  xreg_t <- as.matrix(reg_f[t, , drop = FALSE])
  pred <- forecast(ar_model, h = 1, xreg = xreg_t)

  full_set$ar_forecast[t] <- pred$mean[1]
  full_set$ar_low_80[t] <- pred$lower[1]
  full_set$ar_low_95[t] <- pred$lower[2]
  full_set$ar_high_80[t] <- pred$upper[1]
  full_set$ar_high_95[t] <- pred$upper[2]
}


# Initialize a vector to store forecasts
# ar_forecasts <- numeric(nrow(validation_set))
# ar_forecasts <-
#   predict(ar_model,
#     n.ahead = nrow(validation_set),
#     newxreg = reg_v
#   )$pred
# validation_set$ar_forecast <- ar_forecasts




# ndata <- data.frame(reg_v)
# colnames(ndata) <- paste0("V", 1:19)
# lm_forecasts <- predict(lm_model, newdata = ndata)
# validation_set$lm_forecast <- lm_forecasts


lm_model <- lm(Yt ~ ., data = as.data.frame(reg_t))
summary(lm_model)

# Pre-allocate the space for lm_forecast
full_set$lm_forecast <- NA
full_set$lm_low_80 <- NA
full_set$lm_low_95 <- NA
full_set$lm_high_80 <- NA
full_set$lm_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  xreg_t <- as.data.frame(reg_f[t, , drop = FALSE])

  pred <- forecast(lm_model, h = 1, newdata = xreg_t)

  full_set$lm_forecast[t] <- pred$mean[1]
  full_set$lm_low_80[t] <- pred$lower[1]
  full_set$lm_low_95[t] <- pred$lower[2]
  full_set$lm_high_80[t] <- pred$upper[1]
  full_set$lm_high_95[t] <- pred$upper[2]
}


plot(
  validation_set$DT_MST,
  validation_set$Northwest,
  type = "l",
  col = "blue",
  ylab = "Northwest",
  xlab = "Date"
)
lines(validation_set$DT_MST, validation_set$ar_forecast, col = "red")
legend(
  "topleft",
  legend = c("Actual", "AR-Forecast"),
  col = c("blue", "red"),
  lty = 1
)

plot(
  validation_set$DT_MST,
  validation_set$Northwest,
  type = "l",
  col = "blue",
  ylab = "Northwest",
  xlab = "Date"
)
lines(validation_set$DT_MST, validation_set$lm_forecast, col = "red")
legend(
  "topleft",
  legend = c("Actual", "LM_Forecast"),
  col = c("blue", "red"),
  lty = 1
)


# Residuals from ar
residuals_ar <- residuals(ar_model)
acf(residuals_ar, main = "ACF of Residuals")
pacf(residuals_ar, main = "PACF of Residuals")

# Residuals from lm
residuals_lm <- residuals(lm_model)
acf(residuals_lm, main = "ACF of Residuals")
pacf(residuals_lm, main = "PACF of Residuals")


# Define the MAPE and %BIAS functions
mape <- function(actual, forecast) {
  mean(abs((actual - forecast) / actual), na.rm = TRUE) * 100
}

pct_bias <- function(actual, forecast) {
  mean((forecast - actual) / actual, na.rm = TRUE) * 100
}

# Initialize an empty data frame to store results
results <- data.frame(
  Season = character(),
  Forecast_Type = character(),
  MAPE = numeric(),
  Pct_Bias = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each season and calculate MAPE and %BIAS for AR and LM forecasts
seasons <- unique(validation_set$season)
for (season in seasons) {
  for (forecast_type in c("ar_forecast", "lm_forecast")) {
    subset_data <- validation_set[validation_set$season == season, ]
    mape_value <-
      mape(subset_data$Northwest, subset_data[[forecast_type]])
    pct_bias_value <-
      pct_bias(subset_data$Northwest, subset_data[[forecast_type]])

    results <- rbind(
      results,
      data.frame(
        Season = season,
        Forecast_Type = forecast_type,
        MAPE = mape_value,
        Pct_Bias = pct_bias_value
      )
    )
  }
}

# Calculate total MAPE and %BIAS for the entire validation set
for (forecast_type in c("ar_forecast", "lm_forecast")) {
  mape_value <-
    mape(validation_set$Northwest, validation_set[[forecast_type]])
  pct_bias_value <-
    pct_bias(validation_set$Northwest, validation_set[[forecast_type]])

  results <- rbind(
    results,
    data.frame(
      Season = "Total",
      Forecast_Type = forecast_type,
      MAPE = mape_value,
      Pct_Bias = pct_bias_value
    )
  )
}

# Print the final results
print(results)
