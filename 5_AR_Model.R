
# dummies
full_set$IsHoliday <- as.numeric(full_set$IsHoliday)
full_set$IsWinter <- as.numeric(full_set$season == "Winter")
full_set$IsSpring <- as.numeric(full_set$season == "Spring")
full_set$IsSummer <- as.numeric(full_set$season == "Summer")
full_set$IsFall <- as.numeric(full_set$season == "Autumn")

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


training_set <- training_set[!is.na(training_set$CDD_lag2) & !is.na(training_set$HDD_lag2), ]
Yt <- timeSeries(training_set$Northwest, training_set$DT_MST)

# External regressors for training
reg_t <- cbind(training_set$IsHoliday,training_set$IsWeekend,
               training_set$HDD,training_set$CDD,
               training_set$HDD_lag1, training_set$CDD_lag1,
               training_set$HDD_lag2, training_set$CDD_lag2,
               training_set$Feb, training_set$Mar,
               training_set$Apr, training_set$May,
               training_set$Jun, training_set$Jul, 
               training_set$Aug, training_set$Sep,
               training_set$Oct, training_set$Nov, training_set$Dec)


# Fit the AR model on the training set
ar_model <- 
  auto.arima(Yt, xreg = reg_t, d = 0, max.p = 20, max.q = 0, seasonal = FALSE)
summary(ar_model)

lm_model <- 
  auto.arima(Yt, xreg = reg_t, d = 0, max.p = 0, max.q = 0, seasonal = FALSE)

lm_model <- lm(Yt ~ ., data = as.data.frame(reg_t))
summary(lm_model)

# Prepare the external regressors for the validation set
reg_v <- cbind(validation_set$IsHoliday,validation_set$IsWeekend,
               validation_set$HDD, validation_set$CDD,
               validation_set$HDD_lag1, validation_set$CDD_lag1,
               validation_set$HDD_lag2, validation_set$CDD_lag2,
               validation_set$Feb, validation_set$Mar,
               validation_set$Apr, validation_set$May,
               validation_set$Jun, validation_set$Jul, 
               validation_set$Aug, validation_set$Sep,
               validation_set$Oct, validation_set$Nov, validation_set$Dec)



# Initialize a vector to store forecasts
ar_forecasts <- numeric(nrow(validation_set))
ar_forecasts <- predict(ar_model, n.ahead = nrow(validation_set), newxreg = reg_v)$pred
validation_set$ar_forecast <- ar_forecasts

ndata <- data.frame(reg_v)
colnames(ndata) <- paste0("V", 1:19)
lm_forecasts <- predict(lm_model, newdata = ndata)
validation_set$lm_forecast <- lm_forecasts


plot(validation_set$DT_MST, validation_set$Northwest, type = 'l', col = 'blue', ylab = 'Northwest', xlab = 'Date')
lines(validation_set$DT_MST, validation_set$ar_forecast, col = 'red')
legend('topleft', legend = c('Actual', 'AR-Forecast'), col = c('blue', 'red'), lty = 1)

plot(validation_set$DT_MST, validation_set$Northwest, type = 'l', col = 'blue', ylab = 'Northwest', xlab = 'Date')
lines(validation_set$DT_MST, validation_set$lm_forecast, col = 'red')
legend('topleft', legend = c('Actual', 'LM_Forecast'), col = c('blue', 'red'), lty = 1)


# Residuals from ar
residuals_ar <- residuals(ar_model)
acf(residuals_ar, main="ACF of Residuals")
pacf(residuals_ar, main="PACF of Residuals")

# Residuals from lm
residuals_lm <- residuals(lm_model)
acf(residuals_lm, main="ACF of Residuals")
pacf(residuals_lm, main="PACF of Residuals")


# Define the MAPE and %BIAS functions
mape <- function(actual, forecast) {
  mean(abs((actual - forecast) / actual), na.rm = TRUE) * 100
}

pct_bias <- function(actual, forecast) {
  mean((forecast - actual) / actual, na.rm = TRUE) * 100
}

# Initialize an empty data frame to store results
results <- data.frame(Season = character(),
                      Forecast_Type = character(),
                      MAPE = numeric(),
                      Pct_Bias = numeric(),
                      stringsAsFactors = FALSE)

# Loop through each season and calculate MAPE and %BIAS for AR and LM forecasts
seasons <- unique(validation_set$season)
for (season in seasons) {
  for (forecast_type in c("ar_forecast", "lm_forecast")) {
    subset_data <- validation_set[validation_set$season == season, ]
    mape_value <- mape(subset_data$Northwest, subset_data[[forecast_type]])
    pct_bias_value <- pct_bias(subset_data$Northwest, subset_data[[forecast_type]])
    
    results <- rbind(results, data.frame(Season = season,
                                         Forecast_Type = forecast_type,
                                         MAPE = mape_value,
                                         Pct_Bias = pct_bias_value))
  }
}

# Calculate total MAPE and %BIAS for the entire validation set
for (forecast_type in c("ar_forecast", "lm_forecast")) {
  mape_value <- mape(validation_set$Northwest, validation_set[[forecast_type]])
  pct_bias_value <- pct_bias(validation_set$Northwest, validation_set[[forecast_type]])
  
  results <- rbind(results, data.frame(Season = "Total",
                                       Forecast_Type = forecast_type,
                                       MAPE = mape_value,
                                       Pct_Bias = pct_bias_value))
}

# Print the final results
print(results)
