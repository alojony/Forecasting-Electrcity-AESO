
# dummies
full_set$IsHoliday <- as.numeric(full_set$IsHoliday)
full_set$IsWinter <- as.numeric(full_set$season == "Winter")
full_set$IsSpring <- as.numeric(full_set$season == "Spring")
full_set$IsSummer <- as.numeric(full_set$season == "Summer")
full_set$IsFall <- as.numeric(full_set$season == "Autumn")

full_set$Month <- format(full_set$DT_MST, "%m")
full_set$Month <- as.factor(full_set$Month)
month_dummies <- model.matrix(~ as.factor(Month) - 1, data = full_set)
colnames(month_dummies) <- 
  c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
month_dummies <- month_dummies[, -1]
full_set <- cbind(full_set, month_dummies)
for (col in c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) {
  if (col %in% colnames(full_set)) {
    # Enforce the numeric type
    full_set[[col]] <- as.numeric(full_set[[col]])
  }
}

training_end <- as.Date("2015-12-31")
validation_end <- as.Date("2017-12-31")

# Subset the data into training, validation, and test sets
training_set <- subset(full_set, DT_MST <= training_end)
test_set <- subset(full_set, DT_MST > validation_end)
validation_set <-
  subset(full_set, DT_MST > training_end & DT_MST <= validation_end)


training_set <- training_set[!is.na(training_set$CDD_lag1) & !is.na(training_set$HDD_lag1), ]
#training_set$DT_MST <- as.Date(training_set$Date, format = "%Y-%m-%d")
Yt <- timeSeries(training_set$Northwest, training_set$DT_MST)

# External regressors for training
reg_t <- cbind(training_set$IsHoliday,training_set$IsWeekend,
               training_set$CDD, training_set$HDD,
               training_set$Feb, training_set$Mar,
               training_set$Apr, training_set$May,
               training_set$Jun, training_set$Jul, 
               training_set$Aug, training_set$Sep,
               training_set$Oct, training_set$Nov, training_set$Dec)


# Fit the AR model on the training set
ar_model <- 
  auto.arima(Yt, xreg = reg_t, d = 0, max.p = 20, max.q = 0, seasonal = FALSE)
summary(ar_model)

# Prepare the external regressors for the validation set
reg_v <- cbind(validation_set$IsHoliday,validation_set$IsWeekend,
               validation_set$CDD, validation_set$HDD,
               validation_set$Feb, validation_set$Mar,
               validation_set$Apr, validation_set$May,
               validation_set$Jun, validation_set$Jul, 
               validation_set$Aug, validation_set$Sep,
               validation_set$Oct, validation_set$Nov, validation_set$Dec)


pred_AR <- predict(ar_model, n.ahead=1, newxreg=reg_v)


# Initialize a vector to store forecasts
ar_forecasts <- numeric(nrow(validation_set))

# Loop through the validation set to make one-step ahead forecasts
for (i in 1:nrow(validation_set)) {
  ar_forecasts[i] <- predict(ar_model, n.ahead = 1, newxreg = reg_v[i, ])$mean
}

# Add the forecasts to the validation set for comparison
validation_set$ar_forecast <-  ar_forecasts

# Now you can plot the forecasts against the actual values
plot(validation_set$Date, validation_set$Northwest, type = 'l', col = 'blue', ylab = 'Northwest', xlab = 'Date')
lines(validation_set$Date, validation_set$ar_forecast, col = 'red')
legend('topleft', legend = c('Actual', 'Forecast'), col = c('blue', 'red'), lty = 1)


# Residuals from the model
residuals <- residuals(ar_model)

acf(residuals, main="ACF of Residuals")
pacf(residuals, main="PACF of Residuals")


# Store the point forecast and confidence intervals
validation_set$AR_point_forecast <- ar_forecast$mean
validation_set$AR_low_80 <- ar_forecast$lower[, "80%"]
validation_set$AR_high_80 <- ar_forecast$upper[, "80%"]
validation_set$AR_low_95 <- ar_forecast$lower[, "95%"]
validation_set$AR_high_95 <- ar_forecast$upper[, "95%"]


# Calculate the accuracy measures for each season in the validation set
for (s in unique(full_set$season)) {
  seasonal_subset <- subset(validation_set, season == s)
  
  accuracy_measures[["AR"]][[s]] <- list(
    mape =  mape(seasonal_subset$ar_forecast, seasonal_subset$Northwest),
    pct_bias = pct_bias(seasonal_subset$ar_forecast, seasonal_subset$Northwest)
    
  )
}
  for (s in unique(full_set$season)) {
    seasonal_measures <- accuracy_measures[["AR"]][[s]]
    print(cat("Season:", s, "\n"))
    cat("  MAPE:", seasonal_measures$mape, "\n")
    cat("  Percentage Bias:", seasonal_measures$pct_bias, "\n\n")
  }
  

