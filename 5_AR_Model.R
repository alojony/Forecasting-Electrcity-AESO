
# dummies
full_set$IsHoliday <- as.factor(full_set$IsHoliday)
full_set$IsWinter <- as.factor(full_set$season == "Winter")
full_set$IsSpring <- as.factor(full_set$season == "Spring")
full_set$IsSummer <- as.factor(full_set$season == "Summer")
full_set$IsFall <- as.factor(full_set$season == "Autumn")

training_end <- as.Date("2015-12-31")
validation_end <- as.Date("2017-12-31")



# Subset the data into training, validation, and test sets
training_set <- subset(full_set, DT_MST <= training_end)
test_set <- subset(full_set, DT_MST > validation_end)
validation_set <-
  subset(full_set, DT_MST > training_end & DT_MST <= validation_end)


#### WHAT USED BY PROF
#training_set$DT_MST <- as.Date(training_set$Date, format = "%Y-%m-%d")
#Yt <- timeSeries(training_set$Northwest, training_set$DT_MST)


#ONLY ONE THAT WORKS
Yt <- ts(training_set$Northwest, frequency = 365, start = c(1, 1))
reg_t<-cbind(training_set$IsHoliday, training_set$IsWinter,
             training_set$IsSpring, training_set$IsSummer)

reg_v<-cbind(validation_set$IsHoliday,  validation_set$IsWinter,
             validation_set$IsSpring, validation_set$IsSummer)


# Fit the AR model on the training set
ar_model <- auto.arima(Yt, xreg = reg_t, d = 0, max.p = 10, max.q = 0)


ar_forecast <- forecast(ar_model, xreg = reg_v, h = nrow(validation_set))
# Display the forecast plot
plot(ar_forecast)  


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
  



