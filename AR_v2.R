# dummies
full_set$IsHoliday <- as.numeric(full_set$IsHoliday)
# full_set$IsWinter <- as.numeric(full_set$season == "Winter")
# full_set$IsSpring <- as.numeric(full_set$season == "Spring")
# full_set$IsSummer <- as.numeric(full_set$season == "Summer")
# full_set$IsFall <- as.numeric(full_set$season == "Autumn")

full_set$lag_HDD[is.na(full_set$lag_HDD)] <- 0
full_set$lag2_HDD[is.na(full_set$lag2_HDD)] <- 0
full_set$lag_CDD[is.na(full_set$lag_CDD)] <- 0
full_set$lag2_CD[is.na(full_set$lag2_CD)] <- 0

full_set$Month <- format(full_set$DT_MST, "%m")
full_set$Jan <- as.numeric(full_set$Month == "01")
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
# Drop the first 100 rows
training_set <- training_set[-(1:100), ]

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
  training_set$HDD_lag1, #5
  training_set$CDD_lag1,
  training_set$HDD_lag2,
  training_set$CDD_lag2,
  training_set$Feb,
  training_set$Mar, #10
  training_set$Apr,
  training_set$May,
  training_set$Jun,
  training_set$Jul,
  training_set$Aug,
  training_set$Sep,
  training_set$Oct,
  training_set$Nov,
  training_set$Dec,
  training_set$wind_chill, #20
  #training_set$avg_temp,
  training_set$humidity_avg #22
)

# Fit the AR model on the training set
ar_model <-
  auto.arima(
    Yt,
    xreg = reg_t,
    d = 1,
    max.p = 1,
    max.q = 3,
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
  full_set$Mar,#10
  full_set$Apr,
  full_set$May,
  full_set$Jun,
  full_set$Jul,
  full_set$Aug,
  full_set$Sep,
  full_set$Oct,
  full_set$Nov,
  full_set$Dec,
  full_set$wind_chill,#20
  #full_set$avg_temp,
  full_set$humidity_avg
)

# Pre-allocate the space for ar_forecast
full_set$ar_forecast <- NA
full_set$ar_low_80 <- NA
full_set$ar_low_95 <- NA
full_set$ar_high_80 <- NA
full_set$ar_high_95 <- NA


### test 2

for (t in 700:(nrow(full_set) - 700)) {
  xreg_t <- as.matrix(reg_f[t, , drop = FALSE])  # Correctly prepared for each t
  pred <- forecast(ar_model, h = 1, xreg = xreg_t)  # Use xreg_t here
  
  full_set$ar_forecast[t] <- pred$mean[1]
  full_set$ar_low_80[t] <- pred$lower[1]
  full_set$ar_low_95[t] <- pred$lower[2]
  full_set$ar_high_80[t] <- pred$upper[1]
  full_set$ar_high_95[t] <- pred$upper[2]
}






plot(
  full_set$DT_MST,
  full_set$Northwest,
  type = "l",
  col = "blue",
  ylab = "Northwest",
  xlab = "Date"
)
lines(full_set$DT_MST, full_set$ar_forecast, col = "red")
legend(
  "topleft",
  legend = c("Actual", "AR-Forecast"),
  col = c("blue", "red"),
  lty = 1
)




# Residuals from ar
#residuals_ar <- residuals(ar_model)
#acf(residuals_ar, main = "ACF of Residuals")
#pacf(residuals_ar, main = "PACF of Residuals")

