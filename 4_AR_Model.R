# ***************************** #
#    Forecasting Electricity    #
#    Northwest Alberta.         #
# --- Preprocessing Script----  #
# Outlier treatment             #
# Temperature & Weather Data    #
# Noise for Regresors on Temp.  #
# ---------------------------   #
#      Jonathan Gonzalez.       #
#      Ghali Lahlou             #
#     Xavier Péeladeau-Asselin  #
#        April 18 2024.         #
# ***************************** #


# dummies
full_set$IsHoliday <- as.numeric(full_set$IsHoliday)
full_set$IsWinter <- as.numeric(full_set$season == "Winter")
full_set$IsSpring <- as.numeric(full_set$season == "Spring")
full_set$IsSummer <- as.numeric(full_set$season == "Summer")
full_set$IsFall <- as.numeric(full_set$season == "Autumn")

full_set$lag_HDD[is.na(full_set$lag_HDD)] <- 0
full_set$lag2_HDD[is.na(full_set$lag2_HDD)] <- 0
full_set$lag_CDD[is.na(full_set$lag_CDD)] <- 0
full_set$lag2_CDD[is.na(full_set$lag2_CDD)] <- 0

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

# Prepare the data for arx
full_set$IsHoliday <- as.numeric(full_set$IsHoliday)
full_set$lag1_Northwest <- c(NA, full_set$Northwest[-length(full_set$Northwest)])  # Create lagged NW data
full_set <- na.omit(full_set)  # Remove NAs that were introduced by lagging


training_end <- as.Date("2015-12-31")
validation_end <- as.Date("2017-12-31")

# Subset the data into training, validation, and test sets
training_set <- subset(full_set, DT_MST <= training_end)
# Drop the first 100 rows
training_set <- training_set[-(1:100), ]

test_set <- subset(full_set, DT_MST > validation_end)
validation_set <-
  subset(full_set, DT_MST > training_end & DT_MST <= validation_end)



Yt <- timeSeries(training_set$Northwest, training_set$DT_MST)


# # Define column names based on the variables included in reg_t and reg_f
column_names <- c(
  "IsHoliday",
  "IsWeekend",
  "HDD",
  "noisy_humidity_avg"
)

# External regressors for training
reg_t <- cbind(
  training_set$IsHoliday,
  training_set$IsWeekend,
  training_set$HDD,
  training_set$noisy_humidity_avg
)
# Assign column names to reg_t
colnames(reg_t) <- column_names

# Prepare the external regressors for the validation set
reg_f <- cbind(
  full_set$IsHoliday,
  full_set$IsWeekend,
  full_set$HDD,
  full_set$noisy_humidity_avg
)
colnames(reg_f) <- column_names

# Pre-allocate the space for ar_forecast
full_set$ar_forecast <- NA
full_set$ar_low_80 <- NA
full_set$ar_low_95 <- NA
full_set$ar_high_80 <- NA
full_set$ar_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  if (t %% 50 == 0) {
    print(paste("Processing AR: row", t, "of", nrow(full_set)))
    ar_model <-
      auto.arima(
        Yt[1:(t-1)],
        xreg = reg_f[1:(t-1), , drop = FALSE],
        d = 0,
        max.p = 3,
        max.q = 0,
        seasonal = FALSE
      )
  }
  pred <- forecast(ar_model, h = 1, xreg = as.matrix(reg_f[t, , drop = FALSE]))

  full_set$ar_forecast[t] <- pred$mean[1]
  full_set$ar_low_80[t] <- pred$lower[1]
  full_set$ar_low_95[t] <- pred$lower[2]
  full_set$ar_high_80[t] <- pred$upper[1]
  full_set$ar_high_95[t] <- pred$upper[2]
}


# Pre-allocate the space for lm_forecast
full_set$lm_forecast <- NA
full_set$lm_low_80 <- NA
full_set$lm_low_95 <- NA
full_set$lm_high_80 <- NA
full_set$lm_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  if (t %% 50 == 0) {
    print(paste("Processing LM: row", t, "of", nrow(full_set)))
    lm_model <- lm(Yt[1:(t - 1)] ~ ., data =  as.data.frame(reg_f[1:(t-1), , drop = FALSE]))
  }

  pred <- forecast(lm_model, h = 1, newdata = as.data.frame(reg_f[t, , drop = FALSE]))

  full_set$lm_forecast[t] <- pred$mean[1]
  full_set$lm_low_80[t] <- pred$lower[1]
  full_set$lm_low_95[t] <- pred$lower[2]
  full_set$lm_high_80[t] <- pred$upper[1]
  full_set$lm_high_95[t] <- pred$upper[2]
}

par(mfrow = c(1, 1))
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
  "bottom",
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


#----ARX-----
# Create a temporary dataframe for fitting the model (similar to df.tmp)
train_df.tmp <- data.frame(
  y = training_set$Northwest,
  lag1y = training_set$lag1_Northwest,
  IsHoliday = training_set$IsHoliday,
  HDD = training_set$HDD,
  CDD = training_set$CDD,
  Feb = training_set$Feb,
  Mar = training_set$Mar,
  Apr = training_set$Apr,
  May = training_set$May,
  Jun = training_set$Jun,
  Jul = training_set$Jul,
  Aug = training_set$Aug,
  Sep = training_set$Sep,
  Oct = training_set$Oct,
  Nov = training_set$Nov,
  Dec = training_set$Dec,
  noisy_wind_chill = training_set$noisy_wind_chill,
  noisy_humidity_avg = training_set$noisy_humidity_avg,
  noisy_temp = training_set$noisy_temp
)

full_df.tmp <- data.frame(
  y = full_set$Northwest,
  lag1y = full_set$lag1_Northwest,
  IsHoliday = full_set$IsHoliday,
  HDD = full_set$HDD,
  CDD = full_set$CDD,
  Feb = full_set$Feb,
  Mar = full_set$Mar,
  Apr = full_set$Apr,
  May = full_set$May,
  Jun = full_set$Jun,
  Jul = full_set$Jul,
  Aug = full_set$Aug,
  Sep = full_set$Sep,
  Oct = full_set$Oct,
  Nov = full_set$Nov,
  Dec = full_set$Dec,
  noisy_wind_chill = full_set$noisy_wind_chill,
  noisy_humidity_avg = full_set$noisy_humidity_avg,
  noisy_temp = full_set$noisy_temp
)

# Fitting the linear model
# arx_model <- lm(y ~ ., data=train_df.tmp)
# print(summary(arx_model))

# If you want to use dynlm, you can use the following code
# install.packages("dynlm") 
library(dynlm)

full_set$arx_forecast <- NA
full_set$arx_low_80 <- NA
full_set$arx_low_95 <- NA
full_set$arx_high_80 <- NA
full_set$arx_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  if (t %% 50 == 0) {
    print(paste("Processing ARx: row", t, "of", nrow(full_set)))
    arx_model <- dynlm(y ~ ., data=train_df.tmp[1:(t-1), , drop = FALSE])
  }

  pred <- forecast(arx_model, h = 1, newdata = as.data.frame(full_df.tmp[t, , drop = FALSE]))

  full_set$arx_forecast[t] <- pred$mean[1]
  full_set$arx_low_80[t] <- pred$lower[1]
  full_set$arx_low_95[t] <- pred$lower[2]
  full_set$arx_high_80[t] <- pred$upper[1]
  full_set$arx_high_95[t] <- pred$upper[2]
}


par(mfrow=c(2,2))
plot(arx_model)

# Preparing new data for forecasting
# Preparing new data for forecasting using the same column structure as df.tmp
new_data <- data.frame(
  lag1y = validation_set$lag1_Northwest,
  IsHoliday = validation_set$IsHoliday,
  HDD = validation_set$HDD,
  CDD = validation_set$CDD,
  Feb = validation_set$Feb,
  Mar = validation_set$Mar,
  Apr = validation_set$Apr,
  May = validation_set$May,
  Jun = validation_set$Jun,
  Jul = validation_set$Jul,
  Aug = validation_set$Aug,
  Sep = validation_set$Sep,
  Oct = validation_set$Oct,
  Nov = validation_set$Nov,
  Dec = validation_set$Dec,
  noisy_wind_chill = validation_set$noisy_wind_chill,
  noisy_humidity_avg = validation_set$noisy_humidity_avg,
  noisy_temp = validation_set$noisy_temp
)


# Forecast using the model
validation_set$forecast <- predict(arx_model, newdata=new_data, interval="prediction")


par(mfrow=c(1,1))
plot(validation_set$DT_MST, validation_set$Northwest, type='l', col='blue', 
     main="Forecast vs Actual", xlab="Date", ylab="Northwest")
lines(validation_set$DT_MST, validation_set$forecast[, "fit"], col='red')
legend("bottom", legend=c("Actual", "Forecast"), col=c("blue", "red"), lty=1)

# ----- Measures -----
# Residuals from ar
residuals_ar <- residuals(ar_model)
#acf(residuals_ar, main = "ACF of Residuals")
#pacf(residuals_ar, main = "PACF of Residuals")

# Residuals from lm
residuals_lm <- residuals(lm_model)
#acf(residuals_lm, main = "ACF of Residuals")
#pacf(residuals_lm, main = "PACF of Residuals")

# Residuals from arx
residuals_arx <- residuals(arx_model)
acf(residuals_arx, main = "ACF of ARX Residuals")
pacf(residuals_arx, main = "PACF of ARX Residuals")


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
  for (forecast_type in c("ar_forecast", "lm_forecast, arx_forecast")) {
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
for (forecast_type in c("ar_forecast", "lm_forecast, arx_forecast")) {
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

# ----- Durbin-Watson Test -----
library(lmtest)
# Directly use lm_model's residuals
residuals_lm <- residuals(lm_model)

# Fit a linear model on LM residuals
lm_res_lm <- lm(residuals_lm ~ seq_along(residuals_lm))

# Durbin-Watson test on LM residuals
dw_result_lm <- dwtest(lm_res_lm)
print(dw_result_lm)


# Assuming ar_model is an instance of Arima or similar
residuals_ar <- residuals(ar_model)

# Fit a linear model on residuals to apply the Durbin-Watson test
lm_res_ar <- lm(residuals_ar ~ seq_along(residuals_ar))

# Durbin-Watson test on AR model residuals
dw_result_ar <- dwtest(lm_res_ar)
print(dw_result_ar)

# Assuming arx_model is handled similarly and residuals are calculated
residuals_arx <- residuals(arx_model)  # Assuming arx_lm_model is your ARX model fitted with lm()

# Fit a linear model on ARX model residuals
lm_res_arx <- lm(residuals_arx ~ seq_along(residuals_arx))

# Durbin-Watson test on ARX model residuals
dw_result_arx <- dwtest(lm_res_arx)
print(dw_result_arx)



pdf("ARXplots.pdf")
plot(arx_model)

acf(residuals_arx, main = "ACF of ARX Residuals")
pacf(residuals_arx, main = "PACF of ARX Residuals")
dev.off()

