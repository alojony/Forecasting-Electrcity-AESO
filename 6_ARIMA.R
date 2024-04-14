# Load required libraries
library(astsa)



arima_1_1_2 <- sarima(training_set$Northwest, p = 1, d = 1, q = 2, P = 0, D = 0, Q = 0, S=7, xreg = reg_t)
arima_2_0_2 <- sarima(training_set$Northwest, p = 2, d = 0, q = 2, P = 0, D = 0, Q = 0, S=7, xreg = reg_t)
arima_2_1_1 <- sarima(training_set$Northwest, p = 2, d = 1, q = 1, P = 0, D = 0, Q = 0, S=7, xreg = reg_t)
ar_3_0_0 <- sarima(training_set$Northwest, p = 3, d = 0, q = 0, P = 0, D = 0, Q = 0, S=7, xreg = reg_t)



# Plot ACF and PACF of residuals
tsdiag(arima_1_1_2)

# Forecast using the final model
full_set$arima_forecast <- NA
full_set$arima_low_80 <- NA
full_set$arima_low_95 <- NA
full_set$arima_high_80 <- NA
full_set$arima_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  xreg_t <- as.matrix(reg_f[t, , drop = FALSE])
  pred <- forecast(ar_model, h = 1, xreg = xreg_t)

  full_set$arima_forecast[t] <- pred$mean[1]
  full_set$arima_low_80[t] <- pred$lower[1]
  full_set$arima_low_95[t] <- pred$lower[2]
  full_set$arima_high_80[t] <- pred$upper[1]
  full_set$arima_high_95[t] <- pred$upper[2]
}



# ----- Diebold-Mariano -----
# Diebold-Mariano Test
# Function to calculate the Diebold-Mariano test
dm_result <- dm.test(full_set$ar_forecast, full_set$arima_forecast, h = 1, alternative = "two.sided")

