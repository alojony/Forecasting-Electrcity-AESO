# Load required libraries
library(astsa)


arima_1_1_2 <- Arima(training_set$Northwest, order = c(1, 1, 2), seasonal = c(0, 0, 0), xreg = reg_t)
dwtest(arima_1_1_2)
full_set$arima1_1_2_forecast <- NA
full_set$arima1_1_2_low_80 <- NA
full_set$arima1_1_2_low_95 <- NA
full_set$arima1_1_2_high_80 <- NA
full_set$arima1_1_2_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  xreg_t <- as.matrix(reg_f[t, , drop = FALSE])
  pred <- forecast(arima_1_1_2, h = 1, xreg = xreg_t)

  full_set$arima1_1_2_forecast[t] <- pred$mean[1]
  full_set$arima1_1_2_low_80[t] <- pred$lower[1]
  full_set$arima1_1_2_low_95[t] <- pred$lower[2]
  full_set$arima1_1_2_high_80[t] <- pred$upper[1]
  full_set$arima1_1_2_high_95[t] <- pred$upper[2]
}

arima_2_0_2 <- Arima(training_set$Northwest, order = c(2, 0, 2), seasonal = c(0, 0, 0), xreg = reg_t)
dwtest(arima_2_0_2)
full_set$arima2_0_2_forecast <- NA
full_set$arima2_0_2_low_80 <- NA
full_set$arima2_0_2_low_95 <- NA
full_set$arima2_0_2_high_80 <- NA
full_set$arima2_0_2_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  xreg_t <- as.matrix(reg_f[t, , drop = FALSE])
  pred <- forecast(arima_2_0_2, h = 1, xreg = xreg_t)

  full_set$arima2_0_2_forecast[t] <- pred$mean[1]
  full_set$arima2_0_2_low_80[t] <- pred$lower[1]
  full_set$arima2_0_2_low_95[t] <- pred$lower[2]
  full_set$arima2_0_2_high_80[t] <- pred$upper[1]
  full_set$arima2_0_2_high_95[t] <- pred$upper[2]
}


arima_2_1_1 <- Arima(training_set$Northwest, order = c(2, 1, 1), seasonal = c(0, 0, 0), xreg = reg_t)
dwtest(arima_2_1_1)
full_set$arima2_1_1_forecast <- NA
full_set$arima2_1_1_low_80 <- NA
full_set$arima2_1_1_low_95 <- NA
full_set$arima2_1_1_high_80 <- NA
full_set$arima2_1_1_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  xreg_t <- as.matrix(reg_f[t, , drop = FALSE])
  pred <- forecast(arima_2_1_1, h = 1, xreg = xreg_t)

  full_set$arima2_1_1_forecast[t] <- pred$mean[1]
  full_set$arima2_1_1_low_80[t] <- pred$lower[1]
  full_set$arima2_1_1_low_95[t] <- pred$lower[2]
  full_set$arima2_1_1_high_80[t] <- pred$upper[1]
  full_set$arima2_1_1_high_95[t] <- pred$upper[2]
}


arima_3_0_0 <- Arima(training_set$Northwest, order = c(3, 0, 0), seasonal = c(0, 0, 0), xreg = reg_t)
dwtest(arima_3_0_0)
full_set$arima3_0_0_forecast <- NA
full_set$arima3_0_0_low_80 <- NA
full_set$arima3_0_0_low_95 <- NA
full_set$arima3_0_0_high_80 <- NA
full_set$arima3_0_0_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  xreg_t <- as.matrix(reg_f[t, , drop = FALSE])
  pred <- forecast(arima_3_0_0, h = 1, xreg = xreg_t)

  full_set$arima3_0_0_forecast[t] <- pred$mean[1]
  full_set$arima3_0_0_low_80[t] <- pred$lower[1]
  full_set$arima3_0_0_low_95[t] <- pred$lower[2]
  full_set$arima3_0_0_high_80[t] <- pred$upper[1]
  full_set$arima3_0_0_high_95[t] <- pred$upper[2]
}


# Plot ACF and PACF of residuals
# tsdiag(arima_1_1_2)