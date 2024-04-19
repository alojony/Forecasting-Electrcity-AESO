# ***************************** #
#    Forecasting Electricity    #
#    Northwest Alberta.         #
# ---- ARIMA Models.      ----  #
# ARIMA Models and Forecasts.   #
# ---------------------------   #
#      Jonathan Gonzalez.       #
#      Ghali Lahlou             #
#     Xavier Péeladeau-Asselin  #
#        April 18 2024.         #
# ***************************** #

full_set$arima1_1_2_forecast <- NA
full_set$arima1_1_2_low_80 <- NA
full_set$arima1_1_2_low_95 <- NA
full_set$arima1_1_2_high_80 <- NA
full_set$arima1_1_2_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {

  if (t %% 50 == 0) {
    print(paste("Processing ARIMA 1,1,2: row", t, "of", nrow(full_set)))
  }

  xreg_t <- as.matrix(reg_f[1:(t-1), , drop = FALSE])
  arima_1_1_2 <- Arima(full_set$Northwest[1:(t-1)], order = c(1, 1, 2), 
                       seasonal = c(0, 0, 0), xreg = xreg_t)

  pred <- forecast(arima_1_1_2, h = 1, xreg = as.matrix(reg_f[t, , drop = FALSE]))

  full_set$arima1_1_2_forecast[t] <- pred$mean[1]
  full_set$arima1_1_2_low_80[t] <- pred$lower[1]
  full_set$arima1_1_2_low_95[t] <- pred$lower[2]
  full_set$arima1_1_2_high_80[t] <- pred$upper[1]
  full_set$arima1_1_2_high_95[t] <- pred$upper[2]
}

full_set$arima2_0_2_forecast <- NA
full_set$arima2_0_2_low_80 <- NA
full_set$arima2_0_2_low_95 <- NA
full_set$arima2_0_2_high_80 <- NA
full_set$arima2_0_2_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  if (t %% 50 == 0) {
    print(paste("Processing ARIMA 2,0,2: row", t, "of", nrow(full_set)))
  }
  arima_2_0_2 <- Arima(full_set$Northwest[1:(t-1)], order = c(2, 0, 2), 
                       seasonal = c(0, 0, 0), xreg = as.matrix(reg_f[1:(t-1), ,
                                                                    drop = FALSE]))

  pred <- forecast(arima_2_0_2, h = 1, xreg = as.matrix(reg_f[t, , drop = FALSE]))

  full_set$arima2_0_2_forecast[t] <- pred$mean[1]
  full_set$arima2_0_2_low_80[t] <- pred$lower[1]
  full_set$arima2_0_2_low_95[t] <- pred$lower[2]
  full_set$arima2_0_2_high_80[t] <- pred$upper[1]
  full_set$arima2_0_2_high_95[t] <- pred$upper[2]
}


full_set$arima2_1_1_forecast <- NA
full_set$arima2_1_1_low_80 <- NA
full_set$arima2_1_1_low_95 <- NA
full_set$arima2_1_1_high_80 <- NA
full_set$arima2_1_1_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  if (t %% 50 == 0) {
    print(paste("Processing ARIMA 2,1,1: row", t, "of", nrow(full_set)))
  }

  arima_2_1_1 <- Arima(full_set$Northwest[1:(t-1)], order = c(2, 1, 1),
                       seasonal = c(0, 0, 0), xreg = as.matrix(reg_f[1:(t-1), ,
                                                                    drop = FALSE]))

  pred <- forecast(arima_2_1_1, h = 1, xreg = as.matrix(reg_f[t, , drop = FALSE]))

  full_set$arima2_1_1_forecast[t] <- pred$mean[1]
  full_set$arima2_1_1_low_80[t] <- pred$lower[1]
  full_set$arima2_1_1_low_95[t] <- pred$lower[2]
  full_set$arima2_1_1_high_80[t] <- pred$upper[1]
  full_set$arima2_1_1_high_95[t] <- pred$upper[2]
}



full_set$arima3_0_0_forecast <- NA
full_set$arima3_0_0_low_80 <- NA
full_set$arima3_0_0_low_95 <- NA
full_set$arima3_0_0_high_80 <- NA
full_set$arima3_0_0_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  if (t %% 50 == 0) {
    print(paste("Processing ARIMA 3,0,0: row", t, "of", nrow(full_set)))
  }

  arima_3_0_0 <- Arima(full_set$Northwest[1:(t-1)], order = c(3, 0, 0), 
                       seasonal = c(0, 0, 0), xreg = as.matrix(reg_f[1:(t-1), , 
                                                                     drop = FALSE]))

  pred <- forecast(arima_3_0_0, h = 1, xreg = as.matrix(reg_f[t, , drop = FALSE]))

  full_set$arima3_0_0_forecast[t] <- pred$mean[1]
  full_set$arima3_0_0_low_80[t] <- pred$lower[1]
  full_set$arima3_0_0_low_95[t] <- pred$lower[2]
  full_set$arima3_0_0_high_80[t] <- pred$upper[1]
  full_set$arima3_0_0_high_95[t] <- pred$upper[2]
}


full_set$arima1_0_2_forecast <- NA
full_set$arima1_0_2_low_80 <- NA
full_set$arima1_0_2_low_95 <- NA
full_set$arima1_0_2_high_80 <- NA
full_set$arima1_0_2_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  if (t %% 50 == 0) {
    print(paste("Processing ARIMA 1,0,2: row", t, "of", nrow(full_set)))
  }

  arima_1_0_2 <- Arima(full_set$Northwest[1:(t-1)], order = c(1, 0, 2), 
                       seasonal = c(0, 0, 0), xreg = as.matrix(reg_f[1:(t-1), , 
                                                                    drop = FALSE]))

  pred <- forecast(arima_1_0_2, h = 1, xreg = as.matrix(reg_f[t, , drop = FALSE]))

  full_set$arima1_0_2_forecast[t] <- pred$mean[1]
  full_set$arima1_0_2_low_80[t] <- pred$lower[1]
  full_set$arima1_0_2_low_95[t] <- pred$lower[2]
  full_set$arima1_0_2_high_80[t] <- pred$upper[1]
  full_set$arima1_0_2_high_95[t] <- pred$upper[2]
}


full_set$sarima_forecast <- NA
full_set$sarima_low_80 <- NA
full_set$sarima_low_95 <- NA
full_set$sarima_high_80 <- NA
full_set$sarima_high_95 <- NA

for (t in 100:(nrow(full_set) - 1)) {
  if (t %% 50 == 0) {
    print(paste("Processing SARIMA 2,1,2 0,0,1[7]: row", t, "of", nrow(full_set)))
  }
  northwest_ts <- ts(full_set$Northwest[1:(t-1)], frequency = 7)
  sarima <- Arima(northwest_ts, order = c(2, 1, 2), seasonal = c(0, 0, 1), 
                  xreg = as.matrix(reg_f[1:(t-1), , drop = FALSE]))

  pred <- forecast(sarima, h = 1, xreg = as.matrix(reg_f[t, , drop = FALSE]))

  full_set$sarima_forecast[t] <- pred$mean[1]
  full_set$sarima_low_80[t] <- pred$lower[1]
  full_set$sarima_low_95[t] <- pred$lower[2]
  full_set$sarima_high_80[t] <- pred$upper[1]
  full_set$sarima_high_95[t] <- pred$upper[2]
}
