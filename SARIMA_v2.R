
# Load required libraries
library(astsa)

pdf("sarima_diagns.pdf")

# Step 6: Grid Search
# Grid search to find the best SARIMA parameters
# Define ranges for p, d, q, P, D, Q
p_range <- 0:1
d_range <- 0:1
q_range <- 0:1
P_range <- 0:1
D_range <- 0:1
Q_range <- 0:1

# Create empty list to store results
results <- list()
# Perform grid search
for (p in p_range) {
  for (d in d_range) {
    for (q in q_range) {
      for (P in P_range) {
        for (D in D_range) {
          for (Q in Q_range) {
            model <- tryCatch(
              # Fit SARIMA model
              sarima(training_set$Northwest, p = p, d = d, q = q, P = P, D = D, Q = Q, S=7, xreg = reg_t),
              error = function(e) NULL
            )
            Sys.sleep(2)
            if (!is.null(model)) {
              # Store AIC value and model parameters
              results[[paste("p", p, "d", d, "q", q, "P", P, "D", D, "Q", Q)]] <- model$ICs[1]
            }
          }
        }
      }
    }
  }
}


# Step 7: Evaluate Model Fit
# Fit the best model
arima_model <- auto.arima(training_set$Northwest, 
                           xreg = reg_t, 
                           start.p = 1, start.q = 2, 
                           start.P = 0, start.Q = 0,
                           d = 0, D = 0, 
                           max.p = 1, max.q = 2, 
                           max.P = 0, max.Q = 0,
                           max.d = 1, max.D = 0,
                           seasonal = FALSE)



northwest_ts <- ts(training_set$Northwest, frequency = 7)
sarima_model <- auto.arima(northwest_ts, 
                           xreg = reg_t, 
                           seasonal = TRUE, 
                           start.p = 2, start.q = 2, 
                           start.P = 1, start.Q = 0,
                           max.p = 2, max.q = 2, 
                           max.P = 1, max.Q = 2,
                           max.d = 2, max.D = 2)


sarima_auto <- auto.arima(northwest_ts, xreg = reg_t, seasonal = TRUE)

summary(arima_model)
summary(sarima_model)
# Plot ACF and PACF of residuals
tsdiag(sarima_model)

# Forecast using the final model
full_set$arima_forecast <- 0
full_set$arima_low_80 <- 0
full_set$arima_low_95 <- 0
full_set$arima_high_80 <- 0
full_set$arima_high_95 <- 0
full_set$sarima_forecast <- 0
full_set$sarima_low_80 <- 0
full_set$sarima_low_95 <- 0
full_set$sarima_high_80 <- 0
full_set$sarima_high_95 <- 0


for (t in 700:(nrow(full_set) - 700)) {
  xreg_t <- as.matrix(reg_f[t, , drop = FALSE])  # Correctly prepared for each t
  pred <- forecast(arima_model, h = 1, xreg = xreg_t)  # Use xreg_t here
  
  full_set$arima_forecast[t] <- pred$mean[1]
  full_set$arima_low_80[t] <- pred$lower[1]
  full_set$arima_low_95[t] <- pred$lower[2]
  full_set$arima_high_80[t] <- pred$upper[1]
  full_set$arima_high_95[t] <- pred$upper[2]
}


for (t in 700:(nrow(full_set) - 700)) {
  xreg_t <- as.matrix(reg_f[t, , drop = FALSE])  # Correctly prepared for each t
  pred <- forecast(sarima_model, h = 1, xreg = xreg_t)  # Use xreg_t here
  
  full_set$sarima_forecast[t] <- pred$mean[1]
  full_set$sarima_low_80[t] <- pred$lower[1]
  full_set$sarima_low_95[t] <- pred$lower[2]
  full_set$sarima_high_80[t] <- pred$upper[1]
  full_set$sarima_high_95[t] <- pred$upper[2]
}




for (t in 700:(nrow(full_set) - 700)) {
  xreg_t <- as.matrix(reg_f[t, , drop = FALSE])  # Correctly prepared for each t
  pred <- forecast(sarima_auto, h = 1, xreg = xreg_t)  # Use xreg_t here
  
  full_set$auto_sarima_forecast[t] <- pred$mean[1]
  full_set$auto_sarima_low_80[t] <- pred$lower[1]
  full_set$auto_sarima_low_95[t] <- pred$lower[2]
  full_set$auto_sarima_high_80[t] <- pred$upper[1]
  full_set$auto_sarima_high_95[t] <- pred$upper[2]
}






plot(
  full_set$DT_MST,
  full_set$Northwest,
  type = "l",
  col = "blue",
  ylab = "Northwest",
  xlab = "Date"
)
lines(full_set$DT_MST, full_set$arima_forecast, col = "red")
legend(
  "topleft",
  legend = c("Actual", "Arima-Forecast"),
  col = c("blue", "red"),
  lty = 1
)




plot(
  full_set$DT_MST,
  full_set$Northwest,
  type = "l",
  col = "blue",
  ylab = "Northwest",
  xlab = "Date"
)
lines(full_set$DT_MST, full_set$sarima_forecast, col = "red")
legend(
  "topleft",
  legend = c("Actual", "Sarima-Forecast"),
  col = c("blue", "red"),
  lty = 1
)



plot(
  full_set$DT_MST,
  full_set$Northwest,
  type = "l",
  col = "blue",
  ylab = "Northwest",
  xlab = "Date"
)
lines(full_set$DT_MST, full_set$auto_sarima_forecast, col = "red")
legend(
  "topleft",
  legend = c("Actual", "Sarima-Forecast"),
  col = c("blue", "red"),
  lty = 1
)

summary(sarima_auto)
