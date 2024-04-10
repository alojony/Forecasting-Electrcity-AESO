# Load required libraries
library(astsa)

pdf("sarima_diagns.pdf")

# Step 6: Grid Search
# Grid search to find the best SARIMA parameters
# Define ranges for p, d, q, P, D, Q
p_range <- 0:2
d_range <- 0:1
q_range <- 0:2
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
best_model <- sarima(full_set$Northwest, p = 0, d = 0, q = 0, P = 0, D = 0, Q = 0, S = 7, xreg = reg_f)

# Plot ACF and PACF of residuals
tsdiag(best_model)

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

