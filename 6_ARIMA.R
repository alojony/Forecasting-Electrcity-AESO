# Load required libraries
library(astsa)

# Step 3: Visualize Data
# Plot your time series data
plot(full_set$Northwest, main = "Time Series Data")

# Step 4: Perform Differencing
# Check for stationarity and perform differencing if needed
Northwest_diff <- diff(full_set$Northwest)

pdf("sarima_diagns.pdf")
# Step 5: Identify Autocorrelation and Partial Autocorrelation
# ACF and PACF plots to identify potential values for p and q
acf(Northwest_diff, main = "ACF Plot")
pacf(Northwest_diff, main = "PACF Plot")

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
              sarima(full_set$Northwest, p = p, d = d, q = q, P = P, D = D, Q = Q, S=7),
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

# Find model with lowest AIC value
best_model_params <- names(which.min(unlist(results)))

# Step 7: Evaluate Model Fit
# Fit the best model
best_model <- sarima(full_set$Northwest, p = best_model_params[2], 
                     d = best_model_params[4], q = best_model_params[6],
                     P = best_model_params[8], D = best_model_params[10],
                     Q = best_model_params[12])

# Plot ACF and PACF of residuals
tsdiag(best_model)

# Step 10: Incorporate Regressors
# Once you have the best SARIMA model, incorporate regressors
# Fit SARIMA model with regressors
final_model <- sarima(full_set$Northwest, p = best_model_params[2], 
                      d = best_model_params[4], q = best_model_params[6],
                      P = best_model_params[8], D = best_model_params[10],
                      Q = best_model_params[12],
                      xreg = your_regressors)

# Forecast using the final model
forecast <- predict(final_model, n.ahead = forecast_horizon, 
                    newxreg = your_future_regressors)

# Plot forecast
plot(forecast)
