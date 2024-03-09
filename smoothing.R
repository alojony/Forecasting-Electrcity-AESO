# ----- Smoothing Methods ----

# Simple Exponential Smoothing

# Find optimal value for hyperparameter alpha (using training set)
optimal_alpha <- ses(training_set$Northwest, h = 1)$model$par[[1]]
full_set$ses_forecast <- NA # Initialize forecast column

# Make prediction for each t with data until t-1
for (t in 2:nrow(full_set)) {
  ses_model <-
    ses(full_set[1:(t - 1), "Northwest"], h = 1, alpha = optimal_alpha)
  full_set$ses_forecast[t] <- ses_model$mean[1]
}


# 2-parameters Holt Method
optimal_params <- holt(training_set$Northwest, h = 1)$model$par
optimal_alpha <- optimal_params[[1]]
optimal_beta <- optimal_params[[2]]

full_set$holt_forecast <- NA # Initialize forecast column

# Make prediction for each t with data until t-1
for (t in 3:nrow(full_set)) {
  holt_model <-
    holt(full_set[1:(t - 1), "Northwest"],
         h = 1,
         alpha = optimal_alpha,
         beta = optimal_beta)
  full_set$holt_forecast[t] <- holt_model$mean[1]
}

# Holt-Winters 7-day seasonality
optimal_model <-
  hw(ts(training_set$Northwest, frequency = 7), h = 1)$model

full_set$holt_winters_forecast <- NA # Initialize forecast column

# Make prediction for each t with data until t-1
for (t in 11:nrow(full_set)) {
  holt_winters_model <-
    hw(ts(full_set[1:(t - 1), "Northwest"], frequency = 7),
       h = 1,
       model = optimal_model)
  full_set$holt_winters_forecast[t] <- holt_winters_model$mean[1]
}


# BATS
optimal_model <- bats(training_set$Northwest)

full_set$bats_forecast <- NA

for (t in 1:nrow(full_set)) {
  bats_model <-
    bats(full_set[1:(t - 1), "Northwest"], model = optimal_model)
  full_set$bats_forecast[t] <- forecast(bats, h = 10)
}

plot(forecast(optimal_model)$mean)