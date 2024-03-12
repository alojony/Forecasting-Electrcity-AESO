Sys.setlocale("LC_TIME", "C")
library(timeSeries)
library(astsa)
library(forecast)

# ----- Compute naïve forecasts ----

full_set$naive_forecast <- c(NA, head(full_set$Northwest,-1))
full_set$seasonal_forecast_7d <-
  c(rep(NA, 7), head(full_set$Northwest,-7))
full_set$seasonal_forecast_30d <-
  c(rep(NA, 30), head(full_set$Northwest,-30))

# We offset the rolling mean forecast by an additional period,
# since the rollmean function outputs the forecast for t+1 at position t,
# and we want the forecast for t at position t.
rollmean_forecast <-
  zoo::rollmean(full_set$Northwest,
                k = 3,
                fill = NA,
                align = "right")
full_set$rollmean_forecast <- c(NA, head(rollmean_forecast, -1))



# ----- Item 5 - Define training, validation and test sets ----

# Define the cutoff dates for splitting the data
training_end <- as.Date("2015-12-31")
validation_end <- as.Date("2017-12-31")

# Subset the data into training, validation, and test sets
training_set <- subset(full_set, DT_MST <= training_end)
test_set <- subset(full_set, DT_MST > validation_end)


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
full_set$holt_low_80 <- NA
full_set$holt_high_80 <- NA
full_set$holt_low_95 <- NA
full_set$holt_high_95 <- NA


# Make prediction for each t with data until t-1
for (t in 3:nrow(full_set)) {
  holt_model <-
    holt(full_set[1:(t - 1), "Northwest"],
         h = 1,
         alpha = optimal_alpha,
         beta = optimal_beta)
  
  full_set$holt_forecast[t] <- holt_model$mean[1]
  full_set$holt_low_80[t] <- holt_model$lower[1]
  full_set$holt_high_80[t] <- holt_model$upper[1]
  full_set$holt_low_95[t] <- holt_model$lower[2]
  full_set$holt_high_95[t] <- holt_model$upper[2]
}

# Holt-Winters 7-day seasonality
optimal_model <-
  hw(ts(training_set$Northwest, frequency = 7), h = 1)$model

full_set$hw_forecast <- NA # Initialize forecast column
full_set$hw_high_80 <- NA
full_set$hw_low_80 <- NA
full_set$hw_high_95 <- NA
full_set$hw_low_95 <- NA


# Make prediction for each t with data until t-1
for (t in 11:nrow(full_set)) {
  hw_model <-
    hw(ts(full_set[1:(t - 1), "Northwest"], frequency = 7),
       h = 1,
       model = optimal_model)
  
  full_set$hw_forecast[t] <- hw_model$mean[1]
  full_set$hw_high_80[t] <- hw_model$upper[1]
  full_set$hw_low_80[t] <- hw_model$lower[1]
  full_set$hw_high_95[t] <- hw_model$upper[2]
  full_set$hw_low_95[t] <- hw_model$lower[2]
}


# TBATS
optimal_model <- tbats(ts(training_set$Northwest, frequency = 7))

full_set$tbats_forecast <- NA
full_set$tbats_low_80 <- NA
full_set$tbats_high_80 <- NA
full_set$tbats_low_95 <- NA
full_set$tbats_high_95 <- NA

for (t in 1:nrow(full_set)) {
  tbats_model <-
    tbats(ts(full_set[1:(t - 1), "Northwest"], frequency = 7), model = optimal_model)
  
  full_set$tbats_forecast[t] <-
    forecast(tbats_model, h = 1)$mean
  full_set$tbats_low_80[t] <- forecast(tbats_model, h = 1)$lower[1]
  full_set$tbats_high_80[t] <- forecast(tbats_model, h = 1)$upper[1]
  full_set$tbats_low_95[t] <- forecast(tbats_model, h = 1)$lower[2]
  full_set$tbats_high_95[t] <- forecast(tbats_model, h = 1)$upper[2]
}

plot(forecast(optimal_model)$mean)

# ----- Item 6 - Collect seasonal accuracy measures for forecasts ----

mape <- function(forecast, observed) {
  return (mean(abs((
    observed - forecast
  ) / observed)) * 100)
}

pct_bias <- function(forecast, observed) {
  return (mean((forecast - observed) / observed) * 100)
}

pct_interval_coverage <- function(low_bound, high_bound, observed) {
  return (mean(ifelse(
    low_bound <= observed & observed <= high_bound, 1, 0
  )))
}

validation_set <-
  subset(full_set, DT_MST > training_end & DT_MST <= validation_end)

accuracy_measures <- list()

for (s in unique(full_set$season)) {
  seasonal_subset <- subset(validation_set, season == s)
  
  accuracy_measures[["naive"]][[s]] <-
    mape(seasonal_subset$naive_forecast, seasonal_subset$Northwest)
  
  accuracy_measures[["7d naive"]][[s]] <-
    mape(seasonal_subset$seasonal_forecast_7d,
         seasonal_subset$Northwest)
  
  accuracy_measures[["30d naive"]][[s]] <-
    mape(seasonal_subset$seasonal_forecast_30d,
         seasonal_subset$Northwest)
  
  accuracy_measures[["rollmean"]][[s]] <-
    mape(seasonal_subset$rollmean_forecast,
         seasonal_subset$Northwest)
  
  accuracy_measures[["simple exponential smoothing"]][[s]] <-
    mape(seasonal_subset$ses_forecast, seasonal_subset$Northwest)
  
  accuracy_measures[["holt"]][[s]] <- list(
    mape = mape(seasonal_subset$holt_forecast, seasonal_subset$Northwest),
    coverage_80 = pct_interval_coverage(
      seasonal_subset$holt_low_80,
      seasonal_subset$holt_high_80,
      seasonal_subset$Northwest
    ),
    coverage_95 = pct_interval_coverage(
      seasonal_subset$holt_low_95,
      seasonal_subset$holt_high_95,
      seasonal_subset$Northwest
    )
  )
  
  accuracy_measures[["holt-winters"]][[s]] <- list(
    mape = mape(seasonal_subset$hw_forecast, seasonal_subset$Northwest),
    coverage_80 = pct_interval_coverage(
      seasonal_subset$hw_low_80,
      seasonal_subset$hw_high_80,
      seasonal_subset$Northwest
    ),
    coverage_95 = pct_interval_coverage(
      seasonal_subset$hw_low_95,
      seasonal_subset$hw_high_95,
      seasonal_subset$Northwest
    )
  )
  
  accuracy_measures[["TBATS"]][[s]] <- list(
    mape = mape(seasonal_subset$tbats_forecast, seasonal_subset$Northwest),
    coverage_80 = pct_interval_coverage(
      seasonal_subset$tbats_low_80,
      seasonal_subset$tbats_high_80,
      seasonal_subset$Northwest
    ),
    coverage_95 = pct_interval_coverage(
      seasonal_subset$tbats_low_95,
      seasonal_subset$tbats_high_95,
      seasonal_subset$Northwest
    )
  )
}


# ----- Output results to file ----

# Open a connection to a new file for writing
con <- file("output.txt", open = "wt")
# Redirect print output to the file
sink(con)

print(accuracy_measures)
close(con)
