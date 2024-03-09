Sys.setlocale("LC_TIME", "C")
library(timeSeries)
library(astsa)
library(forecast)


# -----Load and parse data-----

load("aeso.RData")

aeso <- timeSeries(aeso, aeso$DT_MST, format = "%Y-%m-%d")
aeso <-
  aeso[, c("DT_MST", "Northwest")] # Only keep required columns
aeso <-
  aeso[!is.na(aeso$Northwest),] # Remove entries for which hourly consumption is missing

# ---- Correct for Missing Values ----
NAtoNeighborAverage <- function(x) {
  n <- length(x)
  if (n <= 2) return("not enough data to interpolate") 
  
  for (i in 2:(n-1)) {
    if (is.na(x[i])) {
      prev_val <- x[i-1]
      next_val <- x[i+1]
      
      # If both neighbors are not NaN, calculate the average; otherwise, use the non-NaN neighbor.
      if (!is.na(prev_val) && !is.na(next_val)) {
        x[i] <- mean(c(prev_val, next_val))
      } else if (!is.na(prev_val)) {
        x[i] <- prev_val
      } else if (!is.na(next_val)) {
        x[i] <- next_val
      }
      # Note: If both neighbors are NaN, this doesn't change the current NaN value.
    }
  }
  
  # Handle first and last elements if they are NaN, by simple forward or backward fill
  if (is.na(x[1])) x[1] <- x[min(which(!is.na(x)))] # Forward fill
  if (is.na(x[n])) x[n] <- x[max(which(!is.na(x)))] # Backward fill
  
  return("Interpolated")
}



# -----  Aggregate daily peak load ----

# Get daily peak hourly load
everyDay <-
  timeSequence(
    from = min(aeso$DT_MST),
    to = max(aeso$DT_MST),
    by = "day"
  )
full_set <- aggregate(aeso, everyDay, max)

# Convert columns to appropriate types
full_set <-
  transform(full_set,
            DT_MST = as.Date(DT_MST),
            Northwest = as.numeric(Northwest))


# ---- Add Dummy Variables ----

holiday_dates <- read.csv("holidays.csv", colClasses=c("Date"))$Date
full_set$IsHoliday <- ifelse(full_set$DT_MST %in% holiday_dates, 1, 0)

full_set$Weekday <- weekdays(full_set$DT_MST)
full_set$IsWeekend <- ifelse(full_set$Weekday %in% c("Saturday", "Sunday"), 1, 0)

full_set$season <-
  cut(
    as.POSIXlt(full_set$DT_MST)$mon,
    breaks = c(0, 2, 5, 8, 12),
    labels = c("Winter", "Spring", "Summer", "Autumn"),
    right = FALSE
  )


# ----- Compute naïve forecasts ----

full_set$naive_forecast <- c(NA, head(full_set$Northwest, -1))
full_set$seasonal_forecast_7d <-
  c(rep(NA, 7), head(full_set$Northwest, -7))
full_set$seasonal_forecast_30d <-
  c(rep(NA, 30), head(full_set$Northwest, -30))

# We offset the rolling mean forecast by an additional period,
# since the rollmean function outputs the forecast for t+1 at position t,
# and we want the forecast for t at position t.
rollmean_forecast <-
  zoo::rollmean(full_set$Northwest,
                k = 3,
                fill = NA,
                align = "right")
full_set$rollmean_forecast <- c(NA, head(rollmean_forecast,-1))



# ----- Define training, validation and test sets ----

# Define the cutoff dates for splitting the data
training_end <- as.Date("2015-12-31")
validation_end <- as.Date("2017-12-31")

# Subset the data into training, validation, and test sets
training_set <- subset(full_set, DT_MST <= training_end)
test_set <- subset(full_set, DT_MST > validation_end)


# ----- Collect accuracy measures for forecasts ----

mape <- function(forecast, observed) {
  return (mean(abs((
    observed - forecast
  ) / observed)) * 100)
}
evaluate <- function(dataset, forecast_col_name, observed_col_name="Northwest") {
  return(mape(dataset[, forecast_col_name], dataset[, observed_col_name]))
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
  
  accuracy_measures[["holt"]][[s]] <-
    mape(seasonal_subset$holt_forecast, seasonal_subset$Northwest)
  
  accuracy_measures[["holt-winters"]][[s]] <-
    mape(seasonal_subset$ses_forecast, seasonal_subset$Northwest)
}


# ----- Output results to file ----

# Open a connection to a new file for writing
con <- file("output.txt", open = "wt")
# Redirect print output to the file
sink(con)

print(accuracy_measures)
close(con)
