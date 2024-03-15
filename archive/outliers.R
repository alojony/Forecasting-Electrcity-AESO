# Locale to ENG
Sys.setlocale("LC_TIME", "C")
# Load Libraries
library(timeSeries)
library(astsa)
library(forecast)

ts_data <- ts(aeso.nw$Northwest, start = c(2011, 1), frequency = 8760)
original_data <- ts_data

# Detecting outliers
outliers_indices <- which(abs(ts_data - mean(ts_data, na.rm = TRUE)) > 3*sd(ts_data, na.rm = TRUE))

# Replacing outliers with median
ts_data[outliers_indices] <- median(ts_data, na.rm = TRUE)

# Constants
season_length <- 2160
num_seasons <- length(ts_data) / season_length

# Create a copy of the original data to modify
seasonal_ts_data <- ts_data

# Loop through each season
for (i in 1:num_seasons) {
  # Define the start and end indices of the current season
  start_idx <- (i - 1) * season_length + 1
  end_idx <- min(i * season_length, length(ts_data))
  
  # Extract the current season's data
  season_data <- ts_data[start_idx:end_idx]
  
  # Compute median and SD, ignoring NAs
  season_median <- median(season_data, na.rm = TRUE)
  season_sd <- sd(season_data, na.rm = TRUE)
  
  # Detect outliers: more than 3 SDs from the median
  outliers_indices <- which(abs(season_data - season_median) > 3.5 * season_sd)
  
  # Replace outliers with the season's median
  season_data[outliers_indices] <- season_median
  
  # Update the time series with the cleaned season data
  seasonal_ts_data[start_idx:end_idx] <- season_data
}

par(mfrow = c(2, 1))
# Replot the cleaned time series
plot(original_data, xlab="Time", ylab="Value", main="Cleaned Northwest Time Series after Removing Outliers")
plot(seasonal_ts_data ,pch=4)

full_set$no_outliers <- seasonal_ts_data



