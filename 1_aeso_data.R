library(timeSeries)
library(astsa)
library(forecast)

# -----Load and parse data-----

load("./data/aeso.RData")
aeso <-  aeso[!is.na(aeso$Northwest),]
dates <- as.Date(aeso$DT_MST)

aeso.nw <- ts(aeso$Northwest, start = c(2011, 1), frequency = 8760)
                      # hourly data for every year since 2011
# ----- Correct for Missing Values ----

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





# ----- Correct for outliers ----

original_data <- aeso.nw

# Detecting outliers
outliers_indices <- which(abs(aeso.nw - mean(aeso.nw, na.rm = TRUE)) 
                          > 3*sd(aeso.nw, na.rm = TRUE))

# Replacing outliers with median
aeso.nw[outliers_indices] <- median(aeso.nw, na.rm = TRUE)

# Constants
season_length <- 2160
num_seasons <- length(aeso.nw) / season_length

# Create a copy of the original data to modify
nol_aeso.nw <- aeso.nw

# Loop through each season
for (i in 1:num_seasons) {
  # Define the start and end indices of the current season
  start_idx <- (i - 1) * season_length + 1
  end_idx <- min(i * season_length, length(aeso.nw))
  
  # Extract the current season's data
  season_data <- aeso.nw[start_idx:end_idx]
  
  # Compute median and SD, ignoring NAs
  season_median <- median(season_data, na.rm = TRUE)
  season_sd <- sd(season_data, na.rm = TRUE)
  
  # Detect outliers: more than 3 SDs from the median
  outliers_indices <- which(abs(season_data - season_median) > 3.5 * season_sd)
  
  # Replace outliers with the season's median
  season_data[outliers_indices] <- season_median
  
  # Update the time series with the cleaned season data
  nol_aeso.nw[start_idx:end_idx] <- season_data
}
par(mfrow = c(2, 1))

length(nol_aeso.nw)

# -----  Aggregate daily peak load ----
ts_data <- as.ts(nol_aeso.nw, start = c(2011, 1), frequency = 24)


if (is.ts(ts_data)) {
  # Convert to a more generic format for the date conversion
  start_year <- start(ts_data)[1]
  start_month <- start(ts_data)[2]
  frequency_per_year <- frequency(ts_data)
  # For hourly data,
  date_values <- seq(from = as.POSIXct(paste(start_year, 
                                             start_month, 
                                             "01", "00", "00", 
                                             sep="-")), 
                     length.out = length(ts_data), 
                     by = "hour")
  date_values <- as.Date(date_values)  # Convert POSIXct to Date for daily aggregation
}

# Create a data frame for manipulation 
data_df <- data.frame(date = date_values, value = as.vector(ts_data))  # Convert ts_data to vector

data_df$year <- format(data_df$date, "%Y")
data_df$day_of_year <- as.integer(format(data_df$date, "%j"))
length(data_df$day_of_year)
# Aggregate to find the daily max for each year and day
daily <- aggregate(value ~ year + day_of_year, data = data_df, FUN = max)
daily$date <- as.Date(paste0(daily$year, "-", daily$day_of_year), format="%Y-%j")
daily_max <- daily[, c("date", "value")]
daily_max$date <- as.Date(daily_max$date)
daily_max <- daily_max[order(daily_max$date), ]
daily_max
# convert it back to a timeSeries object:
daily_max_ts <- timeSeries(data=daily_max$value, time=daily_max$date)
# Create a full set object to collect all variables
start_date <- as.Date("2011-01-01")  # Adjust based on your actual data start date
end_date <- start_date + length(daily_max) - 1  # Ensuring alignment with 'daily_max' length
full_dates <- seq.Date(min(daily_max$date),
                       max(daily_max$date),
                       by = "day")

# Now, create the 'full_set' data frame
full_set <- data.frame(DT_MST = full_dates)

full_set$DT_MST <- as.Date(daily_max$date)
full_set$Northwest <- as.numeric(daily_max_ts)

head(full_set)


# ---- Add Dummy Variables ----

# Item 6 - Identify all holidays and assess their impact on daily peak hourly load
holiday_dates <-
  read.csv("./data/holidays.csv", colClasses = c("Date"))$Date
full_set$IsHoliday <-
  ifelse(full_set$DT_MST %in% holiday_dates, 1, 0)

# Defining Seasons
full_set$season <-
  cut(
    as.POSIXlt(full_set$DT_MST)$mon,
    breaks = c(0, 2, 5, 8, 11, 12),
    labels = c("Winter", "Spring", "Summer", "Autumn", "Winter"),
    right = FALSE
  )

# Create an interaction term between Month and IsHoliday
full_set$SeasonHoliday <- 
  interaction(full_set$IsHoliday, full_set$season, drop = TRUE, sep = "-")

# Create the boxplot
par(mfrow = c(3, 2))  
boxplot(Northwest ~ SeasonHoliday, data = full_set,
        xlab = "Holiday - season",
        ylab = "Load in MW",
        main = "Load in MW during Holidays by Month",
        las = 2,
        cex.axis = 0.8)  

full_set$Weekday <- weekdays(full_set$DT_MST)
full_set$IsWeekend <-
  ifelse(full_set$Weekday %in% c("Saturday", "Sunday"), 1, 0)


full_set$Year <- as.POSIXlt(full_set$DT_MST)$year + 1900

# List of columns for which to aggregate the data
regions = c("South", "Northwest", "Northeast", "Edmonton", "Calgary", "Central")


# To calculate daily max with no outlier correction for

aeso <-  aeso[!is.na(aeso),] 
aeso$Date <- as.Date(aeso$DT_MST)
daily_max_df <- data.frame(Date = unique(aeso$Date))

# List of regions
regions = c("South", "Northwest", "Northeast", "Edmonton", "Calgary", "Central")

# Aggregate daily max for each region
for(region in regions) {
  # Use aggregate to find daily max for the current region
  daily_max_for_region <- aggregate(. ~ Date, data = aeso[c("Date", region)], FUN = max)
  
  # Merge with daily_max_df
  daily_max_df <- merge(daily_max_df, daily_max_for_region, by = "Date")
}

daily_max_df$Total <- rowSums(daily_max_df[regions], na.rm = TRUE)
full_set$Total <- daily_max_df$Total
