# ***************************** #
#    Forecasting Electricity    #
#    Northwest Alberta.         #
# --- Preprocessing Script      #
# Outlier treatment             #
# Temperature & Weather Data    #
# Noise for Regresors on Temp.  #
# ---------------------------   #
#      Jonathan Gonzalez.       #
#        April 4 2024.          #
# ***************************** #

## --- Functions -----

fill_NAs_with_nearest_averages <- function(data) {
  n <- length(data)
  for (i in 1:n) {
    if (is.na(data[i])) {
      # Find the nearest non-NA values before and after the NA
      before <- data[1:i]
      after <- data[i:n]

      nearest_before <- tail(before[!is.na(before)], 1)
      nearest_after <- head(after[!is.na(after)], 1)

      # If both neighbors are NA, this will remain NA
      if (length(nearest_before) == 0 | length(nearest_after) == 0) next

      # Calculate the average of the nearest non-NA neighbors
      data[i] <- mean(c(nearest_before, nearest_after), na.rm = TRUE)
    }
  }
  return(data)
}

## Libraries and Config ##########

# Locale to ENG
Sys.setlocale("LC_TIME", "C")
# Load Libraries
library(timeSeries)
library(astsa)
library(forecast)

#--- Load Data and Treat Outliers ----
# Load data
load("./data/aeso.RData")

# Extract just the northwest
aeso.nw <- aeso[, c("DT_MST", "Northwest")]
summary(aeso.nw)
which(is.na(aeso.nw))
# Drop the NA Dates
aeso.nw <- aeso.nw[!is.na(aeso.nw$Northwest), ]
which(is.na(aeso.nw))
# Convert to TS
aeso.TS <- timeSeries(aeso.nw, aeso.nw$DT_MST, format = "%Y-%m-%d")
aeso.TS <- aeso.TS[, c("DT_MST", "Northwest")]
# Get a daily max TS
daily <-
  timeSequence(
    from = min(aeso.TS$DT_MST),
    to = max(aeso.TS$DT_MST),
    by = "day"
  )
daily.max <- aggregate(aeso.TS, daily, max)
daily.max <-
  transform(daily.max,
    DT_MST = as.Date(DT_MST),
    Northwest = as.numeric(Northwest)
  )
daily.max <-
  timeSeries(daily.max$Northwest, daily.max$DT_MST, format = "%Y-%m-%d")
colnames(daily.max) <- c("load")
summary(daily.max)


head(daily.max)


########### Delete this; test to find outlier dates#######
# Load the necessary library
library(lubridate)

# Assuming 'aeso.nw' is your dataset and it has a column 'DT_MST' with date-time strings and 'Northwest' with values to plot

# Step 1: Filter for January 2014
# This line creates a new dataframe 'year_interest' that contains only data from January 2014
year_interest <- aeso.nw[month(as.Date(aeso.nw$DT_MST)) == 5 & year(as.Date(aeso.nw$DT_MST)) == "2011", ]

# Step 2: Convert 'DT_MST' to POSIXct
# Ensure the date-time format matches the format in your dataset
year_interest$DT_MST <- as.POSIXct(year_interest$DT_MST, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Step 3: Plot
# Set up the plot parameters (optional, depending on your specific needs)
par(mfrow = c(1, 1))

# Create the plot
plot(year_interest$DT_MST, year_interest$Northwest,
  type = "l", xlab = "Date", ylab = "Northwest",
  main = "Northwest Values over January 2011", xaxt = "n"
) # Suppress x-axis

# Add daily ticks on the x-axis
axis.Date(1,
  at = seq(min(year_interest$DT_MST), max(year_interest$DT_MST), by = "day"),
  format = "%b %d", cex.axis = 0.7
)

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

# Plotting with ggplot2
ggplot(year_interest, aes(x = DT_MST, y = Northwest)) +
  geom_line() +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 day") + # Format and break by day
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate labels for readability
  labs(x = "Date", y = "Northwest", title = "Northwest Values over January 2011")


# Define your outlier threshold. For example, if you decide that any value below 900 is an outlier:
outlier_threshold <- 1000

# Find the rows where the 'Northwest' value is below this threshold
outliers <- year_interest[year_interest$Northwest < outlier_threshold, ]

# Look at the dates corresponding to these outlier values
outlier_dates <- outliers$DT_MST

# Print the outlier dates
print(outlier_dates)

head(year_interest)
########### Delete this; test to find outlier dates#######

# Set the correct timezone for your data
timezone <- "America/Edmonton"

# Convert the DT_MST column to POSIXct with the correct timezone
aeso.nw$DT_MST <- as.POSIXct(aeso.nw$DT_MST, tz = timezone)

# Define the range of outliers with the correct timezone
outlier_start <- as.POSIXct("2011-01-14 00:00:00", tz = timezone)
outlier_end <- as.POSIXct("2011-01-21 00:00:00", tz = timezone)

# Create a sequence of dates at hourly intervals between the start and end dates
outlier_dates <- seq(from = outlier_start, to = outlier_end, by = "hour")

# Function to find the value from one week prior
find_value_one_week_prior <- function(date, data) {
  # Subtract 7 days from the given date
  target_date <- date - 7 * 24 * 60 * 60 # One week prior

  # Find the Northwest value for the target_date
  index <- which(data$DT_MST == target_date)

  # If an index is found, return it, otherwise return NA
  if (length(index) > 0) {
    return(index)
  } else {
    return(NA) # Return NA if no matching date is found
  }
}

# Loop through the range of outlier dates and replace the values
for (date in outlier_dates) {
  index_one_week_prior <- find_value_one_week_prior(date, aeso.nw)
  if (!is.na(index_one_week_prior)) {
    # Replace the value at the outlier index with the value from one week prior
    aeso.nw$Northwest[which(aeso.nw$DT_MST == date)] <- aeso.nw$Northwest[index_one_week_prior]
  } else {
    warning(paste("No matching date one week prior for", date))
  }
}


# Plot the data
plot(aeso.nw$DT_MST, aeso.nw$Northwest,
  type = "l",
  main = "Northwest Values After Replacing Outliers",
  xlab = "Date", ylab = "Northwest"
)




#---- Temperature and Weather Data -----

# Temperature Data extracted from https://acis.alberta.ca/acis/township-data-viewer.jsp
# Took the 9 main townships that AESO designates as the Northwestern area of Alberta




temperature <- read.csv("./data/NW-AB_Temp.csv", sep = ",", header = TRUE)

# Rename Columns for easy accesibility
new_column_names <- c(
  "township", "date", "avg_temp", "min_temp", "max_temp",
  "precip", "precip_acc", "humidity_avg", "snow-water_eq",
  "solar_rad", "wind_speed"
)

colnames(temperature) <- new_column_names
head(temperature)

# Extract Desired Columns
meteo_data <- temperature[c(
  "township", "date", "avg_temp", "min_temp", "max_temp",
  "humidity_avg", "wind_speed"
)]
head(meteo_data)
length(meteo_data$date)
# Fill NA on temperature data


meteo_data$avg_temp <- fill_NAs_with_nearest_averages(meteo_data$avg_temp)
meteo_data$min_temp <- fill_NAs_with_nearest_averages(meteo_data$min_temp)
meteo_data$max_temp <- fill_NAs_with_nearest_averages(meteo_data$max_temp)
meteo_data$humidity_avg <- fill_NAs_with_nearest_averages(meteo_data$humidity_avg)
meteo_data$wind_speed <- fill_NAs_with_nearest_averages(meteo_data$wind_speed)

# Generate noise
head(meteo_data)
length(meteo_data$date)

# Ensure the 'date' column is in the correct Date format
meteo_data$date <- as.Date(meteo_data$date)

# Get the temp range and calculate the noise
temp_range <- max(meteo_data$max_temp) - min(meteo_data$min_temp)
avg_error <- (0.02 * temp_range)^2
noise <- rnorm(length(meteo_data$avg_temp), 0, avg_error)
meteo_data$noisy_temp <- meteo_data$avg_temp + noise

meteo_data <- subset(meteo_data, select = -c(township))
meteo_data <- aggregate(. ~ date, data = meteo_data, FUN = mean)
head(meteo_data)
length(meteo_data$date)

# Plot avg_temp
plot(meteo_data$date, meteo_data$noisy_temp,
  type = "l", col = "red",
  ylim = range(c(meteo_data$avg_temp, meteo_data$noisy_temp)),
  xlab = "Observation", ylab = "Temperature",
  main = "Average vs. Noisy Temperatures \n with same half the std of avg temp"
)
# Add noisy_temp to the same plot
lines(meteo_data$date, meteo_data$avg_temp, type = "l", col = "blue")

# Add a legend
legend("topright",
  legend = c("Avg Temp", "Noisy Temp"),
  col = c("blue", "red"), lty = 1
)

# Temperature yearly
meteo_data_year <- meteo_data[format(meteo_data$date, "%Y") == "2013", ]

# Plotting
plot(meteo_data_year$date, meteo_data_year$avg_temp,
  type = "l", col = "blue",
  ylim = range(c(meteo_data_year$avg_temp, meteo_data_year$noisy_temp)),
  xlab = "Date", ylab = "Temperature",
  main = "Average vs. Noisy Temperatures for 2023"
)
lines(meteo_data_year$date, meteo_data_year$noisy_temp, type = "l", col = "red")

# Adding a legend
legend("topright",
  legend = c("Avg Temp", "Noisy Temp"),
  col = c("blue", "red"), lty = 1
)


## Noise for other Variables



## ---- CDD-HDD -----

head(meteo_data)

T_ref <- 10.00
T_t <- meteo_data$avg_temp

temp.cdd <- max(T_t - T_ref, 0)
temp.hdd <- max(T_ref - T_t, 0)


# Calculate Heating Degree Days (HDD) and Cooling Degree Days (CDD)
meteo_data$HDD <- pmax(T_ref - T_t, 0)
meteo_data$CDD <- pmax(T_t - T_ref, 0)

# Print the updated data frame
print(meteo_data)

plot(
  meteo_data$date,
  meteo_data$HDD,
  type = "l",
  col = "red"
)
lines(meteo_data$date,
  meteo_data$CDD,
  col = "blue"
)


head(meteo_data)
mergedData <- merge(daily.max, meteo_data, by = "date")
cleanData <- na.omit(mergedData)
head(mergedData)
meteo_data <- subset(meteo_data, date <= as.Date("2019-12-31"))
meteo_data$load <- daily.max$load
# Calculate correlation
correlationMatrix <-
  cor(meteo_data[, c("load", "HDD", "CDD")], use = "complete.obs")

# Print the correlation matrix
print(correlationMatrix)

# Create lag-1 for the HDD column
meteo_data$lag_HDD <-
  c(NA, meteo_data$HDD[-nrow(meteo_data)])

# Create lag-1 for the CDD column
meteo_data$lag_CDD <-
  c(NA, meteo_data$CDD[-nrow(meteo_data)])

# Create lag-2 for the HDD column
meteo_data$lag2_HDD <- c(NA, NA, meteo_data$HDD[-(nrow(meteo_data)):-(nrow(meteo_data) - 1)])

# Create lag-2 for the CDD column
meteo_data$lag2_CDD <- c(NA, NA, meteo_data$CDD[-(nrow(meteo_data)):-(nrow(meteo_data) - 1)])


# Convert your data frame columns to time series if they aren't already
HDD_ts <- ts(meteo_data$HDD)
CDD_ts <- ts(meteo_data$CDD)

# lag-2 for CDD
lag2_HDD_ts <- c(NA, NA, HDD_ts[-(length(HDD_ts) - 1):-length(HDD_ts)])
lag2_CDD_ts <- c(NA, NA, CDD_ts[-(length(CDD_ts) - 1):-length(CDD_ts)])

par(mfrow = c(2, 3))
plot(meteo_data$HDD,
  meteo_data$load,
  main = "Electricity Load vs HDD",
  xlab = "Heating Degree Days (HDD)",
  ylab = "Electricity Load",
  col = "red",
  pch = 1
)

# Plot for Electricity Load vs HDD
plot(meteo_data$lag_HDD,
  meteo_data$load,
  main = "Electricity Load vs HDD lag 1",
  xlab = "Heating Degree Days (lag-1 HDD)",
  ylab = "Electricity Load",
  col = "red",
  pch = 1
)
# Plot for Electricity Load vs HDD
plot(lag2_HDD_ts,
  meteo_data$load,
  main = "Electricity Load vs HDD lag 2",
  xlab = "Heating Degree Days (lag-2 HDD)",
  ylab = "Electricity Load",
  col = "red",
  pch = 1
)

# Plot for Electricity Load vs CDD
plot(meteo_data$CDD,
  meteo_data$load,
  main = "Electricity Load vs CDD",
  xlab = "Cooling Degree Days (CDD)",
  ylab = "Electricity Load",
  col = "blue",
  pch = 1
)


# Plot for Electricity Load vs CDD
plot(meteo_data$lag_CDD,
  meteo_data$load,
  main = "Electricity Load vs CDD lag 1",
  xlab = "Cooling Degree Days (lag-1 CDD)",
  ylab = "Electricity Load",
  col = "blue",
  pch = 1
)


# Plot for Electricity Load vs CDD
plot(lag2_CDD_ts,
  meteo_data$load,
  main = "Electricity Load vs CDD lag 2",
  xlab = "Cooling Degree Days (lag-2 CDD)",
  ylab = "Electricity Load",
  col = "blue",
  pch = 1
)


summary(meteo_data)
