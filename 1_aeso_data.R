
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

###
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

timezone <- "America/Edmonton"
# Convert the DT_MST column to POSIXct with the correct timezone
aeso.nw$DT_MST <- as.POSIXct(aeso.nw$DT_MST, tz = timezone, format = "%Y-%m-%d %H:%M:%S")

# Define arrays for the start and end dates of each outlier period
outlier_start_dates <- as.POSIXct(c("2011-05-15 00:00:00", "2014-01-14 00:00:00", 
                                    "2015-05-04 22:00:00", "2015-05-30 12:00:00", 
                                    "2015-08-08 00:00:00", "2015-09-12 00:00:00", 
                                    "2015-09-29 00:00:00", "2018-10-17 00:00:00", 
                                    "2019-02-03 12:00:00", "2019-05-11 12:00:00"), tz = timezone)
outlier_end_dates <- as.POSIXct(c("2011-05-22 00:00:00", "2014-01-21 00:00:00", 
                                  "2015-05-08 00:00:00", "2015-05-31 00:00:00", 
                                  "2015-08-14 00:00:00", "2015-09-14 23:59:00", 
                                  "2015-10-03 00:00:00", "2018-10-18 00:00:00", 
                                  "2019-02-10 12:00:00", "2019-05-18 00:00:00"), tz = timezone)

# Loop through each set of start and end dates
for (i in seq_along(outlier_start_dates)) {
  # Create a sequence of dates at hourly intervals between the start and end dates for each period
  outlier_dates <- seq(from = outlier_start_dates[i], to = outlier_end_dates[i], by = "hour")
  
  # Loop through the range of outlier dates and replace the values
  for (date in outlier_dates) {
    index_one_week_prior <- find_value_one_week_prior(date, aeso.nw)
    if (!is.na(index_one_week_prior)) { # Check if index is not NA
      # Replace the value at the outlier index with the value from one week prior
      aeso.nw$Northwest[which(aeso.nw$DT_MST == date)] <- aeso.nw$Northwest[index_one_week_prior]
    } else {
      warning(paste("No matching date one week prior for", date))
    }
  }
}


################3

# Plot the data
plot(aeso.nw$DT_MST, aeso.nw$Northwest,
     type = "l",
     main = "Northwest Values After Replacing Outliers",
     xlab = "Date", ylab = "Northwest"
)

# Aggregate to find the daily maximum for the 'Northwest' measurement
aeso.nw$Date <- as.Date(aeso.nw$DT_MST)
daily.max <- aggregate(Northwest ~ Date, data = aeso.nw, max)
daily.max <- subset(daily.max, Date <= as.Date("2019-12-31"))
head(daily.max)


# Now, create the 'full_set' data frame
full_set <- data.frame(
  DT_MST = as.Date(daily.max$Date),
  Northwest = as.numeric(daily.max$Northwest)
)

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
#par(mfrow = c(3, 2))
#boxplot(Northwest ~ SeasonHoliday,
#  data = full_set,
#  xlab = "Holiday - season",
#  ylab = "Load in MW",
#  main = "Load in MW during Holidays by Month",
#  las = 2,
#  cex.axis = 0.8
#)

full_set$Weekday <- weekdays(full_set$DT_MST)
full_set$IsWeekend <-
  ifelse(full_set$Weekday %in% c("Saturday", "Sunday"), 1, 0)


full_set$Year <- as.POSIXlt(full_set$DT_MST)$year + 1900

# List of columns for which to aggregate the data
regions <- c("South", "Northwest", "Northeast", "Edmonton", "Calgary", "Central")


# To calculate daily max with no outlier correction for

aeso <- aeso[!is.na(aeso), ]
aeso$Date <- as.Date(aeso$DT_MST)
daily_max_df <- data.frame(Date = unique(aeso$Date))

# List of regions
regions <- c("South", "Northwest", "Northeast", "Edmonton", "Calgary", "Central")

# Aggregate daily max for each region
for (region in regions) {
  # Use aggregate to find daily max for the current region
  daily_max_for_region <- aggregate(. ~ Date, data = aeso[c("Date", region)], FUN = max)

  # Merge with daily_max_df
  daily_max_df <- merge(daily_max_df, daily_max_for_region, by = "Date")
}

daily_max_df$Total <- rowSums(daily_max_df[regions], na.rm = TRUE)
full_set$Total <- daily_max_df$Total

