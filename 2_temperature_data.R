# Locale to ENG
Sys.setlocale("LC_TIME", "C")
# Load Libraries
library(timeSeries)
library(astsa)
library(forecast)
# Temperature Data extracted from https://acis.alberta.ca/acis/township-data-viewer.jsp
# Took the 9 main townships that AESO designates as the Northwestern area of Alberta


#--- CDD-HDD -----

temperature <-
  read.csv(
    "./data/NW-AB_Temp__old.csv",
    sep = ",",
    header = TRUE
  )
summary(temperature)


townships <- unique(temperature$Township)
# Loop through each township
for (township in townships) {
  # Subset the data for the current township
  subset_data <- temperature[temperature$Township == township, ]
  
  # Interpolate missing values for Temp.Min
  temperature_min_ts <- subset_data$Temp.Min
  missing_min_values <- is.na(temperature_min_ts)
  temperature_min_ts[missing_min_values] <-
    approx(
      x = which(!missing_min_values),
      y = temperature_min_ts[!missing_min_values],
      xout = which(missing_min_values)
    )$y
  
  # Interpolate missing values for Temp.Max
  temperature_max_ts <- subset_data$Temp.Max
  missing_max_values <- is.na(temperature_max_ts)
  temperature_max_ts[missing_max_values] <-
    approx(
      x = which(!missing_max_values),
      y = temperature_max_ts[!missing_max_values],
      xout = which(missing_max_values)
    )$y
  
  # Replace the interpolated values in the original data
  temperature[temperature$Township == township, "Temp.Min"] <-
    temperature_min_ts
  temperature[temperature$Township == township, "Temp.Max"] <-
    temperature_max_ts
}

# Check for NA
summary(temperature)

# Convert Date column to Date format
temperature$Date <- as.Date(temperature$Date)

# Calculate the average temperature for each day
temperature.daily_avg <-
  aggregate(
    cbind(Temp.Min, Temp.Max) ~ Date,
    data = temperature,
    FUN = mean,
    na.rm = TRUE
  )

# Rename the columns for the new dataset
colnames(temperature.daily_avg) <- c("Date", "Temp.Min", "Temp.Max")

# Calculate the average temperature for each date
temperature.daily_avg$Temp.Avg <-
  (temperature.daily_avg$Temp.Min + temperature.daily_avg$Temp.Max) / 2
# Display the updated data frame to verify the new column

summary(temperature)
temperature <- na.omit(temperature)

temperature$Date <- as.Date(temperature$Date)

# Calculate the average temperature for each day
temperature.daily_avg <-
  aggregate(
    cbind(Temp.Min, Temp.Max) ~ Date,
    data = temperature,
    FUN = mean,
    na.rm = TRUE
  )

# Rename the columns for the new dataset
colnames(temperature.daily_avg) <- c("Date", "Temp.Min", "Temp.Max")

# Calculate the average temperature for each date
temperature.daily_avg$Temp.Avg <-
  (temperature.daily_avg$Temp.Min + temperature.daily_avg$Temp.Max) / 2
# Display the updated data frame to verify the new column

head(temperature.daily_avg)

T_ref <- 10.00
T_t <- temperature.daily_avg$Temp.Avg

temp.cdd <- max(T_t - T_ref, 0)
temp.hdd <- max(T_ref - T_t, 0)


# Calculate Heating Degree Days (HDD) and Cooling Degree Days (CDD)
temperature.daily_avg$HDD <- pmax(T_ref - T_t, 0)
temperature.daily_avg$CDD <- pmax(T_t - T_ref, 0)

# Print the updated data frame
print(temperature.daily_avg)

plot(
  temperature.daily_avg$Date,
  temperature.daily_avg$HDD,
  type = 'l',
  col = 'red'
)
lines(temperature.daily_avg$Date,
      temperature.daily_avg$CDD,
      col = 'blue')

load(
  "./data/aeso.RData"
)

# extract just the northwest
aeso.nw <- aeso[, c("DT_MST", "Northwest")]
# does it has na?
summary(aeso.nw)
which(is.na(aeso.nw))
# drop the NA Dates
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
            Northwest = as.numeric(Northwest))
daily.max <-
  timeSeries(daily.max$Northwest, daily.max$DT_MST, format = "%Y-%m-%d")
colnames(daily.max) <- c("load")
summary(daily.max)

head(daily.max)
head(temperature.daily_avg)
mergedData <- merge(daily.max, temperature.daily_avg, by = "date")
cleanData <- na.omit(mergedData)
head(mergedData)

temperature.daily_avg$load <- daily.max$load
# Calculate correlation
correlationMatrix <-
  cor(temperature.daily_avg[, c("load", "HDD", "CDD")], use = "complete.obs")

# Print the correlation matrix
print(correlationMatrix)

lag1.plot(temperature.daily_avg$HDD, max.lag = 2)

# Create lag-1 for the HDD column
temperature.daily_avg$lag_HDD <- 
  c(NA, temperature.daily_avg$HDD[-nrow(temperature.daily_avg)])

# Create lag-1 for the CDD column
temperature.daily_avg$lag_CDD <- 
  c(NA, temperature.daily_avg$CDD[-nrow(temperature.daily_avg)])

# Create lag-2 for the HDD column
temperature.daily_avg$lag2_HDD <- c(NA, NA, temperature.daily_avg$HDD[-(nrow(temperature.daily_avg)):-(nrow(temperature.daily_avg)-1)])

# Create lag-2 for the CDD column
temperature.daily_avg$lag2_CDD <- c(NA, NA, temperature.daily_avg$CDD[-(nrow(temperature.daily_avg)):-(nrow(temperature.daily_avg)-1)])


# Convert your data frame columns to time series if they aren't already
HDD_ts <- ts(temperature.daily_avg$HDD)
CDD_ts <- ts(temperature.daily_avg$CDD)

# lag-2 for CDD 
lag2_HDD_ts <- c(NA, NA, HDD_ts[-(length(HDD_ts)-1): -length(HDD_ts)])
lag2_CDD_ts <- c(NA, NA, CDD_ts[-(length(CDD_ts)-1): -length(CDD_ts)])

par(mfrow = c(2, 3))  
plot(temperature.daily_avg$HDD,
     temperature.daily_avg$load,
     main = "Electricity Load vs HDD",
     xlab = "Heating Degree Days (HDD)",
     ylab = "Electricity Load",
     col = 'red',
     pch = 1)

# Plot for Electricity Load vs HDD
plot(temperature.daily_avg$lag_HDD,
     temperature.daily_avg$load,
     main = "Electricity Load vs HDD lag 1",
     xlab = "Heating Degree Days (lag-1 HDD)",
     ylab = "Electricity Load",
     col = 'red',
     pch = 1)
# Plot for Electricity Load vs HDD
plot(lag2_HDD_ts,
     temperature.daily_avg$load,
     main = "Electricity Load vs HDD lag 2",
     xlab = "Heating Degree Days (lag-2 HDD)",
     ylab = "Electricity Load",
     col = 'red',
     pch = 1)

# Plot for Electricity Load vs CDD
plot(temperature.daily_avg$CDD,
     temperature.daily_avg$load,
     main = "Electricity Load vs CDD",
     xlab = "Cooling Degree Days (CDD)",
     ylab = "Electricity Load",
     col = 'blue',
     pch = 1)


# Plot for Electricity Load vs CDD
plot(temperature.daily_avg$lag_CDD,
     temperature.daily_avg$load,
     main = "Electricity Load vs CDD lag 1",
     xlab = "Cooling Degree Days (lag-1 CDD)",
     ylab = "Electricity Load",
     col = 'blue',
     pch = 1)


# Plot for Electricity Load vs CDD
plot(lag2_CDD_ts,
     temperature.daily_avg$load,
     main = "Electricity Load vs CDD lag 2",
     xlab = "Cooling Degree Days (lag-2 CDD)",
     ylab = "Electricity Load",
     col = 'blue',
     pch = 1)


summary(temperature.daily_avg)




