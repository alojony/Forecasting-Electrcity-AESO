# -----Project Info---------
#  Temperature and Load   #
#  Exploratory Analysis   #
#   Jonathan A. Gonzalez  #
#      2024-02-08         #
# _______________________ #

## Get humidity, precipitation, sunlight
## Outliers
### Naive?
### Exp Smoothing
###




# Locale to ENG
Sys.setlocale("LC_TIME", "C")
# Load Libraries
library(timeSeries)
library(astsa)
library(forecast)

temperature <-
  read.csv(
    "~/Documents/Obsidian Vault/HEC/Session 4 Winter 2024/
    Forecasting/Project/AESO_NW/data/NW-AB_Temp.csv",
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

T_ref <- 18.00
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
  "~/Documents/Obsidian Vault/HEC/Session 4 Winter 2024/
  Forecasting/Project/AESO_NW/data/aeso.rdata"
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

# Basic scatter plot for Electricity Load vs HDD
plot(
  temperature.daily_avg$HDD,
  temperature.daily_avg$load,
  main = "Electricity Load vs HDD",
  xlab = "Heating Degree Days (HDD)",
  ylab = "Electricity Load",
  pch = 19
)


# Basic scatter plot for Electricity Load vs CDD
plot(
  temperature.daily_avg$CDD,
  temperature.daily_avg$load,
  main = "Electricity Load vs CDD",
  xlab = "Cooling Degree Days (CDD)",
  ylab = "Electricity Load",
  pch = 19
)


summary(temperature.daily_avg)
