# ***************************** #
#    Forecasting Electricity    #
#    Northwest Alberta.         #
# --- Preprocessing Script      #
# Outlier treatment             #
# Temperature & Weather Data    #
# Noise for Regresors on Temp.  #
# ---------------------------   #
#      Jonathan Gonzalez.       #
#        April 13 2024.         #
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

###

# Function to add noise to a column in meteo_data
add_noise <- function(column, data_frame) {
  # Calculate the range of the column
  value_range <- max(data_frame[[column]], na.rm = TRUE) - min(data_frame[[column]], na.rm = TRUE)
  
  # Calculate the average error based on the range
  avg_error <- (0.02 * value_range)^2
  
  # Generate noise
  noise <- rnorm(n = length(data_frame[[column]]), mean = 0, sd = sqrt(avg_error))
  
  # Add noise to the original column values
  data_frame[[paste0("noisy_", column)]] <- data_frame[[column]] + noise
  
  return(data_frame)
}



### 

# Function to calculate wind chill using the Canadian wind chill index formula
calculate_wind_chill <- function(temperature, wind_speed) {
  # Convert wind speed from meters per second to kilometers per hour if necessary
  # wind_speed_kmh <- wind_speed * 3.6
  
  # Apply the Canadian wind chill formula
  wind_chill <- ifelse(temperature <= 10 & wind_speed > 4.8,
                       13.12 + 0.6215 * temperature - 11.37 * (wind_speed^0.16) + 
                         0.3965 * temperature * (wind_speed^0.16),
                       temperature) # If conditions not met, wind chill equals the air temperature
  
  return(wind_chill)
}

## Libraries and Config ##########

# Locale to ENG
Sys.setlocale("LC_TIME", "C")
# Load Libraries
library(timeSeries)
library(astsa)
library(forecast)

#---- Temperature and Weather Data -----

# Temperature Data extracted from: 
# https://acis.alberta.ca/acis/township-data-viewer.jsp
# The 9 main townships that AESO designates as the Northwestern area of Alberta

temperature <- read.csv("./data/NW-AB_Temp.csv", sep = ",", header = TRUE)

# Rename Columns for easy accesibility
new_column_names <- c(
  "township", "Date", "avg_temp", "min_temp", "max_temp",
  "precip", "precip_acc", "humidity_avg", "snow-water_eq",
  "solar_rad", "wind_speed"
)

colnames(temperature) <- new_column_names
head(temperature)

# Extract Desired Columns
meteo_data <- temperature[c(
  "township", "Date", "avg_temp", "min_temp", "max_temp",
  "humidity_avg", "wind_speed"
)]
head(meteo_data)
length(meteo_data$Date)
# Fill NA on temperature data

meteo_data$avg_temp <- fill_NAs_with_nearest_averages(meteo_data$avg_temp)
meteo_data$min_temp <- fill_NAs_with_nearest_averages(meteo_data$min_temp)
meteo_data$max_temp <- fill_NAs_with_nearest_averages(meteo_data$max_temp)
meteo_data$humidity_avg <- fill_NAs_with_nearest_averages(meteo_data$humidity_avg)
meteo_data$wind_speed <- fill_NAs_with_nearest_averages(meteo_data$wind_speed)

# Generate noise
head(meteo_data)
length(meteo_data$Date)

# Ensure the 'date' column is in the correct Date format
meteo_data$Date <- as.Date(meteo_data$Date)

# Get the temp range and calculate the noise
temp_range <- max(meteo_data$max_temp) - min(meteo_data$min_temp)
avg_error <- (0.02 * temp_range)^2
noise <- rnorm(length(meteo_data$avg_temp), 0, avg_error)
meteo_data$noisy_temp <- meteo_data$avg_temp + noise

meteo_data <- subset(meteo_data, select = -c(township))
meteo_data <- aggregate(. ~ Date, data = meteo_data, FUN = mean)
head(meteo_data)
length(meteo_data$Date)

# Plot avg_temp
plot(meteo_data$Date, meteo_data$noisy_temp,
  type = "l", col = "red",
  ylim = range(c(meteo_data$avg_temp, meteo_data$noisy_temp)),
  xlab = "Observation", ylab = "Temperature",
  main = "Average vs. Noisy Temperatures \n with same half the std of avg temp"
)
# Add noisy_temp to the same plot
lines(meteo_data$Date, meteo_data$avg_temp, type = "l", col = "blue")

# Add a legend
legend("topright",
  legend = c("Avg Temp", "Noisy Temp"),
  col = c("blue", "red"), lty = 1
)

# Temperature yearly
meteo_data_year <- meteo_data[format(meteo_data$Date, "%Y") == "2013", ]

# Plotting
plot(meteo_data_year$Date, meteo_data_year$avg_temp,
  type = "l", col = "blue",
  ylim = range(c(meteo_data_year$avg_temp, meteo_data_year$noisy_temp)),
  xlab = "Date", ylab = "Temperature",
  main = "Average vs. Noisy Temperatures for 2023"
)
lines(meteo_data_year$Date, meteo_data_year$noisy_temp, type = "l", col = "red")

# Adding a legend
legend("topright",
  legend = c("Avg Temp", "Noisy Temp"),
  col = c("blue", "red"), lty = 1
)

# Windchill Variable 
meteo_data$wind_chill <- mapply(calculate_wind_chill, 
                                meteo_data$avg_temp, 
                                meteo_data$wind_speed)

# Apply the function to each column except 'date' and 'avg_temp'
columns_to_noise <- setdiff(names(meteo_data), c("Date", "avg_temp", "township"))

for(column in columns_to_noise) {
  meteo_data <- add_noise(column, meteo_data)
}

## Noise for other Variables
meteo_data <- aggregate(. ~ Date, data = meteo_data, 
                        FUN = mean, na.action = na.pass)

# Check the updated dataset
head(meteo_data)

## Plots for noisy variables 
for (column in columns_to_noise) {
  # Define the original and noisy column names
  original_column <- column
  noisy_column <- paste0("noisy_", column)
  
  # Create a plot with the original data
  plot(meteo_data$Date, meteo_data[[original_column]], type = 'l', 
       main = paste("Original vs. Noisy", column), xlab = "Date", 
       ylab = column, col = "blue", lwd = 2)
  
  # Add the noisy data to the plot
  lines(meteo_data$Date, meteo_data[[noisy_column]], 
        type = 'l', col = "red", lwd = 2)
  
  # Add a legend to distinguish between original and noisy data
  legend("topright", legend = c("Original", "Noisy"),
         col = c("blue", "red"), lty = 1, cex = 0.8)
}


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
  meteo_data$Date,
  meteo_data$HDD,
  type = "l",
  col = "red"
)
lines(meteo_data$Date,
  meteo_data$CDD,
  col = "blue"
)

#daily.max$date <- as.Date(daily.max$DT_MST)
head(meteo_data)
mergedData <- merge(daily.max, meteo_data, by = "Date")
cleanData <- na.omit(mergedData)
head(mergedData)
daily.max <- subset(daily.max, Date <= as.Date("2019-12-31"))
meteo_data$load <- daily.max$Northwest
max(meteo_data$Date)
tail(daily.max)

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

#----Check for correlation and colinearity between variables ----


# Calculate correlation matrix for numeric variables
numeric_vars <- meteo_data[, sapply(meteo_data, is.numeric)]
cor_matrix <- cor(numeric_vars, use = "complete.obs")
cor_matrix

# Define a high correlation threshold (e.g., 0.7 or 0.8)
high_corr_threshold <- 0.7

# Find pairs of variables (excluding 'load') with correlation above the threshold
high_corr_pairs <- which(abs(cor_matrix[-ncol(cor_matrix),
                      -ncol(cor_matrix)]) > high_corr_threshold, arr.ind = TRUE)

# Since 'which' with 'arr.ind=TRUE' returns matrix indices, convert these to variable names
high_corr_var_pairs <- names(numeric_vars)[-ncol(numeric_vars)][unique(as.vector(high_corr_pairs))]
high_corr_var_pairs


summary(meteo_data)

head(daily.max)
head(meteo_data)

full_set <- cbind(full_set,meteo_data)

# Suppose 'fullset' is your data frame
current_columns <- names(full_set)
northwest_position <- which(current_columns == "Northwest")
load_position <- which(current_columns == "load")
new_order <- 1:ncol(full_set)
new_order <- new_order[-load_position]
new_order <- c(new_order[1:(northwest_position+1)], load_position, new_order[(northwest_position+2):length(new_order)])


# Reorder the columns
full_set <- full_set[, new_order]
names(full_set)

# Insert 'load' right after 'Northwest'
new_order <- c(new_order[1:(northwest_position+1)], load_position, new_order[(northwest_position+2):length(new_order)])



