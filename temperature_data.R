# Temperature Data extracted from https://acis.alberta.ca/acis/township-data-viewer.jsp
# Took the 9 main townships that AESO designates as the Northwestern area of Alberta

temperature <- read.csv("./data/NW-AB_Temp.csv",sep = ",",header = TRUE)
summary(temperature)

##  Interpolate missing NAs on every township 

# Get unique townships in the dataset
townships <- unique(temperature$Township)

# Loop through each township
for (township in townships) {
  # Subset the data for the current township
  subset_data <- temperature[temperature$Township == township, ]
  
  # Interpolate missing values for Temp.Min
  temperature_min_ts <- subset_data$Temp.Min
  missing_min_values <- is.na(temperature_min_ts)
  temperature_min_ts[missing_min_values] <- approx(x = which(!missing_min_values),
                                                   y = temperature_min_ts[!missing_min_values],
                                                   xout = which(missing_min_values))$y
  
  # Interpolate missing values for Temp.Max
  temperature_max_ts <- subset_data$Temp.Max
  missing_max_values <- is.na(temperature_max_ts)
  temperature_max_ts[missing_max_values] <- approx(x = which(!missing_max_values),
                                                   y = temperature_max_ts[!missing_max_values],
                                                   xout = which(missing_max_values))$y
  
  # Replace the interpolated values in the original data
  temperature[temperature$Township == township, "Temp.Min"] <- temperature_min_ts
  temperature[temperature$Township == township, "Temp.Max"] <- temperature_max_ts
}

# Check for NA
summary(temperature)

na_rows <- subset(temperature, is.na(Temp.Min) & is.na(Temp.Max))

# Display the subset with NA rows
summary(na_rows)
which(is.na(na_rows)) # Consecutive time stamps
unique(na_rows$Township)  #why?
# Big wildfire in rainbow lake, will have to interpolate or 
# surely will disapear with the averaging due to the close points 


## Join the data to make an average for all the AESO Northwest

# Convert Date column to Date format
temperature$Date <- as.Date(temperature$Date)

# Calculate the average temperature for each day
temperature.daily_avg <- aggregate(cbind(Temp.Min, Temp.Max) ~ Date, data = temperature, FUN = mean, na.rm = TRUE)

# Rename the columns for the new dataset
colnames(temperature.daily_avg) <- c("Date", "Temp.Min", "Temp.Max")

# Calculate the average temperature for each date
temperature.daily_avg$Temp.Avg <- (temperature.daily_avg$Temp.Min + temperature.daily_avg$Temp.Max) / 2
# Display the updated data frame to verify the new column

# Make a Daily Temperature Average (dta), CDD and HDD dataset
dta <- timeSeries(temperature.daily_avg$Temp.Avg, temperature.daily_avg$Date, format="%Y_%m-%d")
cdd <- timeSeries(temperature.daily_avg$Temp.Min, temperature.daily_avg$Date, format="%Y-%m-%d")
hdd <- timeSeries(temperature.daily_avg$Temp.Max, temperature.daily_avg$Date, format="%Y-%m-%d")

colnames(dta) <- c("temp")
colnames(cdd) <- c("temp")
colnames(hdd) <- c("temp")