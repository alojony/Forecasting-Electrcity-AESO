
# ----- plots for presentation ----
# Plot the data

par(mfrow=c(1,1))

start_date ='2016-01-01 00:00:00'
end_date ='2017-12-31 00:00:00'

aeso.nw.2y <- aeso.nw[aeso.nw$DT_MST >= as.POSIXct(start_date) &
                        aeso.nw$DT_MST <= as.POSIXct(end_date), ]
par(mar = par("mar") + c(0,0,4,0))

# Plot the data
plot(aeso.nw.2y$DT_MST, aeso.nw.2y$Northwest,
     type = "l",
     main = "Electricity Load Values for Northwest Alberta (2016-2017)",
     xlab = "Date", ylab = "Electricity Load (MW)",
     ylim = c(mintik, maxtik),
     xaxt = "n" # Turn off the default x-axis to customize it
)

# Add year axis at the bottom
axis(1, at = as.numeric(seq(as.POSIXct(start_date), as.POSIXct(end_date), 
                            by = "year")),
     labels = seq(2016, 2017, by = 1))

# Add month axis on the top with three-letter month abbreviations
months <- seq(as.POSIXct(start_date), as.POSIXct(end_date), by = "month")

# Add the months on the top axis
axis(3, at = as.numeric(months), labels = format(months, "%b"), tick = FALSE)

# Draw vertical grid lines for each month
abline(v = as.numeric(months), col = "lightgray", lty = "dotted")

# Add a box around the plot to tidy up
box()

###
par(mfrow=c(1,1))

# Plot the boxplot
boxplot(full_set$Northwest ~ full_set$season,
        main = "Electric Consumption Patterns by Season in NW Alberta",
        xlab = "Season",
        ylab = "Electricity Consumption (MW)",
        col = "blue") # Use any color you like

# To ensure the seasons are in order, you might want to convert the season column to a factor with ordered levels
full_set$season <- factor(full_set$season, levels = c("Winter", "Spring", "Summer", "Autumn"))


par(mfrow=c(1,1))

# Plot the boxplot
boxplot(full_set$Northwest ~ full_set$Weekday,
        main = "Electric Consumption Patterns by Weekday in NW Alberta",
        xlab = "Weekday",
        ylab = "Electricity Consumption (MW)",
        col = "blue") # Use any color you like

# To ensure the seasons are in order, you might want to convert the season column to a factor with ordered levels
full_set$Weekday <- factor(full_set$season, levels = c("Monday", "Tuesday",
                                                       "Wednesday", "Thursday",
                                                       "Friday", "Saturday",
                                                       "Sunday"))

# Plot the boxplot
boxplot(full_set$Northwest ~ full_set$Weekday,
        main = "Electric Consumption Patterns by Weekday in NW Alberta",
        xlab = "Weekday",
        ylab = "Electricity Consumption (MW)",
        col = "blue") # Use any color you like


# Extract the month from the DT_MST column (as a factor)
full_set$Month <- factor(format(full_set$DT_MST, "%B"), 
                         levels = c("January", "February", "March", "April", "May", "June", "July",
                                    "August", "September", "October", "November", "December"))

# Plot the boxplot grouping by the Month factor
boxplot(full_set$Northwest ~ full_set$Month,
        main = "Electric Consumption Patterns by Month in NW Alberta",
        xlab = "Month",
        ylab = "Electricity Consumption (MW)",
        col = "blue")  # Use any color you like

# To ensure the seasons are in order, you might want to convert the season column to a factor with ordered levels

# Convert the IsHoliday column to a factor with custom labels
full_set$IsHoliday <- factor(full_set$IsHoliday, levels = c(0, 1), labels = c("Not Holiday", "Holiday"))

# Plot the boxplot with the updated factor levels as x-axis tick labels
boxplot(full_set$Northwest ~ full_set$IsHoliday,
        main = "Electric Consumption Patterns by Holidays in NW Alberta",
        xlab = "Holidays",
        ylab = "Electricity Consumption (MW)",
        col = "blue") # This will now use "Not Holiday" and "Holiday" as the x-axis labels



# Plot the boxplot
boxplot(full_set$avg_temp ~ full_set$season,
        main = "Temperature Patterns by Season in NW Alberta",
        xlab = "Season",
        ylab = "Temperature (°C)",
        col = "blue") # Use any color you like


# Plot the boxplot
boxplot(full_set$humidity_avg ~ full_set$season,
        main = "Relative Humidity Average by Season in NW Alberta",
        xlab = "Season",
        ylab = "Humidity Percentage",
        col = "blue") # Use any color you like


# Plot the boxplot
boxplot(full_set$wind_chill ~ full_set$season,
        main = "Wind Chill by Season in NW Alberta",
        xlab = "Season",
        ylab = "Wind Chill (°C)",
        col = "blue") # Use any color you like


# Plot the boxplot
boxplot(full_set$wind_chill ~ full_set$season,
        main = "Wind Chill by Season in NW Alberta",
        xlab = "Season",
        ylab = "Wind Chill (°C)",
        col = "blue") # Use any color you like


# Calculate mean and standard deviation by month
monthly_stats <- aggregate(full_set$Northwest ~ format(full_set$DT_MST, "%B"), 
                           data = full_set, 
                           FUN = function(x) c(mean = mean(x), sd = sd(x)))

# Convert results into a data frame
monthly_stats_df <- do.call(data.frame, monthly_stats)

# Rename the columns
names(monthly_stats_df) <- c("Month", "Mean", "Standard_Deviation")

# Calculate the overall mean
overall_mean <- mean(full_set$Northwest)

# Calculate percent difference from the overall mean
monthly_stats_df$Percent_Difference_From_Mean <- 
  (monthly_stats_df$Mean - overall_mean) / overall_mean * 100

# Order the data by month
months_ordered <- c("January", "February", "March", "April", "May", "June",
                    "July", "August", "September", "October", "November", "December")
monthly_stats_df$Month <- factor(monthly_stats_df$Month, levels = months_ordered)
monthly_stats_df <- monthly_stats_df[order(monthly_stats_df$Month), ]



# Convert the IsHoliday column to a factor with custom labels
full_set$IsHoliday <- factor(full_set$IsHoliday, levels = c(0, 1), labels = c("Not Holiday", "Holiday"))

# Calculate mean and standard deviation by holiday
holiday_stats <- aggregate(full_set$Northwest ~ full_set$IsHoliday, 
                           data = full_set, 
                           FUN = function(x) c(mean = mean(x), sd = sd(x)))

# Convert results into a data frame
holiday_stats_df <- do.call(data.frame, holiday_stats)

# Rename the columns
names(holiday_stats_df) <- c("IsHoliday", "Mean", "Standard_Deviation")

# Calculate percent difference from the overall mean
holiday_stats_df$Percent_Difference_From_Mean <- 
  (holiday_stats_df$Mean - overall_mean) / overall_mean * 100

