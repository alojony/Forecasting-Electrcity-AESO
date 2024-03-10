library(timeSeries)
library(astsa)
library(forecast)

# -----Load and parse data-----

load("./data/aeso.RData")

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

# ----- TODO: Correct for outliers ----


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

# Item 6 - Identify all holidays and asses their impact on daily peak hourly load
holiday_dates <- read.csv("./data/holidays.csv", colClasses=c("Date"))$Date
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
