install.packages("rjson")

library(rjson)
json_str <- toJSON(accuracy_measures)
write(json_str, file = "accuracy_measures.json")



get_std_and_mean <- function(x) {
    return(c(mean = mean(x), sd = sd(x), pct_bias= (mean(x) - mean(full_set$Northwest)) / mean(full_set$Northwest) * 100))
}

full_set$weekday <- weekdays(full_set$DT_MST)
# Get average and standard deviation of load for each weekday
weekday_stats <- aggregate(Northwest ~ weekday, data = full_set, FUN =get_std_and_mean)


# Get average and standard deviation of load for each month
full_set$month <- months(full_set$DT_MST)
month_stats <- aggregate(Northwest ~ month, data = full_set, FUN =get_std_and_mean)


# Get average and standard deviation of load for each season
season_stats <- aggregate(Northwest ~ season, data = full_set, FUN =get_std_and_mean)

# Make ACF plot
acf(full_set$Northwest, main = "ACF of Daily Max Northwest Load")
