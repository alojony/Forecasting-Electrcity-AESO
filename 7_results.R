get_std_and_mean <- function(x) {
    return(c(mean = mean(x), sd = sd(x), pct_bias = (mean(x) - mean(full_set$Northwest)) / mean(full_set$Northwest) * 100))
}

full_set$weekday <- weekdays(full_set$DT_MST)
# Get average and standard deviation of load for each weekday
weekday_stats <- aggregate(Northwest ~ weekday, data = full_set, FUN = get_std_and_mean)


# Get average and standard deviation of load for each month
full_set$month <- months(full_set$DT_MST)
month_stats <- aggregate(Northwest ~ month, data = full_set, FUN = get_std_and_mean)


# Get average and standard deviation of load for each season
season_stats <- aggregate(Northwest ~ season, data = full_set, FUN = get_std_and_mean)

# Make ACF plot
acf(full_set$Northwest, main = "ACF of Daily Max Northwest Load")


# ----- Item 6 - Collect seasonal accuracy measures for forecasts ----

mape <- function(forecast, observed) {
    return(mean(abs((
        observed - forecast
    ) / observed)) * 100)
}

pct_bias <- function(forecast, observed) {
    return(mean((forecast - observed) / observed) * 100)
}

pct_interval_coverage <- function(low_bound, high_bound, observed) {
    return(mean(ifelse(
        low_bound <= observed & observed <= high_bound, 1, 0
    )))
}

interval_size <- function(low_bound, high_bound) {
    return(mean(high_bound - low_bound))
}

validation_set <-
    subset(full_set, DT_MST > training_end & DT_MST <= validation_end)

accuracy_measures <- list()

for (s in unique(full_set$season)) {
    seasonal_subset <- subset(validation_set, season == s)

    accuracy_measures[["naive"]][[s]] <- list(
        mape = mape(seasonal_subset$naive_forecast, seasonal_subset$Northwest),
        pct_bias = pct_bias(seasonal_subset$naive_forecast, seasonal_subset$Northwest)
    )


    accuracy_measures[["7d naive"]][[s]] <- list(
        mape = mape(
            seasonal_subset$seasonal_forecast_7d,
            seasonal_subset$Northwest
        ),
        pct_bias = pct_bias(seasonal_subset$seasonal_forecast_7d, seasonal_subset$Northwest)
    )


    accuracy_measures[["30d naive"]][[s]] <- list(
        mape = mape(
            seasonal_subset$seasonal_forecast_30d,
            seasonal_subset$Northwest
        ),
        pct_bias = pct_bias(seasonal_subset$seasonal_forecast_30d, seasonal_subset$Northwest)
    )

    accuracy_measures[["rollmean"]][[s]] <- list(
        mape = mape(
            seasonal_subset$rollmean_forecast,
            seasonal_subset$Northwest
        ),
        pct_bias = pct_bias(seasonal_subset$rollmean_forecast, seasonal_subset$Northwest)
    )

    accuracy_measures[["simple exponential smoothing"]][[s]] <- list(
        mape = mape(seasonal_subset$ses_forecast, seasonal_subset$Northwest),
        pct_bias = pct_bias(seasonal_subset$ses_forecast, seasonal_subset$Northwest)
    )

    accuracy_measures[["holt"]][[s]] <- list(
        mape = mape(seasonal_subset$holt_forecast, seasonal_subset$Northwest),
        pct_bias = pct_bias(seasonal_subset$holt_forecast, seasonal_subset$Northwest),
        coverage_80 = pct_interval_coverage(
            seasonal_subset$holt_low_80,
            seasonal_subset$holt_high_80,
            seasonal_subset$Northwest
        ),
        coverage_95 = pct_interval_coverage(
            seasonal_subset$holt_low_95,
            seasonal_subset$holt_high_95,
            seasonal_subset$Northwest
        )
    )

    accuracy_measures[["holt-winters"]][[s]] <- list(
        mape = mape(seasonal_subset$hw_forecast, seasonal_subset$Northwest),
        pct_bias = pct_bias(seasonal_subset$hw_forecast, seasonal_subset$Northwest),
        coverage_80 = pct_interval_coverage(
            seasonal_subset$hw_low_80,
            seasonal_subset$hw_high_80,
            seasonal_subset$Northwest
        ),
        coverage_95 = pct_interval_coverage(
            seasonal_subset$hw_low_95,
            seasonal_subset$hw_high_95,
            seasonal_subset$Northwest
        )
    )

    accuracy_measures[["TBATS"]][[s]] <- list(
        mape = mape(seasonal_subset$tbats_forecast, seasonal_subset$Northwest),
        pct_bias = pct_bias(seasonal_subset$tbats_forecast, seasonal_subset$Northwest),
        coverage_80 = pct_interval_coverage(
            seasonal_subset$tbats_low_80,
            seasonal_subset$tbats_high_80,
            seasonal_subset$Northwest
        ),
        coverage_95 = pct_interval_coverage(
            seasonal_subset$tbats_low_95,
            seasonal_subset$tbats_high_95,
            seasonal_subset$Northwest
        )
    )

    accuracy_measures[["TBATS_rf"]][[s]] <- list(
        mape = mape(seasonal_subset$tbats_forecast, seasonal_subset$Northwest),
        pct_bias = pct_bias(seasonal_subset$tbats_forecast, seasonal_subset$Northwest),
        coverage_80 = pct_interval_coverage(
            seasonal_subset$tbats_rf_low_80,
            seasonal_subset$tbats_rf_high_80,
            seasonal_subset$Northwest
        ),
        coverage_95 = pct_interval_coverage(
            seasonal_subset$tbats_rf_low_95,
            seasonal_subset$tbats_rf_high_95,
            seasonal_subset$Northwest
        )
    )

    accuracy_measures[["ETS"]][[s]] <- list(
        mape = mape(seasonal_subset$ets_forecast, seasonal_subset$Northwest),
        pct_bias = pct_bias(seasonal_subset$ets_forecast, seasonal_subset$Northwest),
        coverage_80 = pct_interval_coverage(
            seasonal_subset$ets_low_80,
            seasonal_subset$ets_high_80,
            seasonal_subset$Northwest
        ),
        coverage_95 = pct_interval_coverage(
            seasonal_subset$ets_low_95,
            seasonal_subset$ets_high_95,
            seasonal_subset$Northwest
        )
    )

    accuracy_measures[["ar"]][[s]] <- list(
        mape = mape(seasonal_subset$ar_forecast, seasonal_subset$Northwest),
        pct_bias = pct_bias(seasonal_subset$ar_forecast, seasonal_subset$Northwest),
        coverage_80 = pct_interval_coverage(
            seasonal_subset$ar_low_80,
            seasonal_subset$ar_high_80,
            seasonal_subset$Northwest
        ),
        coverage_95 = pct_interval_coverage(
            seasonal_subset$ar_low_95,
            seasonal_subset$ar_high_95,
            seasonal_subset$Northwest
        )
    )

    accuracy_measures[["lm"]][[s]] <- list(
        mape = mape(seasonal_subset$lm_forecast, seasonal_subset$Northwest),
        pct_bias = pct_bias(seasonal_subset$lm_forecast, seasonal_subset$Northwest),
        coverage_80 = pct_interval_coverage(
            seasonal_subset$lm_low_80,
            seasonal_subset$lm_high_80,
            seasonal_subset$Northwest
        ),
        coverage_95 = pct_interval_coverage(
            seasonal_subset$lm_low_95,
            seasonal_subset$lm_high_95,
            seasonal_subset$Northwest
        )
    )

    accuracy_measures[["arx"]][[s]] <- list(
        mape = mape(seasonal_subset$arx_forecast, seasonal_subset$Northwest),
        pct_bias = pct_bias(seasonal_subset$arx_forecast, seasonal_subset$Northwest),
        coverage_80 = pct_interval_coverage(
            seasonal_subset$arx_low_80,
            seasonal_subset$arx_high_80,
            seasonal_subset$Northwest
        ),
        coverage_95 = pct_interval_coverage(
            seasonal_subset$arx_low_95,
            seasonal_subset$arx_high_95,
            seasonal_subset$Northwest
        )
    )

    accuracy_measures[["arima1_1_2"]][[s]] <- list(
        mape = mape(seasonal_subset$arima1_1_2_forecast, seasonal_subset$Northwest),
        pct_bias = pct_bias(seasonal_subset$arima1_1_2_forecast, seasonal_subset$Northwest),
        coverage_80 = pct_interval_coverage(
            seasonal_subset$arima1_1_2_low_80,
            seasonal_subset$arima1_1_2_high_80,
            seasonal_subset$Northwest
        ),
        coverage_95 = pct_interval_coverage(
            seasonal_subset$arima1_1_2_low_95,
            seasonal_subset$arima1_1_2_high_95,
            seasonal_subset$Northwest
        )
    )

    accuracy_measures[["arima2_0_2"]][[s]] <- list(
        mape = mape(seasonal_subset$arima2_0_2_forecast, seasonal_subset$Northwest),
        pct_bias = pct_bias(seasonal_subset$arima2_0_2_forecast, seasonal_subset$Northwest),
        coverage_80 = pct_interval_coverage(
            seasonal_subset$arima2_0_2_low_80,
            seasonal_subset$arima2_0_2_high_80,
            seasonal_subset$Northwest
        ),
        coverage_95 = pct_interval_coverage(
            seasonal_subset$arima2_0_2_low_95,
            seasonal_subset$arima2_0_2_high_95,
            seasonal_subset$Northwest
        )
    )

    accuracy_measures[["arima2_1_1"]][[s]] <- list(
        mape = mape(seasonal_subset$arima2_1_1_forecast, seasonal_subset$Northwest),
        pct_bias = pct_bias(seasonal_subset$arima2_1_1_forecast, seasonal_subset$Northwest),
        coverage_80 = pct_interval_coverage(
            seasonal_subset$arima2_1_1_low_80,
            seasonal_subset$arima2_1_1_high_80,
            seasonal_subset$Northwest
        ),
        coverage_95 = pct_interval_coverage(
            seasonal_subset$arima2_1_1_low_95,
            seasonal_subset$arima2_1_1_high_95,
            seasonal_subset$Northwest
        )
    )

    accuracy_measures[["arima3_0_0"]][[s]] <- list(
        mape = mape(seasonal_subset$arima3_0_0_forecast, seasonal_subset$Northwest),
        pct_bias = pct_bias(seasonal_subset$arima3_0_0_forecast, seasonal_subset$Northwest),
        coverage_80 = pct_interval_coverage(
            seasonal_subset$arima3_0_0_low_80,
            seasonal_subset$arima3_0_0_high_80,
            seasonal_subset$Northwest
        ),
        coverage_95 = pct_interval_coverage(
            seasonal_subset$arima3_0_0_low_95,
            seasonal_subset$arima3_0_0_high_95,
            seasonal_subset$Northwest
        )
    )
}

accuracy_measures[["naive"]][["year"]] <- list(
    mape = mape(validation_set$naive_forecast, validation_set$Northwest),
    pct_bias = pct_bias(validation_set$naive_forecast, validation_set$Northwest)
)


accuracy_measures[["7d naive"]][["year"]] <- list(
    mape = mape(
        validation_set$seasonal_forecast_7d,
        validation_set$Northwest
    ),
    pct_bias = pct_bias(validation_set$seasonal_forecast_7d, validation_set$Northwest)
)


accuracy_measures[["30d naive"]][["year"]] <- list(
    mape = mape(
        validation_set$seasonal_forecast_30d,
        validation_set$Northwest
    ),
    pct_bias = pct_bias(validation_set$seasonal_forecast_30d, validation_set$Northwest)
)

accuracy_measures[["rollmean"]][["year"]] <- list(
    mape = mape(
        validation_set$rollmean_forecast,
        validation_set$Northwest
    ),
    pct_bias = pct_bias(validation_set$rollmean_forecast, validation_set$Northwest)
)

accuracy_measures[["simple exponential smoothing"]][["year"]] <- list(
    mape = mape(validation_set$ses_forecast, validation_set$Northwest),
    pct_bias = pct_bias(validation_set$ses_forecast, validation_set$Northwest)
)

accuracy_measures[["holt"]][["year"]] <- list(
    mape = mape(validation_set$holt_forecast, validation_set$Northwest),
    pct_bias = pct_bias(validation_set$holt_forecast, validation_set$Northwest),
    coverage_80 = pct_interval_coverage(
        validation_set$holt_low_80,
        validation_set$holt_high_80,
        validation_set$Northwest
    ),
    coverage_95 = pct_interval_coverage(
        validation_set$holt_low_95,
        validation_set$holt_high_95,
        validation_set$Northwest
    )
)

accuracy_measures[["holt-winters"]][["year"]] <- list(
    mape = mape(validation_set$hw_forecast, validation_set$Northwest),
    pct_bias = pct_bias(validation_set$hw_forecast, validation_set$Northwest),
    coverage_80 = pct_interval_coverage(
        validation_set$hw_low_80,
        validation_set$hw_high_80,
        validation_set$Northwest
    ),
    coverage_95 = pct_interval_coverage(
        validation_set$hw_low_95,
        validation_set$hw_high_95,
        validation_set$Northwest
    )
)

accuracy_measures[["TBATS"]][["year"]] <- list(
    mape = mape(validation_set$tbats_forecast, validation_set$Northwest),
    pct_bias = pct_bias(validation_set$tbats_forecast, validation_set$Northwest),
    coverage_80 = pct_interval_coverage(
        validation_set$tbats_low_80,
        validation_set$tbats_high_80,
        validation_set$Northwest
    ),
    coverage_95 = pct_interval_coverage(
        validation_set$tbats_low_95,
        validation_set$tbats_high_95,
        validation_set$Northwest
    )
)

accuracy_measures[["TBATS_rf"]][["year"]] <- list(
    mape = mape(validation_set$tbats_rf_forecast, validation_set$Northwest),
    pct_bias = pct_bias(validation_set$tbats_rf_forecast, validation_set$Northwest),
    coverage_80 = pct_interval_coverage(
        validation_set$tbats_rf_low_80,
        validation_set$tbats_rf_high_80,
        validation_set$Northwest
    ),
    coverage_95 = pct_interval_coverage(
        validation_set$tbats_rf_low_95,
        validation_set$tbats_rf_high_95,
        validation_set$Northwest
    )
)

accuracy_measures[["ETS"]][["year"]] <- list(
    mape = mape(validation_set$ets_forecast, validation_set$Northwest),
    pct_bias = pct_bias(validation_set$ets_forecast, validation_set$Northwest),
    coverage_80 = pct_interval_coverage(
        validation_set$ets_low_80,
        validation_set$ets_high_80,
        validation_set$Northwest
    ),
    coverage_95 = pct_interval_coverage(
        validation_set$ets_low_95,
        validation_set$ets_high_95,
        validation_set$Northwest
    )
)



accuracy_measures[["ar"]][["year"]] <- list(
    mape = mape(validation_set$ar_forecast, validation_set$Northwest),
    pct_bias = pct_bias(validation_set$ar_forecast, validation_set$Northwest),
    coverage_80 = pct_interval_coverage(
        validation_set$ar_low_80,
        validation_set$ar_high_80,
        validation_set$Northwest
    ),
    coverage_95 = pct_interval_coverage(
        validation_set$ar_low_95,
        validation_set$ar_high_95,
        validation_set$Northwest
    )
)

accuracy_measures[["lm"]][["year"]] <- list(
    mape = mape(validation_set$lm_forecast, validation_set$Northwest),
    pct_bias = pct_bias(validation_set$lm_forecast, validation_set$Northwest),
    coverage_80 = pct_interval_coverage(
        validation_set$lm_low_80,
        validation_set$lm_high_80,
        validation_set$Northwest
    ),
    coverage_95 = pct_interval_coverage(
        validation_set$lm_low_95,
        validation_set$lm_high_95,
        validation_set$Northwest
    )
)

accuracy_measures[["arx"]][["year"]] <- list(
    mape = mape(validation_set$arx_forecast, validation_set$Northwest),
    pct_bias = pct_bias(validation_set$arx_forecast, validation_set$Northwest),
    coverage_80 = pct_interval_coverage(
        validation_set$arx_low_80,
        validation_set$arx_high_80,
        validation_set$Northwest
    ),
    coverage_95 = pct_interval_coverage(
        validation_set$arx_low_95,
        validation_set$arx_high_95,
        validation_set$Northwest
    )
)

accuracy_measures[["arima1_1_2"]][["year"]] <- list(
    mape = mape(validation_set$arima1_1_2_forecast, validation_set$Northwest),
    pct_bias = pct_bias(validation_set$arima1_1_2_forecast, validation_set$Northwest),
    coverage_80 = pct_interval_coverage(
        validation_set$arima1_1_2_low_80,
        validation_set$arima1_1_2_high_80,
        validation_set$Northwest
    ),
    coverage_95 = pct_interval_coverage(
        validation_set$arima1_1_2_low_95,
        validation_set$arima1_1_2_high_95,
        validation_set$Northwest
    )
)

accuracy_measures[["arima2_0_2"]][["year"]] <- list(
    mape = mape(validation_set$arima2_0_2_forecast, validation_set$Northwest),
    pct_bias = pct_bias(validation_set$arima2_0_2_forecast, validation_set$Northwest),
    coverage_80 = pct_interval_coverage(
        validation_set$arima2_0_2_low_80,
        validation_set$arima2_0_2_high_80,
        validation_set$Northwest
    ),
    coverage_95 = pct_interval_coverage(
        validation_set$arima2_0_2_low_95,
        validation_set$arima2_0_2_high_95,
        validation_set$Northwest
    )
)

accuracy_measures[["arima2_1_1"]][["year"]] <- list(
    mape = mape(validation_set$arima2_1_1_forecast, validation_set$Northwest),
    pct_bias = pct_bias(validation_set$arima2_1_1_forecast, validation_set$Northwest),
    coverage_80 = pct_interval_coverage(
        validation_set$arima2_1_1_low_80,
        validation_set$arima2_1_1_high_80,
        validation_set$Northwest
    ),
    coverage_95 = pct_interval_coverage(
        validation_set$arima2_1_1_low_95,
        validation_set$arima2_1_1_high_95,
        validation_set$Northwest
    )
)

accuracy_measures[["arima3_0_0"]][["year"]] <- list(
    mape = mape(validation_set$arima3_0_0_forecast, validation_set$Northwest),
    pct_bias = pct_bias(validation_set$arima3_0_0_forecast, validation_set$Northwest),
    coverage_80 = pct_interval_coverage(
        validation_set$arima3_0_0_low_80,
        validation_set$arima3_0_0_high_80,
        validation_set$Northwest
    ),
    coverage_95 = pct_interval_coverage(
        validation_set$arima3_0_0_low_95,
        validation_set$arima3_0_0_high_95,
        validation_set$Northwest
    )
)




# ----- Output to JSON -----
install.packages("rjson")

library(rjson)
json_str <- toJSON(accuracy_measures)
write(json_str, file = "accuracy_measures.json")


# ----- Diebold-Mariano -----
# Diebold-Mariano Test
# Function to calculate the Diebold-Mariano test
dm.test(validation_set$arima2_1_1_forecast, validation_set$arima3_0_0_forecast, h = 1, alternative = "two.sided")
