# ***************************** #
#    Forecasting Electricity    #
#    Northwest Alberta.         #
# --- Zone Information Script-- #
#           Holidays            #
#           Population          #
#           Consumption         #
# ---------------------------   #
#      Jonathan Gonzalez.       #
#      Ghali Lahlou             #
#     Xavier Péeladeau-Asselin  #
#        April 18 2024.         #
# ***************************** #


# ---- Table for population ----

filename <-
  "./data/population-estimates-ab-census-subdivision-municipal-2016-to-current.csv"
Municipal_Population <-
  read.csv(filename, skip = 3, header = TRUE, stringsAsFactors = FALSE)

# Filter to keep only the municpalities of NW Alberta
Municipal_Population <-
  Municipal_Population[Municipal_Population[, 1] >= 4817, ]
Municipal_Population <-
  Municipal_Population[-((nrow(Municipal_Population) - 1):
  nrow(Municipal_Population)), ]
Municipal_Population[5:11] <-
  lapply(Municipal_Population[5:11], function(x) as.numeric(as.character(x)))

# Rename the columns for clarity
col_names <- names(Municipal_Population)
col_names[5:11] <- as.character(2016:2022)
names(Municipal_Population) <- col_names

# Sum the population data to get the total population for each year
Northwest_Population <- sapply(Municipal_Population[5:11], sum, na.rm = TRUE)

# Create a table for the summed population
Northwest_Population <-
  setNames(data.frame(t(Northwest_Population)), col_names[5:11])

# Calculate the average growth from 2016 to 2022 to extrapolate  for 2011-2015
growth_rates <-
  sapply(
    2:7,
    function(i) {
      Northwest_Population[1, i] /
        Northwest_Population[1, i - 1] - 1
    }
  )
average_growth_rate <- mean(growth_rates, na.rm = TRUE)
new_columns <- data.frame(matrix(ncol = 5, nrow = 1))
for (year in 1:5) {
  new_columns[1, year] <-
    Northwest_Population[1, 1] / ((1 + average_growth_rate)^(6 - year))
}
colnames(new_columns) <- as.character(2011:2015)

# Combine the new columns with the existing table
Northwest_Population <- cbind(new_columns, Northwest_Population)

# Remove the last three columns (2020-2022)
Northwest_Population <-
  Northwest_Population[, -((ncol(Northwest_Population) - 2):
  ncol(Northwest_Population))]


# Create a new data frame with 'Year' and 'Northwest_Population' columns
yearly_summary <- data.frame(
  Year = as.numeric(2011:2019),
  Population = as.numeric(Northwest_Population)
)


# Population of Alberta manually extracted
# from population-estimates-ab-annual-1921-to-current.csv
Alberta_Population <-
  c(
    3789030, 3874548, 3981011, 4083648, 4144491,
    4196061, 4241100, 4298275, 4361694
  )
yearly_summary$Alberta_Population <- Alberta_Population


# ---- YEARLY SUMMARY ----

# Aggregate aeso data by year for Northwest and Total consumption
yearly_consumption <-
  aggregate(cbind(Northwest, Total) ~ Year, data = full_set, FUN = sum)

# Merge yearly_consumption with Northwest_Population
yearly_summary <- merge(yearly_summary,
  yearly_consumption,
  by = "Year", all.x = TRUE
)


# Calculate Consumption Per Capita for Alberta in MWh
yearly_summary$Consumption_Percapita_Alberta <-
  yearly_summary$Total / yearly_summary$Alberta_Population

# Calculate Consumption Per Capita for Northwest in MWh
yearly_summary$Consumption_Percapita_Northwest <-
  yearly_summary$Northwest / yearly_summary$Population
