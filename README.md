# MATH60638A - Forecasting Methods - Term Project

#### Daily peak hourly load forecasting of Northwestern Alberta electricity consumption

Some scripts depend on variables which are defined in other scripts, so it is important to execute them in the specified order.
This structure can be modified at any time if any individual file becomes too large and needs to be split.

Repository structure :

-   1_temperature_data.R: Parsing of historical climate data for the region
-   2_aeso_data.R: Parsing of AESO dataset, outlier treatment, aggregation to daily peak hourly load and creation of dummy variables.
-   3_census_data.R: Parsing of demographic and economic data for the region.
-   4_forecasting.R : Definition of training, testing and validation sets. Production of forecasts. Evaluation.
-   "data" folder: Contains all additional data sources used in the project