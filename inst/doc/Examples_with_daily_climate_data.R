## ---- fig.align='center', fig.width=8, fig.height=8, fig.cap=paste("Figure 1: Glimpse of daily temperautres for swit272 site."), message=FALSE, warning=FALSE, include=TRUE----
# Load the dendroTools and ggplot2 R packages
library(dendroTools)
library(ggplot2)

# 1 Load an example data (source: E-OBS)
data("swit272_daily_temperatures")
data("swit272_daily_precipitation")

# 2 Transform data into wide format
swit272_daily_temperatures <- data_transform(swit272_daily_temperatures, format = 'daily', date_format = 'ymd')
swit272_daily_precipitation <- data_transform(swit272_daily_precipitation, format = 'daily', date_format = 'ymd')

# 3 Glimpse daily data
glimpse_daily_data(env_data = swit272_daily_temperatures, na.color = "red") + 
  theme(legend.position = "bottom")

## ---- fig.align='center', fig.width=8, fig.height=8, fig.cap=paste("Figure 2: Glimpse of daily precipitation for swit272 site."), message=FALSE, warning=FALSE, include=TRUE----

glimpse_daily_data(env_data = swit272_daily_precipitation, na.color = "red") + 
  theme(legend.position = "bottom")


## ---- results = 'hide'--------------------------------------------------------
# Load the dendroTools R package
library(dendroTools)

# Load data
data(data_MVA)
data(LJ_daily_temperatures)

# Example with fixed width
example_fixed_width <- daily_response(response = data_MVA, env_data = LJ_daily_temperatures,
                                   method = "cor", fixed_width = 60,
                                   row_names_subset = TRUE, remove_insignificant = TRUE,
                                   alpha = 0.05)


## ---- fig.align='center', fig.width=10, fig.height=8, fig.cap=paste("Figure 3: The MVA parameter contains the optimal temperature signal from March 14 (DOY 73) to May 12 (DOY 132).")----

summary(example_fixed_width)

plot(example_fixed_width, type = 1)


## ---- results = 'hide'--------------------------------------------------------
# Load the dendroTools R package
library(dendroTools)

# Load the data
data(data_MVA)
data(LJ_daily_temperatures)

# Example for past and present
example_MVA_early <- daily_response(response = data_MVA, env_data = LJ_daily_temperatures,
                              method = "cor", lower_limit = 55, upper_limit = 65,
                              row_names_subset = TRUE, previous_year = TRUE,
                              remove_insignificant = TRUE, alpha = 0.05, 
                              subset_years = c(1941, 1980))

example_MVA_late <- daily_response(response = data_MVA, env_data = LJ_daily_temperatures,
                                   method = "cor", lower_limit = 55, upper_limit = 65,
                                   row_names_subset = TRUE, previous_year = TRUE,
                                   remove_insignificant = TRUE, alpha = 0.05, 
                                   subset_years = c(1981, 2010))

## ---- fig.align='center', warning=FALSE, fig.width = 10, fig.height = 8, fig.cap=paste("Figure 4: The temporal correlations pattern for the period 1941-1980"), message=FALSE, warning=FALSE, include=TRUE----
plot(example_MVA_early, type = 2)

## ---- fig.align='center', warning=FALSE, fig.width=10, fig.height=8, fig.cap=paste("Figure 5: The temporal correlations pattern for the period 1981-2010"), message=FALSE, warning=FALSE, include=TRUE----
plot(example_MVA_late, type = 2)

## ---- results = 'hide'--------------------------------------------------------
# Load the dendroTools R package
library(dendroTools)

# Load data
data(data_TRW_1)
data(LJ_daily_temperatures)

# Example negative correlations
data(data_TRW_1)
example_restricted_interval <- daily_response(response = data_TRW_1, env_data = LJ_daily_temperatures,
                                   method = "cor", lower_limit = 55, upper_limit = 65,
                                   previous_year = FALSE, row_names_subset = TRUE, remove_insignificant = TRUE,
                                   alpha = 0.05, day_interval = c(150, 210))

## ---- fig.align='center', warning=FALSE, fig.width=10, fig.height=8, fig.cap=paste("Figure 6: Climate-growth correlations calculated only for a subset of time window from DOY 150 to DOY 210"), message=FALSE, warning=FALSE, include=TRUE----

plot(example_restricted_interval, type = 2)


## -----------------------------------------------------------------------------
# Load the dendroTools and brnn R package
library(dendroTools)

# Example of multiproxy analysis
data(example_proxies_1)
data(LJ_daily_temperatures)

# Summary of the example_proxies_1 data frame
summary(example_proxies_1)

## -----------------------------------------------------------------------------
cor(example_proxies_1)

## ---- results = 'hide'--------------------------------------------------------
example_multiproxy <- daily_response(response = example_proxies_1, 
                                     env_data = LJ_daily_temperatures, 
                                     method = "lm", metric = "adj.r.squared", 
                                     lower_limit = 63, upper_limit = 67, 
                                     row_names_subset = TRUE, previous_year = FALSE, 
                                     remove_insignificant = TRUE, alpha = 0.05)

## ---- fig.align='center', warning=FALSE, fig.width=10, fig.height=8, fig.cap=paste("Figure 7: The temporal pattern of adjusted r-squared for the multiproxy example. The temporal pattern shows similar result than for the example with MVA only. Therefore, the highest percentage of (adjusted) explained variance is most likely related to MVA variable."), message=FALSE, warning=FALSE, include=TRUE----
plot(example_multiproxy, type = 2)

## ---- results = 'hide'--------------------------------------------------------
# Load the dendroTools R package
library(dendroTools)

# Load data
data(example_proxies_individual)
data(LJ_daily_temperatures)

# Example PCA
example_PCA <- daily_response(response = example_proxies_individual, 
                              env_data = LJ_daily_temperatures, method = "lm", 
                              lower_limit = 64, upper_limit = 66, metric = "adj.r.squared",
                              row_names_subset = TRUE, remove_insignificant = TRUE,
                              alpha = 0.001, PCA_transformation = TRUE,
                              components_selection = "manual", N_components = 2)

## -----------------------------------------------------------------------------
# Get the summary statistics for the PCA
summary(example_PCA$PCA_output)

## ---- fig.align='center', warning=FALSE, fig.width=10, fig.height=8, fig.cap=paste("Figure 8: The temporal pattern for the r-squared. The highest coefficients of determination were calculated for DOY around 90 with time span of two months."), message=FALSE, warning=FALSE, include=TRUE----

plot(example_PCA, type = 2)

## ---- results = 'hide'--------------------------------------------------------
# Load the dendroTools R package
library(dendroTools)

# Load data
data(data_TRW)
data(KRE_daily_temperatures)

example_reconstruction_lin <- daily_response(response = data_TRW, 
                                             env_data = KRE_daily_temperatures, 
                                             method = "lm", metric = "r.squared", 
                                             lower_limit = 38, upper_limit = 42,
                                             row_names_subset = TRUE, 
                                             temporal_stability_check = "progressive",
                                             cross_validation_type = "randomized", k = 3)

## ---- fig.align='center', warning=FALSE, fig.width=10, fig.height=8, fig.cap=paste("Figure 9: The highest r squared was calculated for the period from May 15 to June 27. The aggregated (averaged) daily data for this period is saved in a data frame $optimized_return."), message=FALSE, warning=FALSE, include=TRUE----
plot(example_reconstruction_lin)

## -----------------------------------------------------------------------------
example_reconstruction_lin$temporal_stability
example_reconstruction_lin$cross_validation

## ---- fig.align='center', warning=FALSE, fig.width=10, fig.height=8, fig.cap=paste("Figure 10: Linear transfer function"), message=FALSE, warning=FALSE, include=TRUE----
example_reconstruction_lin$transfer_function

## -----------------------------------------------------------------------------
linear_model <- lm(Optimized_return ~ TRW, data = example_reconstruction_lin$optimized_return)
reconstruction <- data.frame(predictions = predict(linear_model, newdata = data_TRW))

## ---- fig.align='center', warning=FALSE, fig.width=10, fig.height=8, fig.cap=paste("Figure 11: The reconstructed average temperature May 15 - June 27 with linear model"), message=FALSE, warning=FALSE, include=TRUE----
plot(row.names(data_TRW), reconstruction$predictions, type = "l", xlab = "Year", ylab = "Mean temperature May 15 - Jun 27 [ºC]")

## ---- results = 'hide'--------------------------------------------------------

# Example with precipitation and temperatures
partial_cor <- daily_response_seascorr(response = swit272,
                    env_data_primary = swit272_daily_temperatures,
                    env_data_control = swit272_daily_precipitation,
                    row_names_subset = TRUE, fixed_width = 45,
                    remove_insignificant = TRUE, alpha = 0.05, 
                    aggregate_function_env_data_primary = 'mean',
                    aggregate_function_env_data_control = 'sum',
                    pcor_method = "spearman",
                    boot = FALSE, reference_window = "end")


## ---- fig.align='center', warning=FALSE, fig.width=10, fig.height=8, fig.cap=paste("Figure 12: Temporal partern of partial climate-growth correlations calculated with daily_response_seascorr()"), message=FALSE, warning=FALSE, include=TRUE----
summary(partial_cor)
plot(partial_cor, type = 2)


