## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.align = "center")

## -----------------------------------------------------------------------------
library(dendroTools)

data(SEZ_rwl)

SEZ_rwi <- dplR::detrend(SEZ_rwl, method = "Spline", nyrs = 32, f = 0.5)
SEZ_chron <- dplR::chron(SEZ_rwi, prewhiten = TRUE)

# the residual chronology, without the first years lost to prewhitening
SEZ_res <- SEZ_chron[!is.na(SEZ_chron$res), "res", drop = FALSE]
range(as.numeric(row.names(SEZ_res)))

## -----------------------------------------------------------------------------
data(SEZ_daily_tmin)
data(SEZ_daily_tavg)
data(SEZ_daily_tmax)
data(SEZ_daily_psum)

# monthly means of temperature and monthly sums of precipitation
monthly_mean <- function(x) {
  data_transform(x, format = "monthly", monthly_aggregate_function = "mean")
}

monthly_tmin <- monthly_mean(SEZ_daily_tmin)
monthly_tavg <- monthly_mean(SEZ_daily_tavg)
monthly_tmax <- monthly_mean(SEZ_daily_tmax)
monthly_psum <- data_transform(SEZ_daily_psum, format = "monthly",
                               monthly_aggregate_function = "sum")

monthly_wb <- water_balance(prec = monthly_psum, tmin = monthly_tmin,
                            tmax = monthly_tmax, lat = 45.71)

# mean monthly water balance (mm)
round(colMeans(monthly_wb), 1)

## ----fig.width = 8, fig.height = 4, fig.cap = "Figure 1: Sums of the water balance and their SPEI values for the window from June to July and for August, one point per year."----
library(ggplot2)

monthly_spei_current <- monthly_response(response = SEZ_res,
                                         env_data = monthly_wb,
                                         row_names_subset = TRUE,
                                         aggregate_function = "SPEI",
                                         remove_insignificant = FALSE)

# windows are named by their first and last month
spei_values <- monthly_spei_current$aggregated_climate
analysed <- row.names(spei_values)

windows <- rbind(
  data.frame(window = "June to July",
             water_balance = rowSums(monthly_wb[analysed, 6:7]),
             SPEI = spei_values[, "6_7"]),
  data.frame(window = "August",
             water_balance = monthly_wb[analysed, 8],
             SPEI = spei_values[, "8_8"]))
windows$window <- factor(windows$window, levels = c("June to July", "August"))

ggplot(windows, aes(x = water_balance, y = SPEI)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(colour = "#2a78d6") +
  facet_wrap(~ window, scales = "free_x") +
  labs(x = "Water balance (mm)", y = "SPEI") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

## -----------------------------------------------------------------------------
spearman_spei <- monthly_response(response = SEZ_res, env_data = monthly_wb,
                                  row_names_subset = TRUE,
                                  aggregate_function = "SPEI",
                                  cor_method = "spearman",
                                  remove_insignificant = FALSE)

spearman_sum <- monthly_response(response = SEZ_res, env_data = monthly_wb,
                                 row_names_subset = TRUE,
                                 aggregate_function = "sum",
                                 cor_method = "spearman",
                                 remove_insignificant = FALSE)

all.equal(spearman_spei$calculations, spearman_sum$calculations)

# Pearson correlations: largest difference between SPEI and the sums
pearson_sum <- monthly_response(response = SEZ_res, env_data = monthly_wb,
                                row_names_subset = TRUE,
                                aggregate_function = "sum",
                                remove_insignificant = FALSE)

round(max(abs(monthly_spei_current$calculations - pearson_sum$calculations),
          na.rm = TRUE), 3)

## -----------------------------------------------------------------------------
monthly_spei <- monthly_response(response = SEZ_res,
                                 env_data = monthly_wb,
                                 row_names_subset = TRUE,
                                 aggregate_function = "SPEI",
                                 previous_year = TRUE,
                                 remove_insignificant = FALSE)

# the main rows of summary()
key_results <- function(x) {
  s <- summary(x)
  s[s$Variable %in% c("analysed_years", "maximal_calculated_metric",
                      "optimal_time_window"), ]
}

key_results(monthly_spei)

## ----fig.width = 11, fig.height = 5.5, fig.cap = "Figure 2: Correlations between the residual chronology of Sežana and the SPEI of monthly windows. A window is shown at its first month and its width in months; the months of the previous year come first."----
plot(monthly_spei, type = 2)

## -----------------------------------------------------------------------------
# daily layout: years as rows, days of year as columns
daily_tmin <- data_transform(SEZ_daily_tmin, format = "daily")
daily_tavg <- data_transform(SEZ_daily_tavg, format = "daily")
daily_tmax <- data_transform(SEZ_daily_tmax, format = "daily")
daily_psum <- data_transform(SEZ_daily_psum, format = "daily")

daily_wb <- water_balance(prec = daily_psum, tmin = daily_tmin,
                          tmax = daily_tmax, lat = 45.71,
                          tmean = daily_tavg)

daily_spei <- daily_response(response = SEZ_res,
                             env_data = daily_wb,
                             row_names_subset = TRUE,
                             aggregate_function = "SPEI",
                             lower_limit = 21,
                             upper_limit = 120,
                             remove_insignificant = FALSE)

summary(daily_spei)

## ----fig.width = 10, fig.height = 6, fig.cap = "Figure 3: Correlations between the residual chronology of Sežana and the SPEI of daily windows from 21 to 120 days, shown at the first day of each window."----
plot(daily_spei, type = 2)

## -----------------------------------------------------------------------------
monthly_early <- monthly_response(response = SEZ_res,
                                  env_data = monthly_wb,
                                  row_names_subset = TRUE,
                                  aggregate_function = "SPEI",
                                  previous_year = TRUE,
                                  remove_insignificant = FALSE,
                                  subset_years = c(1951, 1985))

monthly_late <- monthly_response(response = SEZ_res,
                                 env_data = monthly_wb,
                                 row_names_subset = TRUE,
                                 aggregate_function = "SPEI",
                                 previous_year = TRUE,
                                 remove_insignificant = FALSE,
                                 subset_years = c(1986, 2020))

# the SPEI values of the early period are those of the analysis of all years
early_years <- row.names(monthly_early$aggregated_climate)
all.equal(monthly_early$aggregated_climate,
          monthly_spei$aggregated_climate[early_years, ])

key_results(monthly_early)
key_results(monthly_late)

## -----------------------------------------------------------------------------
monthly_spei_detrended <- monthly_response(response = SEZ_res,
                                           env_data = monthly_wb,
                                           row_names_subset = TRUE,
                                           aggregate_function = "SPEI",
                                           previous_year = TRUE,
                                           remove_insignificant = FALSE,
                                           dc_method = "Spline",
                                           dc_difference = TRUE)

key_results(monthly_spei_detrended)

## -----------------------------------------------------------------------------
monthly_spei_pcor <- monthly_response_seascorr(
  response = SEZ_res,
  env_data_primary = monthly_wb,
  env_data_control = monthly_tavg,
  aggregate_function_env_data_primary = "SPEI",
  row_names_subset = TRUE,
  remove_insignificant = FALSE,
  pcor_control = TRUE)

s <- summary(monthly_spei_pcor)
s[s$Variable %in% c("maximal_calculated_metric", "optimal_time_window",
                    "maximal_calculated_metric_control",
                    "optimal_time_window_control",
                    "control_metric_in_optimal_time_window"), ]

monthly_spei_pcor$variance_partitioning

