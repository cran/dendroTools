## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.align = "center")

## -----------------------------------------------------------------------------
library(dendroTools)

data(data_MVA)
data(LJ_monthly_temperatures)
data(LJ_monthly_precipitation)

monthly_pcor <- monthly_response_seascorr(
  response = data_MVA,
  env_data_primary = LJ_monthly_temperatures,
  env_data_control = LJ_monthly_precipitation,
  tidy_env_data_primary = FALSE,
  tidy_env_data_control = TRUE,
  aggregate_function_env_data_primary = "mean",
  aggregate_function_env_data_control = "sum",
  row_names_subset = TRUE,
  remove_insignificant = FALSE)

summary(monthly_pcor)

## -----------------------------------------------------------------------------
monthly_both <- monthly_response_seascorr(
  response = data_MVA,
  env_data_primary = LJ_monthly_temperatures,
  env_data_control = LJ_monthly_precipitation,
  tidy_env_data_primary = FALSE,
  tidy_env_data_control = TRUE,
  aggregate_function_env_data_control = "sum",
  row_names_subset = TRUE,
  remove_insignificant = FALSE,
  pcor_control = TRUE)

# the main rows of summary()
key_results <- function(x) {
  s <- summary(x)
  s[s$Variable %in% c("maximal_calculated_metric",
                      "optimal_time_window",
                      "maximal_calculated_metric_control",
                      "optimal_time_window_control",
                      "control_metric_in_optimal_time_window"), ]
}

key_results(monthly_both)

## ----fig.width = 13, fig.height = 6, fig.cap = "Figure 1: Partial correlations between MVA and (a) monthly temperature, controlling for precipitation, and (b) monthly precipitation, controlling for temperature. Each panel has its own colour scale."----
library(ggplot2)

# the heatmaps of the primary (type = 2) and the control variable (type = 3)
smaller_legend <- theme(legend.title = element_text(size = 14),
                        legend.text = element_text(size = 12))

grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
print(plot(monthly_both, type = 2) + smaller_legend +
        labs(title = "a) Temperature, controlling for precipitation"),
      vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot(monthly_both, type = 3) + smaller_legend +
        labs(title = "b) Precipitation, controlling for temperature"),
      vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))

## -----------------------------------------------------------------------------
monthly_both$variance_partitioning

## -----------------------------------------------------------------------------
monthly_fdr <- monthly_response_seascorr(
  response = data_MVA,
  env_data_primary = LJ_monthly_temperatures,
  env_data_control = LJ_monthly_precipitation,
  tidy_env_data_primary = FALSE,
  tidy_env_data_control = TRUE,
  aggregate_function_env_data_control = "sum",
  row_names_subset = TRUE,
  remove_insignificant = TRUE,
  remove_insignificant_method = "fdr_BH",
  pcor_control = TRUE)

monthly_fdr$significance_filter$criterion

# windows retained by the filter, out of all windows
c(primary = monthly_fdr$significance_filter$retained_cells,
  control = monthly_fdr$significance_filter_control$retained_cells,
  windows = sum(!is.na(monthly_both$calculations)))

## -----------------------------------------------------------------------------
data(LJ_daily_temperatures)
data(LJ_daily_precipitation)

daily_both <- daily_response_seascorr(
  response = data_MVA,
  env_data_primary = LJ_daily_temperatures,
  env_data_control = LJ_daily_precipitation,
  tidy_env_data_primary = FALSE,
  tidy_env_data_control = TRUE,
  aggregate_function_env_data_control = "sum",
  row_names_subset = TRUE,
  fixed_width = 45,
  remove_insignificant = FALSE,
  pcor_control = TRUE)

key_results(daily_both)
daily_both$variance_partitioning

## ----fig.width = 8, fig.height = 4.5, fig.cap = "Figure 2: Partial correlations between MVA and 45-day windows of temperature, controlling for precipitation, and of precipitation, controlling for temperature. The dashed lines show the critical partial correlation at p = 0.05."----
n_years <- nrow(daily_both$optimized_return)
t_critical <- stats::qt(0.975, df = n_years - 3)
r_critical <- t_critical / sqrt(n_years - 3 + t_critical ^ 2)

n_windows <- ncol(daily_both$calculations)
labels <- c("temperature, controlling for precipitation",
            "precipitation, controlling for temperature")

daily_lines <- data.frame(
  start = rep(seq_len(n_windows), 2),
  partial_r = c(daily_both$calculations[1, ],
                daily_both$calculations_control[1, ]),
  variable = factor(rep(labels, each = n_windows), levels = labels)
)

ggplot(daily_lines, aes(x = start, y = partial_r, colour = variable)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_hline(yintercept = c(-r_critical, r_critical), colour = "grey60",
             linetype = "dashed") +
  geom_line(na.rm = TRUE) +
  scale_colour_manual(values = c("#2a78d6", "#eb6834")) +
  labs(x = "First day of the window (day of year)",
       y = "Partial correlation", colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

## -----------------------------------------------------------------------------
monthly_detrended <- monthly_response_seascorr(
  response = data_MVA,
  env_data_primary = LJ_monthly_temperatures,
  env_data_control = LJ_monthly_precipitation,
  tidy_env_data_primary = FALSE,
  tidy_env_data_control = TRUE,
  aggregate_function_env_data_control = "sum",
  row_names_subset = TRUE,
  remove_insignificant = FALSE,
  dc_method = "Spline",
  dc_difference = TRUE,
  dc_env_data_control = list(dc_difference = FALSE),
  pcor_control = TRUE)

key_results(monthly_detrended)

