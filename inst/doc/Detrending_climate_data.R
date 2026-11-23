## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.align = "center")

## -----------------------------------------------------------------------------
library(dendroTools)

data(data_MVA)
data(LJ_monthly_temperatures)

years <- as.numeric(row.names(data_MVA))

# MVA
cor(data_MVA$MVA, years)

# mean temperature from March to November
cor(rowMeans(LJ_monthly_temperatures[row.names(data_MVA), 3:11]), years)

## -----------------------------------------------------------------------------
no_detrending <- monthly_response(response = data_MVA,
                                  env_data = LJ_monthly_temperatures,
                                  row_names_subset = TRUE,
                                  remove_insignificant = FALSE)

spline_detrending <- monthly_response(response = data_MVA,
                                      env_data = LJ_monthly_temperatures,
                                      row_names_subset = TRUE,
                                      remove_insignificant = FALSE,
                                      dc_method = "Spline",
                                      dc_difference = TRUE)

str(spline_detrending$detrending)

## ----fig.width = 7, fig.height = 4.5, fig.cap = "Figure 1: Mean March to November temperature in Ljubljana with the fitted spline (top), and the detrended series used in the correlations (bottom)."----
library(ggplot2)

window <- "3_11"  # from March (3) to November (11)
observed <- no_detrending$aggregated_climate[, window]
detrended <- spline_detrending$aggregated_climate[, window]
years <- as.numeric(row.names(no_detrending$aggregated_climate))

panels <- c("Mean temperature, March to November",
            "Detrended: observed minus spline")

figure_data <- data.frame(
  year = rep(years, 3),
  value = c(observed, observed - detrended, detrended),
  series = rep(c("observed", "spline", "observed"), each = length(years)),
  panel = factor(rep(panels[c(1, 1, 2)], each = length(years)), levels = panels)
)

ggplot(figure_data, aes(x = year, y = value, colour = series)) +
  geom_line() +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c(observed = "#2a78d6", spline = "#eb6834")) +
  labs(x = NULL, y = NULL, colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

## -----------------------------------------------------------------------------
# the highest correlation and its window, taken from summary()
optimum <- function(x) {
  s <- summary(x)
  data.frame(r = s$Value[s$Variable == "maximal_calculated_metric"],
             window = s$Value[s$Variable == "optimal_time_window"])
}

methods <- c("none", "SLD", "Spline", "Mean", "Ar", "Friedman")

comparison <- do.call(rbind, lapply(methods, function(m) {
  x <- monthly_response(response = data_MVA,
                        env_data = LJ_monthly_temperatures,
                        row_names_subset = TRUE,
                        remove_insignificant = FALSE,
                        dc_method = m, dc_difference = TRUE,
                        cor_na_use = "pairwise.complete.obs")
  data.frame(dc_method = m, optimum(x))
}))

knitr::kable(comparison)

## ----fig.width = 13, fig.height = 6, fig.cap = "Figure 2: Correlations between MVA and monthly temperatures for all windows, (a) without detrending and (b) after spline detrending. Each panel has its own colour scale."----
heatmap_a <- plot(no_detrending, type = 2) + labs(title = "a) No detrending")
heatmap_b <- plot(spline_detrending, type = 2) + labs(title = "b) Spline detrending")

# the two heatmaps side by side
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
print(heatmap_a, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(heatmap_b, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))

## -----------------------------------------------------------------------------
data(LJ_monthly_precipitation)

precipitation_spline <- monthly_response(response = data_MVA,
                                         env_data = LJ_monthly_precipitation,
                                         tidy_env_data = TRUE,
                                         aggregate_function = "sum",
                                         row_names_subset = TRUE,
                                         remove_insignificant = FALSE,
                                         dc_method = "Spline")

optimum(precipitation_spline)

## ----error = TRUE-------------------------------------------------------------
try({
monthly_response(response = data_MVA, env_data = LJ_monthly_temperatures,
                 row_names_subset = TRUE, dc_method = "Spline")
})

## -----------------------------------------------------------------------------
data(LJ_daily_temperatures)

mva_residual <- data_MVA
mva_residual$MVA <- dplR::detrend.series(data_MVA$MVA, method = "Ar",
                                         make.plot = FALSE)

daily_prewhitened <- daily_response(response = mva_residual,
                                    env_data = LJ_daily_temperatures,
                                    row_names_subset = TRUE,
                                    fixed_width = 60,
                                    remove_insignificant = FALSE,
                                    dc_method = "Ar",
                                    dc_difference = TRUE,
                                    cor_na_use = "pairwise.complete.obs")

daily_not_prewhitened <- daily_response(response = data_MVA,
                                        env_data = LJ_daily_temperatures,
                                        row_names_subset = TRUE,
                                        fixed_width = 60,
                                        remove_insignificant = FALSE)

rbind(not_prewhitened = optimum(daily_not_prewhitened),
      prewhitened = optimum(daily_prewhitened))

## -----------------------------------------------------------------------------
seascorr_detrended <- monthly_response_seascorr(
  response = data_MVA,
  env_data_primary = LJ_monthly_temperatures,
  env_data_control = LJ_monthly_precipitation,
  tidy_env_data_primary = FALSE,
  tidy_env_data_control = TRUE,
  row_names_subset = TRUE,
  remove_insignificant = FALSE,
  dc_method = "Spline", dc_difference = TRUE,
  dc_env_data_control = list(dc_difference = FALSE))

seascorr_detrended$detrending$primary$dc_difference
seascorr_detrended$detrending$control$dc_difference

optimum(seascorr_detrended)

