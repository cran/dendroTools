## ----results='hide', message=FALSE, warning=FALSE-----------------------------
# Load packages
library(dendroTools)
library(ggplot2)

# Load example data
data(data_MVA)
data(LJ_daily_temperatures)

# Run daily_response()
example_basic <- daily_response(response = data_MVA,
                                env_data = LJ_daily_temperatures, 
                                row_names_subset = TRUE,
                                lower_limit = 35, upper_limit = 45, 
                                remove_insignificant = FALSE,
                                previous_year = FALSE, 
                                reference_window = "end")

## ----fig.align='center', fig.width=10, fig.height=8, fig.cap=paste("Figure 1: Default dendroTools plot for daily_response() output."), message=FALSE, warning=FALSE, include=TRUE----
plot(example_basic)

## ----fig.align='center', fig.width=10, fig.height=8, fig.cap=paste("Figure 2: The same plot modified with ggplot2 layers (scale + theme)."), message=FALSE, warning=FALSE, include=TRUE----
plot(example_basic) +
  scale_fill_gradient2(
    name = "cor",
    low = "blue",
    mid = "white",
    high = "red",
    na.value = "white",
    limits = c(-1, 1) # select min-max here
  ) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        plot.title = element_blank(),
        legend.position = "bottom"
        )

## ----fig.align='center', fig.width=10, fig.height=8, fig.cap=paste("Figure 3: Example with modified labels and rotated x-axis text."), message=FALSE, warning=FALSE, include=TRUE----
plot(example_basic) +
  scale_fill_gradient2(
    name = "Correlation",
    low = "blue",
    mid = "white",
    high = "red",
    na.value = "white",
    limits = c(-1, 1)
  ) +
  labs(x = "Season end (DOY)",
       y = "Season length (days)") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
# Extract calculations (correlation table) from the dmrs object
cor_mat <- example_basic$calculations

# Convert matrix-like object to long format using base R
melted <- as.data.frame(as.table(as.matrix(cor_mat)))
colnames(melted) <- c("season_length", "season_end", "value")

# Convert labels such as "X35" into numeric values (if present)
melted$season_end  <- as.numeric(gsub("X", "", melted$season_end))
melted$season_length <- as.numeric(gsub("X", "", melted$season_length))

# Remove NA values (if any)
melted <- melted[!is.na(melted$value), ]

summary(melted)

## ----fig.align='center', fig.width=10, fig.height=8, fig.cap=paste("Figure 4: Heatmap created from scratch using ggplot2 and extracted calculations."), message=FALSE, warning=FALSE, include=TRUE----
ggplot(melted, aes(x = season_end, y = season_length, fill = value)) + 
  geom_tile() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradient2(
    name = "cor",
    low = "blue",
    mid = "white",
    high = "red",
    na.value = "white",
    limits = c(-1, 1)
  ) +
  xlab("Season end") +
  ylab("Season Length") +
  theme_bw() +
  theme(legend.position = "bottom")

