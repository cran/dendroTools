## ---- echo = FALSE, warning = FALSE--------------------------------------
library(knitr)
dt <- data.frame(Element = c("$mean_std", "$std_ranks", "$edge_results", "$bias_cal", "$bias_val", "$transfer_functions", "$transfer_functions_together", "$parameter_values", "$PCA_output", "$reconstructions", "$reconstructions_together"), 
                 Element_description = c("data frame with calculated metrics for the selected regression methods. For each regression method and each calculated metric, mean and standard deviation are given", "data frame with ranks of calculated metrics: mean rank and  share of rank_1 are given", "data frame with calculated performance metrics for the central-edge test. The central part of the data represents the calibration data, while the edge data, i.e. extreme values, represent the edge data. Different regression models are calibrated using the central data and validated for the edge (extreme) data. This test is particularly important to assess the performance of models for the prediction of the extreme data. The share of the edge (extreme) data is defined with the edge_share argument", "ggplot object of mean bias for calibration data", "ggplot object of mean bias for validation data", "ggplot or plotly object with transfer functions of different methods, facet is used to separate methods", "ggplot or plotly object with transfer functions of methods plotted together", "a data frame with specifications of parameters used for different regression methods", "princomp object: the result output of the PCA analysis", "ggplot object: reconstructed dependent variable based on the dataset_complete argument, facet is used to split plots by methods", "ggplot object: reconstructed dependent variable based on the dataset_complete argument, all reconstructions are on the same plot"))
kable(dt, "html")


## ---- results = 'hide', warning=FALSE------------------------------------
# Load the dendroTools R package
library(dendroTools)

# Load the data
data(dataset_MVA)

# Basic example
basic_example <- compare_methods(formula = T_Apr ~ MVA, dataset = dataset_MVA, k = 10, repeats = 2, optimize = TRUE)

## ------------------------------------------------------------------------
# The data frame with mean and standard deviation of performance metrics for the calibration and the validation data
kable(basic_example$mean_std)

## ------------------------------------------------------------------------
# The data frame with non-parametric estimation of different methods: average rank and share of rank one
kable(basic_example$rank)

## ---- fig.align='center', warning=FALSE, fig.width=7, fig.height=5, fig.cap=paste("Histogram for the validation data for the basic_example")----
# See the histogram of mean bias for the validation data
basic_example$bias_val

## ---- fig.align='center', warning=FALSE, fig.width=7, fig.height=5, fig.cap=paste("Histogram for the calibration data for the basic_example")----
# See the histogram of mean bias for the calibration data
basic_example$bias_cal

## ---- fig.align='center', warning=FALSE, fig.width=7, fig.height=5, fig.cap=paste("The transfer functions of different methods, facet is used to separate plots by method.")----
# See the transfer functions, separated by facets. This is a ggplot object and could be easily customized. 
library(ggplot2)
basic_example$transfer_functions +   
  xlab(expression(paste('MVA [',mm^2,']'))) +
  ylab("April Mean Temperature [°C]")

## ---- fig.align='center', warning=FALSE, fig.width=7, fig.height=5, fig.cap=paste("The transfer functions of different methods, all functions are on the same plot, therefore it is easy to see the differences among different methods.")----
# See the transfer functions, plotted together. This is a ggplot object and could be easily customized. 
basic_example$transfer_functions_together +   
  xlab(expression(paste('MVA [',mm^2,']'))) +
  ylab("April Mean Temperature [°C]")

## ------------------------------------------------------------------------
# The data frame of optimized tuning parameters for different methods
basic_example$parameter_values

## ---- results = 'hide', warning=FALSE------------------------------------
# Load the dendroTools R package
library(dendroTools)

# Load data
data(dataset_MVA_individual)

# Example PCA
example_PCA <- compare_methods(formula = T_Apr ~ ., dataset = dataset_MVA_individual, k = 5, repeats = 2, optimize = TRUE, methods = c("MLR", "MT", "BRNN"), PCA_transformation = TRUE, components_selection = "manual", N_components = 2, seed_factor = 5)

## ------------------------------------------------------------------------
# Get the summary statistics for the PCA
summary(example_PCA$PCA_output)

## ------------------------------------------------------------------------
# The mean and standard deviation data frame 
kable(example_PCA$mean_std)

## ---- results = 'hide'---------------------------------------------------
# Load the dendroTools R package
library(dendroTools)

# Load data
data(example_dataset_1)

# Example multiproxy
example_multiproxy <- compare_methods(formula = MVA ~ T_APR + T_aug_sep, dataset = example_dataset_1, k = 10, repeats = 2, optimize = TRUE, methods = c("MT", "BMT", "RF"))

## ---- results = 'hide'---------------------------------------------------
# Load the dendroTools R package
library(dendroTools)

# Load the data
data(dataset_TRW)
data(dataset_TRW_complete)

# Example reconstruction
example_reconstruction <- compare_methods(formula = T_Jun_Jul ~ TRW, dataset = dataset_TRW, k = 3, optimize = TRUE, methods = c("MLR", "RF"), dataset_complete = dataset_TRW_complete)

## ---- fig.align='center', warning=FALSE, fig.width=7, fig.height=5, fig.cap=paste("The reconstructed June-July temperatures based on the dataset_complete argument, facet is used to split plots by methods.")----
example_reconstruction$reconstructions

## ---- fig.align='center', warning=FALSE, fig.width=7, fig.height=5, fig.cap=paste("The reconstructed June-July temperatures based on the dataset_complete argument, all reconstructions are on the same plot. The RF model reconstructed temperatures with much lower variance than the MLR model.")----
example_reconstruction$reconstructions_together

## ------------------------------------------------------------------------
# The central-edge test
example_reconstruction$edge_results

## ---- echo = FALSE, warning = FALSE--------------------------------------
library(knitr)
dt <- data.frame(Method = c("BRNN", "MT", "MT", "MT", "MT", "BMT", "BMT", "BMT", "BMT", "BMT", "BMT", "RF", "RF"),
                 Parameter = c("BRNN_neurons", "MT_M", "MT_N", "MT_U", "MT_R", "BMT_P", "BMT_I", "BMT_M", "BMT_N", "BMT_U", "BMT_R", "RF_P", "RF_I"), Vector_for_optimization = c("BRNN_neurons_vector", "MT_M_vector", "MT_N_vector", "MT_U_vector", "MT_R_vector", "BMT_P_vector", "BMT_I_vector", "BMT_M_vector", "BMT_N_vector", "BMT_U_vector", "BMT_R_vector", "RF_P_vector", "RF_I_vector"))
kable(dt, "html")


## ---- results = 'hide'---------------------------------------------------
# Load the dendroTools R package
library(dendroTools)

# Load the data
data(example_dataset_1)

example_optimize <- compare_methods(formula = MVA ~  T_APR, dataset = example_dataset_1, k = 5, repeats = 10, optimize = FALSE, BRNN_neurons = 1, MT_M = 4, MT_N = FALSE, MT_U = FALSE, MT_R = FALSE, BMT_P = 100, BMT_I = 100, BMT_M = 4, BMT_N = FALSE, BMT_U = FALSE, BMT_R = FALSE, RF_P = 100, RF_I = 100, RF_depth = 0, seed_factor = 5)

## ------------------------------------------------------------------------
# The data frame of tuning parameters, as defined by the user
example_optimize$parameter_values

