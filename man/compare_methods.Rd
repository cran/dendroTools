% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_methods.R
\name{compare_methods}
\alias{compare_methods}
\title{compare_methods}
\usage{
compare_methods(
  formula,
  dataset,
  k = 10,
  repeats = 2,
  optimize = TRUE,
  dataset_complete = NULL,
  BRNN_neurons = 1,
  MT_committees = 1,
  MT_neighbors = 5,
  MT_rules = 200,
  MT_unbiased = TRUE,
  MT_extrapolation = 100,
  MT_sample = 0,
  RF_ntree = 500,
  RF_maxnodes = 5,
  RF_mtry = 1,
  RF_nodesize = 1,
  seed_factor = 5,
  digits = 3,
  blocked_CV = FALSE,
  PCA_transformation = FALSE,
  log_preprocess = TRUE,
  components_selection = "automatic",
  eigenvalues_threshold = 1,
  N_components = 2,
  round_bias_cal = 15,
  round_bias_val = 4,
  n_bins = 30,
  edge_share = 0.1,
  MLR_stepwise = FALSE,
  stepwise_direction = "backward",
  methods = c("MLR", "BRNN", "MT", "RF"),
  tuning_metric = "RMSE",
  BRNN_neurons_vector = c(1, 2, 3),
  MT_committees_vector = c(1, 5, 10),
  MT_neighbors_vector = c(0, 5),
  MT_rules_vector = c(100, 200),
  MT_unbiased_vector = c(TRUE, FALSE),
  MT_extrapolation_vector = c(100),
  MT_sample_vector = c(0),
  RF_ntree_vector = c(100, 250, 500),
  RF_maxnodes_vector = c(5, 10, 20, 25),
  RF_mtry_vector = c(1),
  RF_nodesize_vector = c(1, 5, 10),
  holdout = NULL,
  holdout_share = 0.1,
  holdout_manual = NULL,
  total_reproducibility = FALSE
)
}
\arguments{
\item{formula}{an object of class "formula" (or one that can be coerced
to that class): a symbolic description of the model to be fitted.}

\item{dataset}{a data frame with dependent and independent variables as
columns and (optional) years as row names.}

\item{k}{number of folds for cross-validation}

\item{repeats}{number of cross-validation repeats. Should be equal or more
than 1}

\item{optimize}{if set to TRUE (default), the optimal values for the tuning
parameters will be selected in a preliminary cross-validation procedure}

\item{dataset_complete}{optional, a data frame with the full length of tree-ring
parameter, which will be used to reconstruct the climate variable specified
with the formula argument}

\item{BRNN_neurons}{number of neurons to be used for the brnn method}

\item{MT_committees}{an integer:  how many committee models (e.g.  boosting
iterations) should be used?}

\item{MT_neighbors}{how many, if any, neighbors should be used to correct the
model predictions}

\item{MT_rules}{an integer (or NA): define an explicit limit to the number of
rules used (NA let’s Cubist decide).}

\item{MT_unbiased}{a logical: should unbiased rules be used?}

\item{MT_extrapolation}{a number between 0 and 100: since Cubist uses linear models,
predictions can be outside of the outside of the range seen the training set. This
parameter controls how much rule predictions are adjusted to be consistent with the
training set.}

\item{MT_sample}{a number between 0 and 99.9:  this is the percentage of the dataset
to be randomly selected for model building (not for out-of-bag type evaluation)}

\item{RF_ntree}{number of trees to grow. This should not be set to too small
a number, to ensure that every input row gets predicted at least a few times}

\item{RF_maxnodes}{maximum number of terminal nodes trees in the forest can
have}

\item{RF_mtry}{number of variables randomly sampled as candidates at each
split}

\item{RF_nodesize}{minimum size of terminal nodes. Setting this number larger
causes smaller trees to be grown (and thus take less time).}

\item{seed_factor}{an integer that will be used to change the seed options
for different repeats.}

\item{digits}{integer of number of digits to be displayed in the final
result tables}

\item{blocked_CV}{default is FALSE, if changed to TRUE, blocked cross-validation
will be used to compare regression methods.}

\item{PCA_transformation}{if set to TRUE, all independent variables will be
transformed using PCA transformation.}

\item{log_preprocess}{if set to TRUE, variables will be transformed with
logarithmic transformation before used in PCA}

\item{components_selection}{character string specifying how to select the Principal
Components used as predictors.
There are three options: "automatic", "manual" and "plot_selection". If
parameter is set to automatic, all scores with eigenvalues above 1 will be
selected. This threshold could be changed by changing the
eigenvalues_threshold argument. If parameter is set to "manual", user should
set the number of components with N_components argument. If component
selection is se to "plot_selection", Scree plot will be shown and user must
manually enter the number of components used as predictors.}

\item{eigenvalues_threshold}{threshold for automatic selection of Principal Components}

\item{N_components}{number of Principal Components used as predictors}

\item{round_bias_cal}{number of digits for bias in calibration period. Effects
the outlook of the final ggplot  of mean bias for calibration data (element 3 of
the output list)}

\item{round_bias_val}{number of digits for bias in validation period. Effects
the outlook of the final ggplot of mean bias for validation data (element 4 of
the output list)}

\item{n_bins}{number of bins used for the histograms of mean bias}

\item{edge_share}{the share of the data to be considered as the edge (extreme) data.
This argument could be between 0.10 and 0.50. If the argument is set to 0.10, then
the 5 % of the maximal extreme values and 5 % of the minimal extreme values are
considered to be the edge data.}

\item{MLR_stepwise}{if set to TRUE, stepwise selection of predictors will be used
for the MLR method}

\item{stepwise_direction}{the mode of stepwise search, can be one of "both",
"backward", or "forward", with a default of "backward".}

\item{methods}{a vector of strings related to methods that will be compared. A full
method vector is methods = c("MLR", "BRNN", "MT", "RF").
To use only a subset of methods, pass a vector of methods that you would like to compare.}

\item{tuning_metric}{a string that specifies what summary metric will be used to select
the optimal value of tuning parameters. By default, the argument is set to "RMSE". It is
also possible to use "RSquared".}

\item{BRNN_neurons_vector}{a vector of possible values for BRNN_neurons argument optimization}

\item{MT_committees_vector}{a vector of possible values for MT_committees argument optimization}

\item{MT_neighbors_vector}{a vector of possible values for MT_neighbors argument optimization}

\item{MT_rules_vector}{a vector of possible values for MT_rules argument optimization}

\item{MT_unbiased_vector}{a vector of possible values for MT_unbiased argument optimization}

\item{MT_extrapolation_vector}{a vector of possible values for MT_extrapolation argument optimization}

\item{MT_sample_vector}{a vector of possible values for MT_sample argument optimization}

\item{RF_ntree_vector}{a vector of possible values for RF_ntree argument optimization}

\item{RF_maxnodes_vector}{a vector of possible values for RF_maxnodes argument optimization}

\item{RF_mtry_vector}{a vector of possible values for RF_mtry argument optimization}

\item{RF_nodesize_vector}{a vector of possible values for RF_nodesize argument optimization}

\item{holdout}{this argument is used to define observations, which are excluded
from the cross-validation and hyperparameters optimization. The holdout argument must be
a character with one of the following inputs: “early”, “late” or “manual”. If
"early" or "late" characters are specified, then the early or late years will be
used as a holdout data. How many of the "early" or "late" years are used as a holdout
is specified with the argument holdout_share. If the argument holdout is set to “manual”,
then supply a vector of years (or row names) to the argument holdout_manual. Defined
years will be used as a holdout. For the holdout data, the same statistical measures are
calculated as for the cross-validation. The results for holdout metrics are given in the
output element $holdout_results.}

\item{holdout_share}{the share of the whole dataset to be used as a holdout.
Default is 0.10.}

\item{holdout_manual}{a vector of years (or row names) which will be used as a holdout.
calculated as for the cross-validation.}

\item{total_reproducibility}{logical, default is FALSE. This argument ensures total
reproducibility despite the inclusion/exclusion of different methods. By default, the
optimization is done only for the methods, that are included in the methods vector. If
one method is absent or added, the optimization phase is different, and this affects
all the final cross-validation results. By setting the total_reproducibility = TRUE,
all methods will be optimized, even though they are not included in the methods vector
and the final results will be subset based on the methods vector. Setting the
total_reproducibility to TRUE will result in longer optimization phase as well.}
}
\value{
a list with 19 elements:
\enumerate{
 \item $mean_std - data frame with calculated metrics for the selected \\ regression methods. For each regression method and each calculated metric, mean and standard deviation are given
 \item $ranks - data frame with ranks of calculated metrics: mean rank and  share of rank_1 are given
 \item $edge_results - data frame with calculated performance metrics for the central-edge test. The central part of the data represents the calibration data, while the edge data, i.e. extreme values, represent the test/validation data. Different regression models are calibrated using the central data and validated for the edge (extreme) data. This test is particularly important to assess the performance of models for the predictions of the extreme data. The share of the edge (extreme) data is defined with the edge_share argument
 \item $holdout_results - calculated metrics for the holdout data
 \item $bias_cal - ggplot object of mean bias for calibration data
 \item $bias_val - ggplot object of mean bias for validation data
 \item $transfer_functions - ggplot or plotly object with transfer functions of methods
 \item $transfer_functions_together - ggplot or plotly object with transfer functions of methods plotted together
 \item $parameter_values - a data frame with specifications of parameters used for different regression methods
 \item $PCA_output - princomp object: the result output of the PCA analysis
 \item $reconstructions - ggplot object: reconstructed dependent variable based on the dataset_complete argument, facet is used to split plots by methods
 \item $reconstructions_together - ggplot object: reconstructed dependent variable based on the dataset_complete argument, all reconstructions are on the same plot
 \item $normal_QQ_cal - normal q-q plot for calibration data
 \item $normal_QQ_holdout - normal q-q plot for holdout data
 \item $normal_QQ_edge- normal q-q plot for edge data
 \item $residuals_vs_fitted_cal - residuals vs fitted values plot for calibration data
 \item $residuals_vs_fitted_holdout - residuals vs fitted values plot for holdout data
 \item $residuals_vs_fitted_edge - residuals vs fitted values plot for edge data
 \item $reconstructions_data - raw data that is used for creating reconstruction plots
}
}
\description{
Calculates performance metrics for calibration (train) and validation (test)
data of different regression methods: multiple linear regression (MLR),
artificial neural networks with Bayesian regularization training
algorithm (BRNN), (ensemble of) model trees (MT) and random forest of regression
trees (RF). With the subset argument, specific methods of interest could be
specified. Calculated performance metrics are the correlation coefficient (r),
the root mean squared error (RMSE), the root relative squared error (RRSE),
the index of agreement (d), the reduction of error (RE), the coefficient of
efficiency (CE), the detrended efficiency (DE) and mean bias. For each of the
considered methods, there are also residual diagnostic plots available,
separately for calibration, holdout and edge data, if applicable.
}
\examples{
\donttest{

# The examples below are enclosed within donttest{} to minimize the execution
# time during R package checks. #'

# An example with default settings of machine learning algorithms
library(dendroTools)
library(ggplot2)

data(example_dataset_1)
data(dataset_TRW)

example_1 <- compare_methods(formula = MVA ~  T_APR,
dataset = example_dataset_1, k = 5, repeats = 1, BRNN_neurons = 1,
RF_ntree = 100, RF_mtry = 2, RF_maxnodes = 35, seed_factor = 5)

# example_1$mean_std
# example_1$ranks
# example_1$bias_cal
# example_1$transfer_functions
# example_1$transfer_functions_together
# example_1$PCA_output
# example_1$parameter_values

example_2 <- compare_methods(formula = MVA ~ .,
dataset = example_dataset_1, k = 2, repeats = 2,
methods = c("MLR", "BRNN", "MT"),
optimize = TRUE, MLR_stepwise = TRUE)
# example_2$mean_std
# example_2$ranks
# example_2$bias_val
# example_2$transfer_functions
# example_2$transfer_functions_together
# example_2$parameter_values

comparison_TRW <- compare_methods(formula = T_Jun_Jul ~ TRW, dataset = dataset_TRW,
k = 3, repeats = 5, optimize = FALSE, methods = c("MLR", "BRNN", "RF", "MT"),
seed_factor = 5, dataset_complete = dataset_TRW_complete, MLR_stepwise = TRUE,
stepwise_direction = "backward")

# comparison_TRW$mean_std
# comparison_TRW$bias_val
# comparison_TRW$transfer_functions
# comparison_TRW$reconstructions
# comparison_TRW$reconstructions_together
# comparison_TRW$edge_results
# comparison_TRW$reconstructions_data

}
}
\references{
Bishop, C.M., 1995. Neural Networks for Pattern Recognition. Oxford
University Press, Inc. 482 pp.

Breiman, L., 1996. Bagging predictors. Machine Learning 24, 123-140.

Breiman, L., 2001. Random forests. Machine Learning 45, 5-32.

Burden, F., Winkler, D., 2008. Bayesian Regularization of Neural Networks,
in: Livingstone, D.J. (ed.), Artificial Neural Networks: Methods and
Applications, vol. 458. Humana Press, Totowa, NJ, pp. 23-42.

Hastie, T., Tibshirani, R., Friedman, J.H., 2009. The Elements of
Statistical Learning : Data Mining, Inference, and Prediction, 2nd ed.
Springer, New York xxii, 745 p. pp.

Ho, T.K., 1995. Random decision forests, Proceedings of the Third
International Conference on Document Analysis and Recognition Volume 1.
IEEE Computer Society, pp. 278-282.

Hornik, K., Buchta, C., Zeileis, A., 2009. Open-source machine learning: R
meets Weka. Comput. Stat. 24, 225-232.

Perez-Rodriguez, P., Gianola, D., 2016. Brnn: Brnn (Bayesian Regularization
for Feed-forward Neural Networks). R package version 0.6.

Quinlan, J.R., 1992. Learning with Continuous Classes, Proceedings of the
5th Australian Joint Conference on Artificial Intelligence (AI '92). World
Scientific, Hobart, pp. 343-348.
}
