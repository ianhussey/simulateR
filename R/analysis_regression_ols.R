#' analysis_regression_ols
#'
#' Fit an Ordinary Least Squares linear regression to the data using the model Y ~ X and extract the beta estimate, its 95% CIs and p values.
#' 
#' One use case for this package is to compare how differently specified models fit to a given data generating model (e.g., generating collider model data and fitting simple regression, mediation, collider, and confound models to it). Those other models will be fit using lavaan (and therefore maximum-likelihood estimation over OLS). In order to compare like with like, it may therefore be more appropriate to use analysis_regression_ml() for such a simulation. Other use cases may prefer a simple OLS regression. I therefore include both OLS/lm() and ML/lavaan::sem() implementations of bivariate regression. 
#'
#' @import stats
#' @import utils
#' @import graphics
#' @import broom
#' @import dplyr
#' @import tidyr
#' 
#' @param data A nested data frame containing the processed data.
#'
#' @return Results of analysis.
#' 
#' @examples
#' # population model
#' population_model <-
#'   create_population_model_with_static_item_loadings(
#'     model_specification = 
#'       "Y_latent ~ 0.5*X_latent + 0.5*M_latent; 
#'        X_latent ~~ 0.5*M_latent",
#'     item_loading_y = 0.8,
#'     item_loading_x = 0.7,
#'     item_loading_m = 0.6,
#'     n_indicators_y = 10,
#'     n_indicators_x = 8,
#'     n_indicators_m = 6
#'   )
#' 
#' # run a simulation
#' results <- 
#'   generate_data(pop_model_label = "covariate indicators",
#'                 pop_model = population_model, 
#'                 n = 100, 
#'                 iterations = 15) |>
#'   data_preprocessing(method = convert_to_likert) |>
#'   data_processing(method = calculate_mean_scores) |>
#'   fit_model(analysis = analysis_regression_ols)
#' 
#' @export
analysis_regression_ols <- function(data){
  
  # fit model
  results <- 
    broom::tidy(lm(Y ~ X, data = data)) |>
    dplyr::filter(term == "X") |>
    mutate(Y_X_ci_lower = estimate - std.error*1.96,
           Y_X_ci_upper = estimate + std.error*1.96) |>
    rename(Y_X_estimate = estimate,
           Y_X_pvalue = p.value) |>
    mutate(model_type = "regression (OLS)",
           model = "Y ~ X",
           decision_regression_ols = ifelse(Y_X_pvalue < 0.05, TRUE, FALSE)) |>
    select(model_type,
           model,
           Y_X_estimate,
           Y_X_pvalue,
           Y_X_ci_lower,
           Y_X_ci_upper,
           decision_regression_ols)
  
  return(results)
  
}




