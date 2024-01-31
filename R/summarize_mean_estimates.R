#' summarize_mean_estimates
#'
#' Summarize the mean estimates across across iterations of a fitted regression analysis (simple regression, confounder, mediation, collider, or correlational).
#'
#' @import stats
#' @import utils
#' @import graphics
#' @import dplyr
#' 
#' @param nested_results A nested data frame.
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
#'   fit_model(analysis = analysis_correlations) |>
#'   summarize_mean_estimates()
#'  
#' @export
summarize_mean_estimates <- function(nested_results) {
  
  results <- nested_results |>
    select(pop_model_label, fit) |>
    mutate(fit = map(fit, ~select(., ends_with("_estimate")))) |> 
    unnest(fit) |>
    group_by(pop_model_label) |>
    summarise(across(contains("_estimate"), mean, .names = "mean_{.col}")) |>
    round_df(2)
  
  return(results)
  
}

# TODO add estimates of dispersion too





  


