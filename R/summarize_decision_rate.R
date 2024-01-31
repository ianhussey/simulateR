#' summarize_decision_rate
#'
#' Summarize the decision based on the p value for Y ~ X across iterations of a fitted analysis. Useful for power analyses.
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
#'   create_population_model_with_random_item_loadings(
#'     model_specification = "Y_latent ~ 0*X_latent", # true effect size of zero
#'     item_loading_min_y = 0.99,
#'     item_loading_max_y = 0.99,
#'     n_indicators_y = 8,
#'   )
#' 
#' # run simulations
#' results <- 
#'   generate_data(pop_model_label = "ttest indicators",
#'                 pop_model = population_model, 
#'                 factorial_design = TRUE,
#'                 n = 100, 
#'                 iterations = 1000) |>
#'   data_processing(method = use_latent_scores) |>
#'   fit_model(analysis = analysis_ttest)
#' 
#' # decision rate should equal the alpha value of the test when the true effect size is zero (.05)
#' results |>
#'   summarize_decision_rate()
#'  
#' @export
summarize_decision_rate <- function(nested_results) {
  
  results <- nested_results |>
    select(pop_model_label, fit) |>
    mutate(fit = map(fit, ~select(., starts_with("decision")))) |>
    unnest(fit) |>
    group_by(pop_model_label) |>
    summarize_all(.funs = mean) |>
    rename_with(~ str_replace(.x, 
                              pattern = "decision", 
                              replacement = "positive_rate")) |>
    round_df(2)
  
  return(results)
  
}


