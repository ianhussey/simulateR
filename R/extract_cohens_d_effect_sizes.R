#' summarize_mean_estimates
#'
#' Extract the Cohen's d estimates from each iteration of a fitted t test analysis, e.g., for later meta analysis and/ forest plotting.
#'
#' @import stats
#' @import utils
#' @import graphics
#' @import dplyr
#' @import tidyr
#' @param nested_results A nested data frame.
#' @examples
#' set.seed(42)
#' 
#' true_population_effect_size <- 0.2
#' 
#' # population model
#' population_model <-
#'   create_population_model_with_random_item_loadings(
#'     model_specification = paste0("Y_latent ~ ", true_population_effect_size, "*X_latent"),
#'     item_loading_min_y = 0.5,
#'     item_loading_max_y = 0.9,
#'     n_indicators_y = 8,
#'   )
#' 
#' # run a simulation
#' results <- 
#'   generate_data(pop_model_label = "ttest indicators",
#'                 pop_model = population_model, 
#'                 factorial_design = TRUE,
#'                 #n = 50, 
#'                 n_mean = 100,
#'                 n_sd = 25,
#'                 iterations = 25) |>
#'   data_preprocessing(method = convert_to_likert) |>
#'   data_processing(method = use_latent_scores) |>
#'   fit_model(analysis = analysis_ttest) |>
#'   extract_cohens_d_effect_sizes()
#' 
#' # meta with no publication bias
#' fit <- results |>
#'   tidyr::unnest(effect_sizes) |>
#'   metafor::rma(yi = y,
#'                sei = se,
#'                data = _) 
#' 
#' # meta ESs
#' paste("true population effect size =", true_population_effect_size)
#' paste("meta effect size =", janitor::round_half_up(fit$beta[,1], 2))
#'  
#' @export
extract_cohens_d_effect_sizes <- function(nested_fits){
  
  results <- nested_fits |>
    mutate(effect_sizes = fit) |>
    unnest(effect_sizes) |>
    mutate(effect_size_extracted = "Cohen's d") |>
    rename(y        = Y_X_std_es_estimate,
           ci_lower = Y_X_std_es_ci_lower,
           ci_upper = Y_X_std_es_ci_upper) |>
    mutate(se = (ci_upper - ci_lower)/(1.96*2),
           conclusion = ifelse((ci_lower > 0 & ci_upper > 0) |
                                 (ci_lower < 0 & ci_upper < 0),
                               "significant",
                               "non-significant")) |>
    select(pop_model_label, pop_model, iteration, 
           data_raw, data_processed, fit, 
           effect_size_extracted, y, se, conclusion) |>
    nest(effect_sizes = c(effect_size_extracted, y, se, conclusion))
  
  return(results)
  
}
