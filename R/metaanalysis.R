#' metaanalysis
#'
#' Fit a random effects meta-analysis to the results across iterations. 
#'
#' @import tidyr
#' @import metafor
#' @param results Nested data frame containing results, as outputted by \code{"fit_model()"}.
#' @examples
#' set.seed(42)
#' 
#' # population model
#' population_model <-
#'   create_population_model_with_random_item_loadings(
#'     model_specification = "Y_latent ~ 0.2*X_latent",
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
#' # fit meta analysis across iterations
#' metaanalysis(results)
#'  
#' @export
metaanalysis <- function(results, published_only = FALSE){
  
  if(published_only == TRUE){
    fit <- results |>
      tidyr::unnest(effect_sizes) |>
      dplyr::filter(published == TRUE) |>
      metafor::rma(yi = y,
                   sei = se,
                   data = _)
  } else {
    fit <- results |>
      tidyr::unnest(effect_sizes) |>
      metafor::rma(yi = y,
                   sei = se,
                   data = _)
  }
  
  return(fit)
}
