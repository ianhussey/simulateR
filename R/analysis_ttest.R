#' analysis_ttest
#'
#' Fit a t test to the data using the model Y ~ X and extract the estimated mean difference, the effects, 95% CIs and p values.
#'
#' @import broom
#' @import dplyr
#' @import tidyr
#' @import forcats
#' @import effsize
#' @importFrom stats sd
#' @importFrom stats t.test
#' @param data A nested data frame containing the processed data.
#' @return Results of analysis.
#' @examples
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
#'                 iterations = 100) |>
#'   data_preprocessing(method = convert_to_likert) |>
#'   data_processing(method = use_latent_scores) |>
#'   fit_model(analysis = analysis_ttest)
#' 
#' @export
analysis_ttest <- function(data){
  
  # test that X can be coerced to a factor with exactly two levels
  if(length(levels(as.factor(data$X))) == 2) {
    
    data_relevelled <- data |>
      # relevel X so that estimate has correct sign
      mutate(X = forcats::fct_relevel(as.factor(X), "1", "0"))
    
    results_ttest <- data_relevelled |>
      t.test(Y ~ X, data = _, var.equal = TRUE, alternative = "two.sided") |>
      broom::tidy() |>
      rename(Y_X_estimate = estimate,
             Y_X_pvalue = p.value,
             Y_X_ci_lower = conf.low,
             Y_X_ci_upper = conf.high) |>
      mutate(model_type = "t-test",
             model = "Y ~ X",
             decision_ttest = ifelse(Y_X_pvalue < 0.05, TRUE, FALSE)) |>
      select(model_type,
             model,
             Y_X_estimate,
             Y_X_pvalue,
             Y_X_ci_lower,
             Y_X_ci_upper,
             decision_ttest)
    
    fit_cohensd <- 
      effsize::cohen.d(Y ~ as.factor(X), data = data_relevelled)
    
    results_cohensd <- 
      tibble(Y_X_std_es_estimate = fit_cohensd$estimate,
             Y_X_std_es_ci_lower = fit_cohensd$conf.int[1],
             Y_X_std_es_ci_upper = fit_cohensd$conf.int[2],
             Y_X_std_es_variance = fit_cohensd$var)
    
    results <- bind_cols(results_ttest, results_cohensd)
    
  } else {
    
    results <- "X is not a factor with exactly two levels. t-test could not be applied to the data." 
    
  }
  
  return(results)
  
}




