#' analysis_confounder
#'
#' Fit a confounder configural model (Y ~ X + M) to the data using linear regression and a Maximum Likelihood estimator. Extract the beta estimates, their 95% CIs and p values.
#'
#' @import stats
#' @import utils
#' @import graphics
#' @import lavaan
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
#'   fit_model(analysis = analysis_confounder)
#' 
#' @export
analysis_confounder <- function(data){
  
  # specify model
  model <-  '
            X ~ M
            Y ~ M + X
            '
  
  # fit model
  fit <- sem(model = model, data = data)

  # extract results
  results <- parameterEstimates(fit) |>
    dplyr::filter(op == "~") |>
    mutate(model_type = "confounder",
           model = paste(lhs, rhs, sep = "_")) |>
    select(model_type, 
           model, 
           pvalue, 
           estimate = est,
           ci_lower = ci.lower,
           ci_upper = ci.upper) |>
    pivot_wider(names_from = model,
                values_from = c(pvalue, estimate, ci_lower, ci_upper),
                names_glue = "{model}_{.value}") |>
    # mutate(confounder =  ifelse(Y_X_pvalue < 0.05 & 
    #                                Y_M_pvalue < 0.05 & 
    #                                X_M_pvalue < 0.05, TRUE, FALSE))
    mutate(decision_confounder =  ifelse(Y_M_pvalue < 0.05 & 
                                           X_M_pvalue < 0.05, TRUE, FALSE))
  
  return(results)
  
}