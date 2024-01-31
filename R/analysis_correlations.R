#' analysis_correlations
#'
#' Estimate correlations between X, M, and Y, as well as their 95% CIs and p values.
#'
#' @importFrom lavaan sem
#' @importFrom lavaan parameterEstimates
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
#'   fit_model(analysis = analysis_correlations)
#' 
#' @export
analysis_correlations <- function(data){
  
  # # old, no CIs
  # cors <- cor(data)
  # 
  # results <- 
  #   tibble(Y_X_r = cors[1,2],
  #          Y_M_r = cors[1,3],
  #          X_M_r = cors[2,3])
  
  results_Y_X <- 
    broom::tidy(cor.test(x = data$Y,
                         y = data$X,
                         alternative = "two.sided",
                         method = "pearson")) |>
    rename(Y_X_estimate = estimate,
           #Y_X_r        = estimate,
           Y_X_pvalue   = p.value,
           Y_X_ci_lower = conf.low,
           Y_X_ci_upper = conf.high) |>
    mutate(decision_correlation_Y_X = ifelse(Y_X_pvalue < 0.05, TRUE, FALSE)) |>
    select(Y_X_estimate,
           #Y_X_r,
           Y_X_pvalue,
           Y_X_ci_lower,
           Y_X_ci_upper,
           decision_correlation_Y_X)
  
  results_Y_M <- 
    broom::tidy(cor.test(x = data$Y,
                         y = data$M,
                         alternative = "two.sided",
                         method = "pearson")) |>
    rename(Y_M_estimate        = estimate,
           #Y_M_r        = estimate,
           Y_M_pvalue   = p.value,
           Y_M_ci_lower = conf.low,
           Y_M_ci_upper = conf.high) |>
    mutate(decision_correlation_Y_M = ifelse(Y_M_pvalue < 0.05, TRUE, FALSE)) |>
    select(Y_M_estimate,
           #Y_M_r,
           Y_M_pvalue,
           Y_M_ci_lower,
           Y_M_ci_upper,
           decision_correlation_Y_M)
  
  results_M_X <- 
    broom::tidy(cor.test(x = data$M,
                         y = data$X,
                         alternative = "two.sided",
                         method = "pearson")) |>
    rename(M_X_estimate = estimate,
           #M_X_r        = estimate,
           M_X_pvalue   = p.value,
           M_X_ci_lower = conf.low,
           M_X_ci_upper = conf.high) |>
    mutate(decision_correlation_M_X = ifelse(M_X_pvalue < 0.05, TRUE, FALSE)) |>
    select(M_X_estimate,
           #M_X_r,
           M_X_pvalue,
           M_X_ci_lower,
           M_X_ci_upper,
           decision_correlation_M_X)
  
  results <- 
    bind_cols(results_Y_X, 
              results_Y_M,
              results_M_X) |>
    mutate(model_type = "correlations",
           model = "Y ~~ X; Y ~~ M; M ~~ X")
  
  return(results)
  
}




