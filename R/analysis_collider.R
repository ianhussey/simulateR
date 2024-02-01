#' analysis_collider
#'
#' Fit a collider configural model (M ~ X + Y; Y ~ X) to the data using linear regression and a Maximum Likelihood estimator. Extract the beta estimates, their 95% CIs and p values.
#'
#' @importFrom lavaan sem
#' @importFrom lavaan parameterEstimates
#' @import dplyr
#' @import tidyr
#' @param data A nested data frame containing the processed data.
#' @return Results of analysis.
#' @export
analysis_collider <- function(data){
  
  # specify model
  model <-  '
            M ~ X + Y
            Y ~ X
            '
  
  # fit model
  fit <- sem(model = model, data = data)
  
  # extract results
  results <- parameterEstimates(fit) |>
    dplyr::filter(op == "~") |>
    mutate(model_type = "collider",
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
    # mutate(collider =  ifelse(Y_X_pvalue < 0.05 & 
    #                             M_X_pvalue < 0.05 & 
    #                             M_Y_pvalue < 0.05, TRUE, FALSE))
    mutate(decision = ifelse(M_X_pvalue < 0.05 & M_Y_pvalue < 0.05, TRUE, FALSE))
  
  return(results)
  
}
