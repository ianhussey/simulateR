#' analysis_confounder
#'
#' Fit a confounder configural model (Y ~ X + M) to the data using linear regression and a Maximum Likelihood estimator. Extract the beta estimates, their 95% CIs and p values.
#'
#' @importFrom lavaan sem
#' @importFrom lavaan parameterEstimates
#' @import dplyr
#' @import tidyr
#' @param data A nested data frame containing the processed data.
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
    mutate(decision =  ifelse(Y_M_pvalue < 0.05 & 
                                X_M_pvalue < 0.05, TRUE, FALSE))
  
  return(results)
  
}
