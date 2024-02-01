#' analysis_mediation
#'
#' Fit a mediation configural model (Y ~ X + M; M ~ X) to the data using linear regression and a Maximum Likelihood estimator. Extract the beta estimates, their 95% CIs and p values.
#'
#' @importFrom lavaan sem
#' @importFrom lavaan parameterEstimates
#' @import dplyr
#' @import tidyr
#' @param data A nested data frame containing the processed data.
#' @export
analysis_mediation <- function(data){
  
  # specify model
  model <-  '
            M ~ a*X
            Y ~ b*M + c*X
            indirect := a*b
            direct := c
            total := c + (a*b)
            proportion_mediated := indirect / total
            '
  
  # fit model
  fit <- sem(model = model, data = data)
  
  # extract results
  results <- parameterEstimates(fit) |>
    dplyr::filter(op %in% c("~", ":=")) |>
    mutate(model_type = "mediation",
           model = paste(lhs, rhs, sep = "_"),
           model = case_when(
             model == "indirect_a*b" ~ "indirect",
             model == "direct_c" ~ "direct",
             model == "total_c+(a*b)" ~ "total",
             model == "proportion_mediated_indirect/total" ~ "proportion",
             TRUE ~ model
           )) |>
    select(model_type, 
           model, 
           pvalue, 
           estimate = est,
           ci_lower = ci.lower,
           ci_upper = ci.upper) |>
    pivot_wider(names_from = model,
                values_from = c(pvalue, estimate, ci_lower, ci_upper),
                names_glue = "{model}_{.value}") |>
    mutate(decision               = ifelse(indirect_pvalue < 0.05, TRUE, FALSE))
           #decision_2             = ifelse(direct_pvalue < 0.05, TRUE, FALSE), # not checked
           #decision_mediation_Y_X = ifelse(Y_X_pvalue < 0.05, TRUE, FALSE),
           #decision_mediation_Y_M = ifelse(Y_M_pvalue < 0.05, TRUE, FALSE))
  
  return(results)
  
}




