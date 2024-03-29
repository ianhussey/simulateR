#' analysis_correlations
#'
#' Estimate correlations between X, M, and Y, as well as their 95% CIs and p values.
#'
#' @importFrom lavaan sem
#' @importFrom lavaan parameterEstimates
#' @importFrom stats cor.test 
#' @import dplyr
#' @import tidyr
#' @param data A nested data frame containing the processed data.
#' @return A nested data frame containing the fitted results. The "decision" variable refers to whether a statistically correlation between Y and X (Y ~~ X) was found. This is somewhat arbitrary and could be rethought.
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
    #mutate(decision_correlation_Y_X = ifelse(Y_X_pvalue < 0.05, TRUE, FALSE)) |>
    mutate(decision = ifelse(Y_X_pvalue < 0.05, TRUE, FALSE)) |>
    select(Y_X_estimate,
           #Y_X_r,
           Y_X_pvalue,
           Y_X_ci_lower,
           Y_X_ci_upper,
           decision_correlation)
  
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
    #mutate(decision_correlation_Y_M = ifelse(Y_M_pvalue < 0.05, TRUE, FALSE)) |>
    select(Y_M_estimate,
           #Y_M_r,
           Y_M_pvalue,
           Y_M_ci_lower,
           Y_M_ci_upper)
           #decision_correlation_Y_M)
  
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
    #mutate(decision_correlation_M_X = ifelse(M_X_pvalue < 0.05, TRUE, FALSE)) |>
    select(M_X_estimate,
           #M_X_r,
           M_X_pvalue,
           M_X_ci_lower,
           M_X_ci_upper)
           #decision_correlation_M_X)
  
  results <- 
    bind_cols(results_Y_X, 
              results_Y_M,
              results_M_X) |>
    mutate(model_type = "correlations",
           model = "Y ~~ X; Y ~~ M; M ~~ X")
  
  return(results)
  
}




