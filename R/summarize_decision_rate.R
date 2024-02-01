#' summarize_decision_rate
#'
#' Summarize the decision based on the p value for Y ~ X across iterations of a fitted analysis. Useful for power analyses.
#'
#' @import dplyr
#' @param nested_results A nested data frame.
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


