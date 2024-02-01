#' summarize_mean_estimates
#'
#' Summarize the mean estimates across across iterations of a fitted regression analysis (simple regression, confounder, mediation, collider, or correlational).
#'
#' @import dplyr
#' @param nested_results A nested data frame.
#' @export
summarize_mean_estimates <- function(nested_results) {
  
  results <- nested_results |>
    select(pop_model_label, fit) |>
    mutate(fit = map(fit, ~select(., ends_with("_estimate")))) |> 
    unnest(fit) |>
    group_by(pop_model_label) |>
    summarise(across(contains("_estimate"), mean, .names = "mean_{.col}")) |>
    round_df(2)
  
  return(results)
  
}

# TODO add estimates of dispersion too





  


