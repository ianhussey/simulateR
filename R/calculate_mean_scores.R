#' calculate_mean_scores
#'
#' Convert individual items to a mean score.
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @param data A nested data frame.
#' @return A nested data frame of data_raw + new scored data column.
#' @export
calculate_mean_scores <- function(data) {
  
  results <- data |>
    rownames_to_column(var = "temp_id") |>
    # select only the indicators' data
    select(temp_id, starts_with("Y"), starts_with("X"), starts_with("M")) |>
    select(!contains("latent") & !contains("meanscore")) |>
    # make longer
    pivot_longer(cols = -temp_id,
                 names_to = c("scale", "item"),
                 names_pattern = "(.)_(.)",
                 values_to = "score") |>
    # calculate means
    group_by(temp_id, scale) |>
    dplyr::summarize(mean_score = mean(score), .groups = "keep") |>
    ungroup() |>
    # make wider again
    pivot_wider(id_cols = temp_id,
                names_from = scale,
                #names_glue = "{scale}_meanscore",
                names_glue = "{scale}",
                values_from = mean_score) |>
    select(-temp_id)
  
  return(results)
  
}



