#' calculate_mean_scores
#'
#' Convert individual items to a mean score.
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @param data A nested data frame.
#' @return A nested data frame of data_raw + new scored data column.
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
#' # run simulation
#' results <- 
#'   generate_data(pop_model_label = "covariate",
#'                 pop_model = population_model, 
#'                 n = 100, 
#'                 iterations = 15) |>
#'   data_preprocessing(method = convert_to_likert) |>
#'   data_processing(method = calculate_mean_scores)
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



