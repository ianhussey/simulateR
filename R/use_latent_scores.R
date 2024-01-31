#' use_latent_scores
#'
#' Extract the latent scores already calculated in a previous data generation step.
#'
#' @import dplyr
#' @param data A nested data frame.
#' @return A nested data frame of data_raw + new scored data column.
#' @examples
#' # population model
#' population_model <-
#'   create_population_model_with_random_item_loadings(
#'     model_specification = "Y_latent ~ 0.2*X_latent",
#'     item_loading_min_y = 0.5,
#'     item_loading_max_y = 0.9,
#'     n_indicators_y = 8,
#'   )
#' 
#' # run a simulation
#' results <- 
#'   generate_data(pop_model_label = "ttest indicators",
#'                 pop_model = population_model, 
#'                 factorial_design = TRUE,
#'                 n_mean = 100,
#'                 n_sd = 25,
#'                 iterations = 25) |>
#'   data_processing(method = use_latent_scores)
#' 
#' @export
use_latent_scores <- function(data){
  
  data |>
    select(contains("latent")) |>
    rename_with(~str_remove(., "_latent"))
  
}



