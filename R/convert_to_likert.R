#' convert_to_likert
#'
#' Convert continuous raw data to interval Likert data.
#'
#' @import dplyr
#' @importFrom janitor round_half_up
#' 
#' @param data A nested data frame.
#'
#' @return A nested data frame of data_raw + new preprocessed data column.
#' 
#' @examples 
#' # specify population model
#' population_model <-
#'   create_population_model_with_static_item_loadings(
#'     model_specification = 
#'        "Y_latent ~ 0.0*X_latent + 0.0*M_latent; X_latent ~~ 0.0*M_latent",
#'     item_loading_y = 0.9,
#'     item_loading_x = 0.7,
#'     item_loading_m = 0.5,
#'     n_indicators_y = 10,
#'     n_indicators_x = 10,
#'     n_indicators_m = 10
#'   )
#' 
#' generated_data <-
#'   # generate data
#'   generate_data(pop_model_label = "covariate indicators",
#'                 pop_model = population_model,
#'                 n = 100,
#'                 iterations = 2) |>
#'   # convert to likert
#'   data_preprocessing(method = convert_to_likert)
#' 
#' @export
convert_to_likert <- function(data) {
  
  results <- data |>
    # select only the indicators' data
    select(starts_with("Y"), starts_with("X"), starts_with("M")) |>
    # round continuous data
    mutate(across(everything(), ~ janitor::round_half_up(.))) |>
    # change any outliers to the max values
    mutate(across(everything(), ~ case_when(. < -3 ~ as.integer(-3), 
                                            . > +3 ~ as.integer(+3),
                                            TRUE ~ as.integer(.))))
  
  return(results)
  
}

