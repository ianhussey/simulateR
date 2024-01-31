#' data_preprocessing
#'
#' Apply a data processing function to the raw data.
#'
#' @import stats
#' @import utils
#' @import graphics
#' @import dplyr
#' @import furrr
#' 
#' @param nested_data A nested data frame containing a column named data_raw, which is nested data frames of raw data, produced by the data generation functions.
#' @param method the preprocessing function to be applied to data_raw, e.g., \code{"convert_to_likert()"}.
#'
#' @return A nested data frame of data_raw + new preprocessed data column.
#' 
#' @export
data_preprocessing <- function(nested_data, method){

  nested_data |>
    mutate(data_raw = furrr::future_map(data_raw, method))
  
}

