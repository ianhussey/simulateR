#' data_processing
#'
#' Apply a data processing function to the raw data such as \code{"calculate_mean_scores()"}.
#'
#' @import stats
#' @import utils
#' @import graphics
#' @import dplyr
#' @import furrr
#' 
#' @param nested_data A nested data frame containing a column named data_raw, which is nested data frames of raw data, produced by the data generation functions.
#' @param method the processing function to be applied to data_raw, e.g., \code{"calculate_mean_scores()"} or \code{"use_latent_scores()"}.
#'
#' @return A nested data frame of data_raw + new processed data column.
#' 
#' @export
data_processing <- function(nested_data, method){

  nested_data %>% 
    mutate(data_processed = furrr::future_map(data_raw, method))
  
}



