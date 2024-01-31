#' fit_model
#'
#' Apply an analysis function to the processed data such as \code{"analysis_ttest()"}.
#'
#' @import dplyr
#' @import furrr
#' @param nested_data A nested data frame containing a column named data_raw, which is nested data frames of raw data, produced by the data generation functions.
#' @param analysis the analysis function to be applied to data_raw, e.g., \code{"analysis_ttest()"}.
#' @return A nested data frame of data_processed + new fit column.
#' @export
fit_model <- function(nested_data, analysis){
  
  fits <- nested_data %>% 
    # mutate(correlations = furrr::future_map(data_processed, analysis_correlations),
    #        fit          = furrr::future_map(data_processed, analysis))
    mutate(fit = furrr::future_map(data_processed, analysis))
  
  return(fits)
  
}



