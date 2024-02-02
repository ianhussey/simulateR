#' convert_to_likert
#'
#' Convert continuous raw data to interval Likert-like data. Assumes simulated raw data has mean = 0 and SD = 1. Note that this is violated when generating factorials data, especially when the effect size is very large.
#'
#' @import dplyr
#' @importFrom janitor round_half_up
#' @param data A nested data frame.
#' @return A nested data frame of data_raw + new preprocessed data column.
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

