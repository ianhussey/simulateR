#' round_df
#'
#' Round all numeric columns in a data frame to a given number of places using the round-half-up method, using \code{"janitor::round_half_up()"}.
#'
#' @import stats
#' @import utils
#' @import graphics
#' @import janitor
#' @import dplyr
#' @import tibble
#' 
#' @param x Data frame or tibble to be rounded.
#' @param digits Number of digits to round to.
#'
#' @return A data frame or tibble.
#' 
#' @examples
#' library(ggplot2)
#' 
#' mtcars |>
#'   tibble::rownames_to_column(var = "automobile") |>
#'   round_df()
#' 
#' @export
round_df <- function(x, digits = 3) {

  mutate_if(x, is.numeric, janitor::round_half_up, digits = digits)
  
}






