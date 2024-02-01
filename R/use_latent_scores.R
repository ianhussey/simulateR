#' use_latent_scores
#'
#' Extract the latent scores already calculated in a previous data generation step.
#'
#' @import dplyr
#' @importFrom stats sd
#' @param data A nested data frame.
#' @return A nested data frame of data_raw + new scored data column.
#' @export
use_latent_scores <- function(data){
  
  data |>
    select(contains("latent")) |>
    rename_with(~str_remove(., "_latent"))
  
}



