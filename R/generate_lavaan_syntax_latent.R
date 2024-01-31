#' generate_lavaan_syntax_latent
#'
#' Generate lavaan syntax for latent variables.
#' 
#' @import stats
#' @import utils
#' @import graphics
#' 
#' @param variable_name name of variable(s) as it appears in the data frame
#' @param item_loading factor loading(s)
#' @param n_indicators number of indicators per variable
#'
#' @return A nested data frame of data_raw + new scored data column.
#' 
#' @export
generate_lavaan_syntax_latent <- function(variable_name, item_loading, n_indicators){
  
  paste(variable_name, 
        "_latent =~ " , 
        paste(item_loading, 
              "*", 
              variable_name, 
              "_", 
              seq(1, n_indicators), 
              sep = "", 
              collapse = " + "), 
        sep = "")
  
}



