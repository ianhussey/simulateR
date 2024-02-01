#' create_population_model_with_static_item_loadings
#'
#' Generate lavaan syntax for latent variables Y, X and M, and paste together with string specifying regression models among them.
#'
#' @import dplyr
#' @import tidyr
#' @param model_specification string lavaan model specification
#' @param item_loading_y factor loading for variable Y
#' @param item_loading_x factor loading for variable X
#' @param item_loading_m factor loading for variable M
#' @param n_indicators_y number of indicators for varible Y
#' @param n_indicators_x number of indicators for varible X
#' @param n_indicators_m number of indicators for varible M
#' @return A nested data frame of data_raw + new scored data column.
#' @export
create_population_model_with_static_item_loadings <- function(model_specification,
                                                              item_loading_y = .99, 
                                                              item_loading_x = .99, 
                                                              item_loading_m = .99,
                                                              n_indicators_y = 1, 
                                                              n_indicators_x = 1, 
                                                              n_indicators_m = 1){
  
  if(item_loading_y >= 1 | item_loading_x >= 1 | item_loading_m >= 1) stop("item loadings must be >= 0.01 and <= 0.99")
  
  paste(
    generate_lavaan_syntax_latent(item_loading = item_loading_y,
                                  variable_name = "Y",
                                  n_indicators = n_indicators_y),
    generate_lavaan_syntax_latent(item_loading = item_loading_x,
                                  variable_name = "X",
                                  n_indicators = n_indicators_x),
    generate_lavaan_syntax_latent(item_loading = item_loading_m,
                                  variable_name = "M",
                                  n_indicators = n_indicators_m),
    model_specification,
    sep = "; \n "
  )
  
}



