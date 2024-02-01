#' create_population_model_with_random_item_loadings
#'
#' Generate lavaan syntax for latent variables Y, X and M, and paste together with string specifying regression models among them. 
#' 
#' Instead of using the same item loading for all indicators, define a min and max for each scale and sample from a uniform distribution between these values.
#' 
#' @importFrom stats runif
#' @param model_specification string lavaan model specification
#' @param item_loading_min_y minimum factor loading for variable Y
#' @param item_loading_max_y maximum factor loading for variable Y
#' @param item_loading_min_x minimum factor loading for variable X
#' @param item_loading_max_x maximum factor loading for variable X
#' @param item_loading_min_m minimum factor loading for variable M
#' @param item_loading_max_m maximum factor loading for variable M
#' @param n_indicators_y number of indicators for varible Y
#' @param n_indicators_x number of indicators for varible X
#' @param n_indicators_m number of indicators for varible M
#' @return A nested data frame of data_raw + new scored data column.
#' @export
create_population_model_with_random_item_loadings <- function(model_specification,
                                                              item_loading_min_y = .99,
                                                              item_loading_max_y = .99,
                                                              item_loading_min_x = .99,
                                                              item_loading_max_x = .99,
                                                              item_loading_min_m = .99,
                                                              item_loading_max_m = .99,
                                                              n_indicators_y = 1, 
                                                              n_indicators_x = 1, 
                                                              n_indicators_m = 1){
  
  # check inputs
  if(min(c(item_loading_min_y, 
           item_loading_min_x,
           item_loading_min_m,
           item_loading_max_y,
           item_loading_max_x,
           item_loading_max_m) < 0.01 | 
         max(c(item_loading_min_y, 
               item_loading_min_x,
               item_loading_min_m,
               item_loading_max_y,
               item_loading_max_x,
               item_loading_max_m) > 0.99))) {
    stop("item loadings must be >= 0.01 and <= 0.99")
  }
  
  # generate item loadings
  item_loadings_y = 
    runif(n_indicators_y,
          min = item_loading_min_y,
          max = item_loading_max_y) |>
    round(2)
  
  item_loadings_x = 
    runif(n_indicators_x,
          min = item_loading_min_x,
          max = item_loading_max_x) |>
    round(2)
  
  item_loadings_m = 
    runif(n_indicators_m,
          min = item_loading_min_m,
          max = item_loading_max_m) |>
    round(2)
  
  # paste together model strings
  paste(
    generate_lavaan_syntax_latent(variable_name = "Y", 
                                  item_loading = item_loadings_y, 
                                  n_indicators = n_indicators_y),
    generate_lavaan_syntax_latent(variable_name = "X", 
                                  item_loading = item_loadings_x, 
                                  n_indicators = n_indicators_x),
    generate_lavaan_syntax_latent(variable_name = "M", 
                                  item_loading = item_loadings_m, 
                                  n_indicators = n_indicators_m),
    model_specification,
    sep = "; \n "
  )
  
}
