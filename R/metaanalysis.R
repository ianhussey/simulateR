#' metaanalysis
#'
#' Fit a random effects meta-analysis to the results across iterations. 
#'
#' @import tidyr
#' @import metafor
#' @importFrom stats sd
#' @param results Nested data frame containing results, as outputted by \code{"fit_model()"}.
#' @param published_only should the meta-analysis only include studied with the column "published" == TRUE? This column is created by \code{"publication_bias()"}.
#' @export
metaanalysis <- function(results, published_only = FALSE){
  
  if(published_only == TRUE){
    fit <- results |>
      tidyr::unnest(effect_sizes) |>
      dplyr::filter(published == TRUE) |>
      metafor::rma(yi = y,
                   sei = se,
                   data = _)
  } else {
    fit <- results |>
      tidyr::unnest(effect_sizes) |>
      metafor::rma(yi = y,
                   sei = se,
                   data = _)
  }
  
  return(fit)
}
