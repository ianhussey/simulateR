#' heterogeneity_metrics_for_forest
#'
#' Create a string reporting tau2, I2, and H2 from a meta-analysis fitted in metafor.
#' 
#' @param fit An rma object returned by (\code{"metafor::rma()"} or (\code{"metafor::rma.mv()"}, i.e. a fitted meta analysis model.
#' @return A string reporting the heterogeneity metrics that can be reported in a base R forest plot.
#' @export
heterogeneity_metrics_for_forest <- function(fit) {
  
  if(!is.na(fit$I2) & !is.na(fit$H2)) {
    
    output <- 
      bquote(
        paste(
          "RE Model (", 
          tau^2, " = ", .(formatC(format(round(fit$tau2, 2), nsmall = 2))), 
          ", ", 
          italic('I')^"2", " = ", .(formatC(format(round(fit$I2, 1), nsmall = 1))),
          "%, ", 
          italic('H')^"2", " = ", .(formatC(format(round(fit$H2, 1), nsmall = 1))),
          ")"
        )
      )
    
  } else {
    
    output <- 
      bquote(
        paste(
          "RE Model (", 
          tau^2, " = ", .(formatC(format(round(fit$tau2, 2), nsmall = 2))), 
          ")"
        )
      )
    
  }
  
  return(output)
  
}

