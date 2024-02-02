#' publication_bias
#'
#' Create a boolean "published" column for each iteration (ie simulated study) based on the prior probability of statistically (non)significant results being published.
#' 
#' This requires that we choose values for the probability of publishing results given significant results and given non significant results. Several estimates of the prevalence of significant vs non-significant results in the literature exist (e.g., Motyl et al., 2017; Sterling et al, 1995) but these estimate estimate the probability of being significant given having been published (i.e., P(significant | published) ). 
#' 
#' Here we're interested in the opposite, P(published | significant) and P(published | nonsignificant). Anne Scheel suggests that Franco et al. (2014; 2016) provide estimates for these using a registered database of studies. However, these studies came from a registered database of studies that had been peer reviewed and preapproved prior to be run. There is a chance they are not representative, as they are therefore half-way to being a registered report, which may inflate the rate of published non-significant results. But in the absence of other data, these estimates are informative.
#' 
#' From Franco et al. (2014; 2016):
#' 
#' P(published | significant) = 57/93 = 0.61
#' P(published | nonsignificant) = 11/49 = 0.22
#' 
#' I use these as default values, but you can use your own. More extreme values might be more realistic, based on anecdotal experience (e.g., p_pub_sig = 0.70, p_pub_nonsig = 0.05)
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stats runif
#' @param nested_fits nested data frame with fitted models (i.e., results)
#' @param p_pub_sig probability of a significant result being labelled published = TRUE 
#' @param p_pub_nonsig probability of a non-significant result being labelled published = TRUE 
#' @param support_only determines whether results are more likely to be published if they are merely statistically significant (support_only = FALSE) or if they have to be significant and in the direction predicted by the theory (i.e., effect size > 0: support_only = TRUE).  
#' @return Data frame. The input data frame with a new column, published (logical) 
#' @export
publication_bias <- function(nested_fits, p_pub_sig = 0.61, p_pub_nonsig = 0.22, support_only = TRUE){
  
  if(support_only == TRUE){
    
    published <- nested_fits |>
      unnest(fit) |>
      mutate(p_pub = runif(n = n(), min = 0, max = 1),
             published = ifelse((decision == TRUE & Y_X_estimate > 0 & p_pub >= (1 - p_pub_sig)) |
                                  (decision == FALSE & Y_X_estimate > 0 & p_pub >= (1 - p_pub_nonsig)), TRUE, FALSE)) |>
      select(iteration, published)
    
  } else if (support_only == FALSE){
    
    published <- nested_fits |>
      unnest(fit) |>
      mutate(p_pub = runif(n = n(), min = 0, max = 1),
             published = ifelse((decision == TRUE & p_pub >= (1 - p_pub_sig)) |
                                  (decision == FALSE & p_pub >= (1 - p_pub_nonsig)), TRUE, FALSE)) |>
      select(iteration, published)
    
  } else {
    stop("support_only must be either TRUE or FALSE")
  }
  
  results_published <- left_join(nested_fits, published, by = "iteration")
  
  return(results_published)
  
}

  
