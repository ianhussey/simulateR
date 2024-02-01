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
#' @param effect_sizes nested data frame with results
#' @param p_pub_sig probability of a significant result being labelled published = TRUE 
#' @param p_pub_nonsig probability of a non-significant result being labelled published = TRUE 
#' @return A data frame or tibble.
#' @export
publication_bias <- function(effect_sizes, p_pub_sig = 0.61, p_pub_nonsig = 0.22){
  
  results <- effect_sizes |>
    mutate(effect_sizes_with_publication_bias = effect_sizes) |>
    unnest(effect_sizes_with_publication_bias) |>
    mutate(p_pub = runif(n = n(), min = 0, max = 1),
           published = ifelse((conclusion == "significant" & p_pub >= (1 - p_pub_sig)) |
                                (conclusion == "non-significant" & p_pub >= (1 - p_pub_nonsig)), TRUE, FALSE)) |>
    select(pop_model_label, pop_model, iteration, data_raw, data_processed, fit, 
           y, se, conclusion, p_pub, published) |>
    nest(effect_sizes = c(y, se, conclusion, p_pub, published))
  
  return(results)
  
}


