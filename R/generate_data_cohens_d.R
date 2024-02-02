#' generate_data_cohens_d
#'
#' Generate simulated data using a given population model, n iterations, and sample size.
#' 
#' To generate both item level data and latent scores, use a helper function (e.g., create_population_model_with_static_item_loadings() 
#' or create_population_model_with_random_item_loadings()) create the model specification which must include the assignment of indicators via "=~". 
#' To generate only the latent variables, specify only the regressions (e.g., "Y ~ 0.5*X + 0.0*M"). See the vignettes for examples. 
#' 
#' Note that for the later processing, hacking, and analysis functions to work, generate_data() must produce data with the correct variable names:
#' To generate item level data and latent variables, you must specify the population model to include variables "Y_latent", "X_latent", and (for some) "M_latent". 
#' To generate just the latent variables/scored data, you must specify the population model to include the variables "Y", "X", and (for some) "M" (i.e., no "_latent").
#' 
#' @import dplyr
#' @import tidyr
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace_all
#' @import future.apply
#' @importFrom stats rnorm
#' @param pop_model_label string label for the population model, e.g., "confounder indicators" for a confounder causal model with data for individual indicators (e.g., self report items) as well as the latent scores (e.g., sum scores)
#' @param pop_model string specifying population model using lavaan notation.  
#' @param iterations number of iterations aka simulated studies.
#' @param n1 sample size for group 1.
#' @param n2 sample size for group 2.
#' @param m1 population mean (mu) for group 1.
#' @param m2 population mean (mu) for group 2.
#' @param sd1 population SD (sigma) for group 1.
#' @param sd2 population SD (sigma) for group 2.
#' @param cohens_d the population standardized mean difference effect size (Cohen's d). You must only supply values for either (a) m1, m2, sd1, and sd2 OR (b) cohens_d but not both.
#' @return A nested data frame.
#' @export
generate_data_cohens_d <- function(pop_model_label, n1, n2, m1, m2, sd1, sd2, cohens_d = NULL, iterations) {
  
  # TODO add variable sample size by iteration here, eg using n_mean, n_sd
  
  
  # check that only either (m1, m2, sd1, sd2) or cohens_d was provided
  if(missing(m1) & missing(m2) & missing(sd1) & missing(sd2) & !missing(cohens_d)){
    sd1 <- sd2 <- 1
    m1 <- 0
    m2 <- m1 + (cohens_d * sd1)
  } else if(!missing(m1) & !missing(m2) & !missing(sd1) & !missing(sd2) & missing(cohens_d)){
    # continue
  } else {
    stop("You must supply values for either (a) m1, m2, sd1, and sd2 OR (b) cohens_d but not both.")
  }
  
  # set up vectors for loops
  iterations_vector <- seq(1:iterations)
  n_per_iteration <- rep(n, times = iterations)
  
  results <- 
    future.apply::future_lapply(
      seq_along(iterations_vector), 
      function(i, ...){
        
        # simulate data for the two groups
        data_group0 <- data.frame(Y = rnorm(n = n1, mean = m1, sd = sd1),
                                  X = 0)
        
        data_group1 <- data.frame(Y = rnorm(n = n2, mean = m2, sd = sd2),
                                  X = 1)
        
        # combine the two groups into one dataset
        data_combined <- rbind(data_group0, data_group1) |>
          mutate(iteration = iterations_vector[[i]])

        return(data_combined)
      },
      future.seed = TRUE
    ) |>
    dplyr::bind_rows() |>
    mutate(pop_model_label = pop_model_label,
           pop_model = pop_model |>
             str_remove("\n  ") |>
             str_replace_all("\n  ", "; ") |>
             str_remove("; $")) |>
    group_by(pop_model_label, pop_model, iteration) |>
    nest() |>
    ungroup() |>
    select(pop_model_label, pop_model, iteration, data_processed = data)
  
  return(results)
}

