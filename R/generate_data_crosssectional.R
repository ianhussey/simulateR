#' generate_data_crosssectional
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
#' @importFrom janitor round_half_up
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_detect
#' @import lavaan
#' @import dplyr
#' @import tidyr
#' @import future.apply
#' @importFrom stats rnorm
#' @importFrom stats sd
#' @param pop_model_label string label for the population model, e.g., "confounder indicators" for a confounder causal model with data for individual indicators (e.g., self report items) as well as the latent scores (e.g., sum scores)
#' @param pop_model string specifying population model using lavaan notation.  
#' @param iterations number of iterations aka simulated studies
#' @param n sample size per iteration (simulated study). Either n or both n_mean and n_sd must be specified but not both. 
#' @param n_mean integer mean sample size per iteration (simulated study). Either n or both n_mean and n_sd must be specified but not both. Note that generated values <6 will be changed to 6.
#' @param n_sd integer SD sample size per iteration (simulated study). Either n or both n_mean and n_sd must be specified but not both.
#' @return A nested data frame of data_raw + new scored data column.
#' @export
generate_data_crosssectional <- function(pop_model_label, pop_model, iterations, 
                                         n, n_mean, n_sd) {
  
  # set up vectors for loops
  iterations_vector <- seq(1:iterations)
  
  # specify n for each iteration
  if(missing(n) & !missing(n_mean) & !missing(n_sd)){
    
    n_per_iteration <- 
      # sample numbers from normal population
      rnorm(n = iterations, mean = n_mean, sd = n_sd) |>
      # convert to integer
      janitor::round_half_up() 
    
    # replace values <10 with 10
    n_per_iteration <- ifelse(n_per_iteration < 6, 6, n_per_iteration)
    
  } else if(!missing(n) & missing(n_mean) & missing(n_sd)){
    n_per_iteration <- rep(n, times = iterations)
  } else {
    stop("You must specify either an integer value for n (i.e., fixed n used for all datasets) or values for both n_mean and n_sd (i.e., variable n per datasest, with specfied mean and SD [corrected to have a minimum n = 10]).")
  }

  results <- 
    future.apply::future_lapply(
      seq_along(iterations_vector), 
      function(i, ...){
        # sim_standardized(m = pop_model, n = n_per_iteration[i], errors = FALSE) |>
        lavaan::simulateData(model = pop_model, sample.nobs = n_per_iteration[i]) |>
          mutate(iteration = iterations_vector[[i]])
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
    ungroup() 
  # |>
  #   select(pop_model_label, pop_model, iteration, data)
  
  # if the population model specified latent variables and their observed indicators, 
  # then assign the generated data to the data_raw column as it then needs processing prior to analysis.
  # if the population model did not specify any indicators, just latents, 
  # then assign the generated data to the data_processeed as it already contains the latent scores for analysis.
  if(str_detect(pop_model, "=~")){
    results <- results |>
      rename(data_raw = data)
  } else {
    results <- results |>
      rename(data_processed = data)
  }
  
  return(results)
  
}

