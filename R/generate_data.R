#' generate_data
#'
#' Generate simulated data using a given population model, n iterations, and sample size.
#' 
#' @importFrom janitor round_half_up
#' @import stringr
#' @import simstandard
#' @import dplyr
#' @import tidyr
#' @import future.apply
#' 
#' @param pop_model_label string label for the population model
#' @param pop_model string lavaan specification for the population model
#' @param iterations number of iterations aka simulated studies
#' @param n sample size per iteration (simulated study). Either n or both n_mean and n_sd must be specified but not both.
#' @param n_mean integer mean sample size per iteration (simulated study). Either n or both n_mean and n_sd must be specified but not both.
#' @param n_sd integer SD sample size per iteration (simulated study). Either n or both n_mean and n_sd must be specified but not both.
#' @param factorial_design boolean indicating if data should be generated for a single group cross sectional design or a two group factorial design.
#'
#' @return A nested data frame of data_raw + new scored data column.
#' 
#' @examples
#' # Example 1: Population is a covariate regression model. 
#' # Each sample from the population is of equal size.
#' population_model <-
#'   create_population_model_with_static_item_loadings(
#'     model_specification =
#'       "Y_latent ~ 0.5*X_latent + 0.0*M_latent;
#'        X_latent ~~ 0.0*M_latent",
#'     item_loading_y = 0.8,
#'     item_loading_x = 0.8,
#'     item_loading_m = 0.8,
#'     n_indicators_y = 10,
#'     n_indicators_x = 10,
#'     n_indicators_m = 10
#'   )
#' 
#' simulated_data <-
#'   generate_data(pop_model_label = "covariate indicators",
#'                 pop_model = population_model,
#'                 n = 100,
#'                 iterations = 50)
#'                 
#' # Example 2: Population is a between-groups difference in means. 
#' # Studies vary in their sample sizes.
#' population_model <-
#'   create_population_model_with_random_item_loadings(
#'     model_specification = "Y_latent ~ 0.25*X_latent",
#'     item_loading_min_y = 0.5,
#'     item_loading_max_y = 0.9,
#'     n_indicators_y = 8,
#'   )
#' 
#' # run a simulation
#' results <- 
#'   generate_data(pop_model_label = "ttest indicators",
#'                 pop_model = population_model, 
#'                 factorial_design = TRUE,
#'                 n_mean = 100,
#'                 n_sd = 25,
#'                 iterations = 25)
#' 
#' @export
generate_data <- function(pop_model_label, pop_model, iterations, 
                          n, n_mean, n_sd,
                          factorial_design = FALSE) {
  
  # set up vectors for loops
  iterations_vector <- seq(1:iterations)
  
  # specify n for each iteration
  if(missing(n) & !missing(n_mean) & !missing(n_sd)){
    
    n_per_iteraction <- 
      # sample numbers from normal population
      rnorm(n = iterations, mean = n_mean, sd = n_sd) |>
      # convert to integer
      janitor::round_half_up() 
    
    # replace values <10 with 10
    n_per_iteraction <- ifelse(n_per_iteraction < 10, 10, n_per_iteraction)
    
  } else if(!missing(n) & missing(n_mean) & missing(n_sd)){
    
    n_per_iteraction <- rep(n, times = iterations)
    
  } else {
    
    stop("You must specify either an integer value for n (i.e., fixed n used for all datasets) or values for both n_mean and n_sd (i.e., variable n per datasest, with specfied mean and SD [corrected to have a minimum n = 10]).")
    
  }
  
  if(factorial_design == TRUE){
    
    # extract beta value from model specification
    # this is very quick and dirty and could fail if the model isn't specified in the correct way for this specific analysis. I'm only using this hacky approach so that syntax can be shared between this and all the continuous IV models using lavaan syntax. Rather than put effort into extracting this correctly in all cases, there is a check that should catch many (but not all) cases of misspecification 
    
    # find the start of the regression model, and drop everything before it
    pop_model_regression <- str_split(pop_model, "Y_latent ~ ", simplify = TRUE)[,2]
    pop_model_regression <- str_split(pop_model_regression, "Y_latent =~ ", simplify = TRUE)[,1]
    
    # extract beta value from model 
    beta <- pop_model_regression |>
      # remove all non numbers (inc decimals)
      str_remove_all("[^0-9.-]") |>
      as.numeric()
    
    pop_model_latent <- str_split(pop_model, "Y_latent =~ ", simplify = TRUE)[,2]
    
    # same latent specification as pop_model, but regression is set to zero. Otherwise noise is introduced into Y~X other beyond sampling variance and the beta value.
    dummy_pop_model <- paste("Y_latent ~ 0.0*X \n Y_latent =~ ", pop_model_latent)
    
    results <- 
      future.apply::future_lapply(
        seq_along(iterations_vector), 
        function(i, ...){
          
          # if the numeric value extracted from the model is also a string found in the pop model specification, hopefully the beta value (and nothing else) was correctly extracted. if not, throw an error.
          if(str_detect(pop_model, as.character(beta))) {
            
            n_control      <- ceiling(n_per_iteraction[i]/2)
            n_intervention <- n_per_iteraction[i] - n_control
            
            results <- bind_rows(
              # simulate data for control group (X_latent = 0)
              sim_standardized(m = dummy_pop_model, 
                               n = n_control, 
                               errors = FALSE) |>
                mutate(X_latent = 0) |>
                select(X_latent, contains("Y")),
              # simulate data for control group (X_latent = 1)
              sim_standardized(m = dummy_pop_model, 
                               n = n_intervention,
                               errors = FALSE) |>
                # offset this condition by value of beta
                mutate(across(everything(), function(x){x + beta}), 
                       X_latent = 1) |>
                select(X_latent, contains("Y"))
            ) |>
              mutate(iteration = iterations_vector[[i]])
            
          } else {
            
            stop("Error: Population model is mispecified. The effect size for X was not parsed correctly from the pop_model string. Correct specification is 'Y ~ 0.5*X' for effect size = 0.5. No M value supported.")
            
          }
          
          return(results)
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
      select(pop_model_label, pop_model, iteration, data_raw = data)
    
    
  } else if (factorial_design == FALSE){
    # otherwise simply generate data with continuous IVs
    
    results <- 
      future.apply::future_lapply(
        seq_along(iterations_vector), 
        function(i, ...){
          sim_standardized(m = pop_model, 
                           n = n_per_iteraction[i], 
                           errors = FALSE) |>
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
      ungroup() |>
      select(pop_model_label, pop_model, iteration, data_raw = data)
    
  } else {
    
    stop("factorial_design must be set to TRUE or FALSE")
    
  }
  
  return(results)
  
}

