#' summarize_mean_estimates
#'
#' Extract the Cohen's d estimates from each iteration of a fitted t test analysis, e.g., for later meta analysis and/ forest plotting.
#' 
#' Note that this function extracts the *unstandardized* effect size on the assumption that the population model has an SD of 1.0. If this isn't the case
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stats sd
#' @param nested_fits A nested data frame.
#' @export
extract_cohens_d_effect_sizes <- function(nested_fits){
  
  effectsizes <- nested_fits |>
    unnest(fit) |>
    mutate(effect_size_extracted = "Cohen's d") |>
    rename(y        = Y_X_std_es_estimate,
           ci_lower = Y_X_std_es_ci_lower,
           ci_upper = Y_X_std_es_ci_upper) |>
    mutate(se = (ci_upper - ci_lower)/(1.96*2),
           significant = ifelse((ci_lower > 0 & ci_upper > 0) |
                                  (ci_lower < 0 & ci_upper < 0),
                                TRUE,
                                FALSE)) |>
    # select(pop_model_label, pop_model, iteration, 
    #        data_raw, data_processed, fit, 
    #        effect_size_extracted, y, se, significant) |>
    # select(-model_type,
    #        -model,
    #        -Y_X_estimate,
    #        -Y_X_pvalue,
    #        -Y_X_ci_lower,
    #        -Y_X_ci_upper,
    #        -decision,
    #        -ci_lower,
    #        -ci_upper,
    #        -Y_X_std_es_variance) |>
    select(iteration, effect_size_extracted, y, se, significant) |>
    nest(effect_sizes = c(effect_size_extracted, y, se, significant))
    
  nested_fits_effectsizes <- left_join(nested_fits, effectsizes, by = "iteration")
  
  return(nested_fits_effectsizes)
  
}

