#' multiverse_plot
#'
#' Plot results from a simulation using a multiverse-style plot
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom cowplot plot_grid
#' @param data A data frame containing the simulation parameters, the outcome variable, and the column to rank the rows by.
#' @param outcome Column name in the data that represents the outcome variable.
#' @param outcome_name Label for the outcome variable in the plot.
#' @param interval_lower Column name in the data that represents the lower-bound interval. Defaults to NULL.
#' @param interval_upper Column name in the data that represents the upper-bound interval. Defaults to NULL.
#' @param rank_by Column name in the data that represents the variable that specifies the ranks order for the outcome (1 = first location on left of the plot).
#' @param rank_by_outcome If TRUE, create ranks by ranking the outcome column, therefore ignoring rank_by. Defaults to FALSE.
#' @param relative_height_of_upper_plot The height of the upper plot for the simulation outcomes relative to the conditions. Defaults to 0.70. May need to be increased if intervals are used.
#' @param outcome_cutoff If a numeric value is specified, a horizontal dashed line will be added to the outcomes plot at this value. This can be used to represent cut-off values, e.g., 80% power. Defaults to NULL.

#' @export
multiverse_plot <- function(data, 
                            outcome = "outcome", 
                            outcome_name = "Outcome",
                            interval_lower = NULL,
                            interval_upper = NULL,
                            rank_by = "rank",
                            rank_by_outcome = FALSE,
                            relative_height_of_upper_plot = 0.70, 
                            outcome_cutoff = NULL){

  # TODO consider checking inputs with rlang::ensym(variable)
  
  # rename outcome variable
  data <- data |>
    rename(outcome = {{outcome}},
           rank = {{rank_by}})
  
  # if intervals aren't specified, set them to the same value as outcome (so they are invisible in the plot)
  if(!is.null(interval_lower)){
    data <- data |>
      rename(interval_lower = {{interval_lower}})
  } else {
    data$interval_lower <- data$outcome
  }
  
  if(!is.null(interval_upper)){
    data <- data |>
      rename(interval_upper = {{interval_upper}})
  } else {
    data$interval_upper <- data$outcome
  }
  
  # if rank_by_outcome, rank by the outcome. otherwise, take order that was passed.
  if(rank_by_outcome){
    data <- data |>
      arrange(outcome) |>
      mutate(rank = row_number())
  }
  
  # ensure all columns other than rank and outcome are factors
  data <- data |>
    mutate(across(.cols = -c(rank, outcome, interval_lower, interval_upper),
                  .fns = as.factor))
  
  p_estimates <- data |>
    mutate(outcome_name = outcome_name) |>
    ggplot(aes(rank, outcome)) +
    geom_linerange(aes(ymin = interval_lower, ymax = interval_upper)) +
    geom_point(shape = "circle", size = 2) +
    facet_grid(outcome_name ~ ., space = "free_y", scales = "free_y", switch = "y") +
    scale_y_continuous(breaks = scales::pretty_breaks()) + 
    scale_x_continuous(NULL, expand = c(.02, .02)) +
    ylab("Simulation results") +
    #theme_classic() +
    theme_minimal() +
    theme(legend.position = "none",
          axis.line.x = element_blank(),
          strip.placement = "outside",
          strip.background = element_rect(fill = NA, colour = NA),
          panel.spacing.x = unit(0.15, "cm"),
          strip.text.y = element_markdown(angle = 180, face = "bold", size = 7),
          axis.text.y = element_text(angle = 0, 
                                     hjust = 0.5, 
                                     size = 6),
          axis.title.y = element_text(size = 9, face = "bold"),
          panel.spacing = unit(0.25, "lines"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  if(!is.null(outcome_cutoff)){
    p_estimates <- p_estimates + geom_hline(yintercept = outcome_cutoff, linetype = "dotted")
  }
  
  p_specs <- data |> 
    tidyr::pivot_longer(cols = c(-"rank", -"outcome", -"interval_lower", -"interval_upper")) |> 
    arrange(rank) |>
    ggplot(aes(x = rank, y = factor(value), color = name)) + 
    geom_point(size = 2, shape = "square") +
    facet_grid(name ~ ., space = "free_y", scales = "free_y", switch = "y") +
    guides(color = "none") +
    scale_x_continuous(NULL, expand = c(.02, .02)) +
    ylab("Simulation conditions") +
    #theme_classic() +
    theme_minimal() +
    theme(strip.placement = "outside",
          strip.background = element_rect(fill = NA, colour = NA),
          panel.spacing.x = unit(0.15, "cm"),
          strip.text.y = element_markdown(angle = 180, face = "bold", size = 7),
          axis.text.y = element_text(angle = 0, hjust = 1, size = 6),
          axis.title.y = element_text(size = 9, face = "bold"),
          panel.spacing = unit(0.25, "lines"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    scale_color_brewer(palette = "Dark2")
  
  p_multiverse <- cowplot::plot_grid(p_estimates, 
                                     p_specs, 
                                     axis = "bltr", 
                                     align = "v", 
                                     ncol = 1, 
                                     rel_heights = c(relative_height_of_upper_plot, 1))
  
  return(p_multiverse)
}
