#' vector_to_histogram_with_summary_statistics
#'
#' Plot a numeric vector as a ggplot2 histogram with annotated sample mean and SD
#'
#' @import ggplot2
#' @import scales
#' @param simulated_scores numeric vector of values to be plotted
#' @param binwidth numeric binwidth argument for (\code{"ggplot2::geom_histogram()}.
#' @param xmin numeric the xmin argument for (\code{"ggplot2::scale_x_continuous()}, used in both the breaks and limits arguments
#' @param xmax numeric the xmax argument for (\code{"ggplot2::scale_x_continuous()}, used in both the breaks and limits arguments
#' @param x_prop numeric the x coordinate, in proportions of plot, where the annotation should appear
#' @param y_prop numeric the y coordinate, in proportions of plot, where the annotation should appear
#' @param fill string the fill color argument for (\code{"ggplot2::geom_histogram()}
#' @return A ggplot object  
#' @examples
#' library(ggplot2)
#' 
#' rnorm(n = 100000, # sample n
#'       mean = 0, # population mean (μ or mu)
#'       sd = 1) |> # population sd (σ or sigma)
#'   vector_to_histogram_with_summary_statistics(xmin = -5, xmax = +5)
#' 
#' runif(n = 100000, # sample n
#'       min = -5, # population mean (μ or mu)
#'       max = +5) |> # population sd (σ or sigma)
#'   vector_to_histogram_with_summary_statistics(xmin = -5, xmax = +5, fill = "darkcyan") +
#'   coord_cartesian(ylim = c(0, 4100))
#'
#' @export
vector_to_histogram_with_summary_statistics <- function(simulated_scores, 
                                                        binwidth = 0.1, 
                                                        xmin = NULL, 
                                                        xmax = NULL, 
                                                        x_prop = 1, 
                                                        y_prop = 0.95,
                                                        fill = "#702963"){
  
  if(is.null(xmin) | is.null(xmax)){
    stop("The arguments `xmin` and `xmax` must be set to numeric values. E.g., `xmin = -5, xmax = +5`")
  }
  
  # compute mean and standard deviation
  sample_mean <- mean(simulated_scores)
  sample_sd <- sd(simulated_scores)
  n <- length(simulated_scores)
  
  # compute absolute x values based on proportions
  x_abs <- xmin + (xmax - xmin) * x_prop
  
  # Compute breaks based on the range of the data and binwidth
  breaks <- seq(min(simulated_scores), max(simulated_scores) + binwidth, by = binwidth)
  
  # Compute the histogram counts without plotting using the computed breaks
  hist_counts <- hist(simulated_scores, breaks = breaks, plot = FALSE)
  
  # Calculate the 80% position
  y_abs  <- max(hist_counts$counts) * y_prop
  
  # plot
  p <- 
    data.frame(simulated_scores = simulated_scores) |>
    ggplot(aes(simulated_scores)) +
    geom_histogram(binwidth = binwidth, boundary = binwidth - binwidth/2, fill = fill, alpha = 0.80) +
    scale_x_continuous(breaks = breaks_pretty(xmax - xmin), 
                       limits = c(xmin, xmax), 
                       name = "Simulated scores") +
    ylab("Count") +
    theme_linedraw() +
    # theme(panel.grid.minor = element_blank(),
    #       text = element_text(family = "Courier New")) + 
    theme(panel.grid.minor = element_blank()) + 
    annotate("text", 
             x = x_abs, 
             y = y_abs,
             size = 4,
             hjust = 1,
             #family = "Courier New",
             fontface = "bold",
             label = sprintf("Sample: N = %.0f, M = %.2f, SD = %.2f", n, sample_mean, sample_sd))
  
  return(p)
}


