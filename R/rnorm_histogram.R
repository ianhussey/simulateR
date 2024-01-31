#' rnorm_histogram
#'
#' Draw samples from a normal distribution and plot as a ggplot2 histogram, with annotated population mu and signma, and sample n, mean, and SD
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @param n numeric sample size (N)
#' @param mean population mean (mu)
#' @param sd population standard deviation (sigma)
#' @param binwidth numeric binwidth argument for (\code{"ggplot2::geom_histogram()}.
#' @param xmin numeric the xmin argument for (\code{"ggplot2::scale_x_continuous()}, used in both the breaks and limits arguments
#' @param xmax numeric the xmax argument for (\code{"ggplot2::scale_x_continuous()}, used in both the breaks and limits arguments
#' @param x_prop numeric the x coordinate, in proportions of plot, where the annotation should appear
#' @param y_prop numeric the y coordinate, in proportions of plot, where the annotation should appear
#' @param fill string the fill color argument for (\code{"ggplot2::geom_histogram()}
#' @return A ggplot object
#' @examples
#' rnorm_histogram(n = 100000, 
#'                 mean = 0, 
#'                 sd = 1)
#' 
#' rnorm_histogram(n = 100000, 
#'                 mean = -1, 
#'                 sd = 1,
#'                 fill = "darkcyan")
#' 
#' 
#' rnorm_histogram(n = 100000, 
#'                 mean = 0, 
#'                 sd = 1,
#'                 binwidth = 1)
#' 
#' rnorm_histogram(n = 100000, 
#'                 mean = -1, 
#'                 sd = 1,
#'                 fill = "darkcyan",
#'                 binwidth = 1)
#' 
#' @export
rnorm_histogram <- function(n,
                            mean,
                            sd,
                            binwidth = 0.1, 
                            xmin = -5, 
                            xmax = +5, 
                            x_prop = 1, 
                            y_prop = 0.95,
                            fill = "#702963"){
  
  simulated_scores <- rnorm(n = n,
                            mean = mean,
                            sd = sd)
  
  # compute mean and standard deviation
  sample_mean <- mean(simulated_scores)
  sample_sd <- sd(simulated_scores)
  
  # compute absolute x values based on proportions
  x_abs <- xmin + (xmax - xmin) * x_prop
  
  # Compute breaks based on the range of the data and binwidth
  breaks <- seq(min(simulated_scores), max(simulated_scores) + binwidth, by = binwidth)
  
  # Compute the histogram counts without plotting using the computed breaks
  hist_counts <- hist(simulated_scores, breaks = breaks, plot = FALSE)
  
  # Calculate the 80% position
  y_abs  <- max(hist_counts$counts) * y_prop
  y_abs2 <- max(hist_counts$counts) * (y_prop - 0.1)
  
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
             label = sprintf("Population: \u03BC = %.2f,  \u03C3 = %.2f", mean, sd)) + 
    annotate("text", 
             x = x_abs, 
             y = y_abs2,
             size = 4,
             hjust = 1,
             #family = "Courier New",
             fontface = "bold",
             label = sprintf("Sample: N = %.0f, M = %.2f, SD = %.2f", n, sample_mean, sample_sd))
  
  return(p)
}




