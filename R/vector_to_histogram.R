#' vector_to_histogram
#'
#'#' Plot a numeric vector as a ggplot2 histogram 
#'
#' @param simulated_scores numeric vector of values to be plotted
#' @param binwidth numeric binwidth argument for (\code{"ggplot2::geom_histogram()}.
#' @param xmin numeric the xmin argument for (\code{"ggplot2::scale_x_continuous()}, used in both the breaks and limits arguments
#' @param xmax numeric the xmax argument for (\code{"ggplot2::scale_x_continuous()}, used in both the breaks and limits arguments
#' @param fill string the fill color argument for (\code{"ggplot2::geom_histogram()}
#'
#' @return A ggplot object  
#' 
#' @examples
#' rnorm(n = 50, # sample n
#'       mean = 0, # population mean (μ or mu)
#'       sd = 1) |> # population sd (σ or sigma)
#'   vector_to_histogram(xmin = -5, xmax = +5)
#' 
#' rnorm(n = 50, # sample n
#'       mean = 0, # population mean (μ or mu)
#'       sd = 1) |> # population sd (σ or sigma)
#'   vector_to_histogram(binwidth = 1, xmin = -5, xmax = +5)
#'   
#' rnorm(n = 50, # sample n
#'         mean = 0, # population mean (μ or mu)
#'         sd = 1) |> # population sd (σ or sigma)
#'   vector_to_histogram()
#' 
#' runif(n = 50, # sample n
#'       min = -5, # population mean (μ or mu)
#'       max = +5) |> # population sd (σ or sigma)
#'   vector_to_histogram(fill = "darkcyan")
#' 
#' rnorm(n = 100000, # sample n
#'       mean = 0, # population mean (μ or mu)
#'       sd = 1) |> # population sd (σ or sigma)
#'   vector_to_histogram(xmin = -5, xmax = +5)
#' 
#' runif(n = 100000, # sample n
#'       min = -5, # population mean (μ or mu)
#'       max = +5) |> # population sd (σ or sigma)
#'   vector_to_histogram(xmin = -5, xmax = +5, fill = "darkcyan") +
#'   coord_cartesian(ylim = c(0, 4100))
#' 
#' @export
vector_to_histogram <- function(simulated_scores, 
                                binwidth = 0.1, 
                                xmin = NULL, 
                                xmax = NULL, 
                                fill = "#702963"){
  
  require(ggplot2)
  require(scales)
  
  # plot
  if(is.null(xmin) & is.null(xmax)){
    p <- 
      data.frame(simulated_scores = simulated_scores) |>
      ggplot(aes(simulated_scores)) +
      geom_histogram(binwidth = binwidth, boundary = binwidth - binwidth/2, fill = fill, alpha = 0.80) +
      scale_x_continuous(name = "Simulated scores") +
      ylab("Count") +
      theme_linedraw() +
      theme(panel.grid.minor = element_blank(),
            text = element_text(family = "Courier New"))
  } else {
    p <- 
      data.frame(simulated_scores = simulated_scores) |>
      ggplot(aes(simulated_scores)) +
      geom_histogram(binwidth = binwidth, boundary = binwidth - binwidth/2, fill = fill, alpha = 0.80) +
      scale_x_continuous(breaks = breaks_pretty(xmax - xmin), 
                         limits = c(xmin, xmax), 
                         name = "Simulated scores") +
      ylab("Count") +
      theme_linedraw() +
      theme(panel.grid.minor = element_blank(),
            text = element_text(family = "Courier New"))
  }
  
  return(p)
}

