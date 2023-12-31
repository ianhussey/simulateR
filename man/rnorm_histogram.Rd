% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rnorm_histogram.R
\name{rnorm_histogram}
\alias{rnorm_histogram}
\title{rnorm_histogram}
\usage{
rnorm_histogram(
  n,
  mean,
  sd,
  binwidth = 0.1,
  xmin = -5,
  xmax = +5,
  x_prop = 1,
  y_prop = 0.95,
  fill = "#702963"
)
}
\arguments{
\item{n}{numeric sample size (N)}

\item{mean}{population mean (mu)}

\item{sd}{population standard deviation (sigma)}

\item{binwidth}{numeric binwidth argument for (\code{"ggplot2::geom_histogram()}.}

\item{xmin}{numeric the xmin argument for (\code{"ggplot2::scale_x_continuous()}, used in both the breaks and limits arguments}

\item{xmax}{numeric the xmax argument for (\code{"ggplot2::scale_x_continuous()}, used in both the breaks and limits arguments}

\item{x_prop}{numeric the x coordinate, in proportions of plot, where the annotation should appear}

\item{y_prop}{numeric the y coordinate, in proportions of plot, where the annotation should appear}

\item{fill}{string the fill color argument for (\code{"ggplot2::geom_histogram()}}
}
\value{
A ggplot object
}
\description{
Draw samples from a normal distribution and plot as a ggplot2 histogram, with annotated population mu and signma, and sample n, mean, and SD
}
\examples{
rnorm_histogram(n = 100000, 
                mean = 0, 
                sd = 1)

rnorm_histogram(n = 100000, 
                mean = -1, 
                sd = 1,
                fill = "darkcyan")


rnorm_histogram(n = 100000, 
                mean = 0, 
                sd = 1,
                binwidth = 1)

rnorm_histogram(n = 100000, 
                mean = -1, 
                sd = 1,
                fill = "darkcyan",
                binwidth = 1)

}
