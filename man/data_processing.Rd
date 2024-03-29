% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_processing.R
\name{data_processing}
\alias{data_processing}
\title{data_processing}
\usage{
data_processing(nested_data, method)
}
\arguments{
\item{nested_data}{A nested data frame containing a column named data_raw, which is nested data frames of raw data, produced by the data generation functions.}

\item{method}{the processing function to be applied to data_raw, e.g., \code{"calculate_mean_scores()"} or \code{"use_latent_scores()"}.}
}
\value{
A nested data frame of data_raw + new processed data column.
}
\description{
Apply a data processing function to the raw data such as \code{"calculate_mean_scores()"}.
}
