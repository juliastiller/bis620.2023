% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_concurrent_trials.R
\name{get_concurrent_trials}
\alias{get_concurrent_trials}
\title{Get the number of concurrent trials for each date in a set of studies.}
\usage{
get_concurrent_trials(d)
}
\arguments{
\item{d}{A tibble containing information about studies, including \code{start_date} and \code{completion_date}.}
}
\value{
A tibble with a \code{date} column and a \code{count} column representing the number of concurrent trials.
}
\description{
This function takes a dataset of studies (\code{d}) and calculates the number of concurrent trials
for each unique date within the studies. The result is a tibble with a \code{date} column and a \code{count}
column indicating the number of concurrent trials at each date.
}
\details{
The function works by first extracting all unique dates from the input dataset (\code{d}). It then
iterates through each date and calculates the number of concurrent trials by checking if each
trial's \code{start_date} and \code{completion_date} fall within the date of interest.
The result is a tibble with a \code{date} column representing unique dates and a corresponding \code{count} column.
}
