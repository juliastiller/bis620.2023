% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_conditions_histogram.R
\name{plot_conditions_histogram}
\alias{plot_conditions_histogram}
\title{Create a histogram of the conditions that trials in a query are examining.}
\usage{
plot_conditions_histogram(data, num_top_conditions)
}
\arguments{
\item{data}{A database table containing information about trials, including \code{condition_name}.}

\item{num_top_conditions}{The number of top conditions to visualize.}
}
\value{
A histogram showing the distribution of conditions in the trials.
}
\description{
Create a histogram of the conditions that trials in a query are examining.
}
\details{
This function takes a database table of trials (\code{data}) and generates a histogram
depicting the distribution of conditions examined in the trials. The user can specify
the number of top conditions (\code{num_top_conditions}) they want to visualize.
}
\seealso{
\code{\link{get_concurrent_trials}}
}
