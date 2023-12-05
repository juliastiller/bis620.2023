#' Accelerometry Data Resampled from UK Biobank
#'
#' Toy accelerometry data for BIS620
#'
#' @format ## `accel`
#' A data frame with 1,080,000 rows and 4 columns:
#' \describe{
#'   \item{time}{the time of the measurement}
#'   \item{X, Y, Z}{Accelerometry measurement (in milligravities).}
#' }
"accel"

#' Subset of NCT studies data for BIS620.
#'
#' @format ## `studies`
#' A data frame with 10,000 rows and 70 columns. Access the data [here](https://aact.ctti-clinicaltrials.org/pipe_files).
"studies"

#' NCT Countries Data
#'
#' @format ## `countries`
#' A data frame with 654,096 rows and 4 columns. Access the data [here](https://aact.ctti-clinicaltrials.org/pipe_files).
#' \describe{
#'   \item{id}{Unique number for each study}
#'   \item{nct_id}{NCT ID of the study}
#'   \item{name}{Country name}
#'   \item{removed}{Whether the study is removed}
#' }
"countries"

#' Subset of NCT interventions data for BIS620.
#'
#' @format ## `interventions`
#' A data frame with 10,000 rows and 2 columns. Access the data [here](https://aact.ctti-clinicaltrials.org/pipe_files).
"interventions"

#' NCT Sponsors Data
#'
#' @format ## `sponsors`
#' A data frame with 749,487 rows and 5 columns. Access the data [here](https://aact.ctti-clinicaltrials.org/pipe_files).
"sponsors"

#' NCT Conditions Data
#'
#' @format ## `conditions`
#' A data frame with 808,713 rows and 4 columns. Access the data [here](https://aact.ctti-clinicaltrials.org/pipe_files).
"conditions"
