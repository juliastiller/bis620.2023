#' Get the number of concurrent trials for each date in a set of studies.
#'
#' This function takes a dataset of studies (`d`) and calculates the number of concurrent trials
#' for each unique date within the studies. The result is a tibble with a `date` column and a `count`
#' column indicating the number of concurrent trials at each date.
#'
#' @param d A tibble containing information about studies, including `start_date` and `completion_date`.
#' @return A tibble with a `date` column and a `count` column representing the number of concurrent trials.
#'
#' @importFrom dplyr pivot_longer select distinct arrange na.omit rename
#' @importFrom data.table na.omit
#' @importFrom purrr map_dbl
#'
#' @details
#' The function works by first extracting all unique dates from the input dataset (`d`). It then
#' iterates through each date and calculates the number of concurrent trials by checking if each
#' trial's `start_date` and `completion_date` fall within the date of interest.
#' The result is a tibble with a `date` column representing unique dates and a corresponding `count` column.
#'
#' @export

get_concurrent_trials = function(d) {
  # Extract all unique dates from the dataset.
  all_dates = d |>
    pivot_longer(cols = everything()) |>
    select(-name) |>
    distinct() |>
    arrange(value) |>
    na.omit() |>
    rename(date = value)

  # Helper function to check if a date is within a range.
  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }

  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count =
    map_dbl(
      all_dates$date,
      ~ .x |>
        within_date(d$start_date, d$completion_date) |>
        sum(na.rm = TRUE)
    )
  return(all_dates)
}
