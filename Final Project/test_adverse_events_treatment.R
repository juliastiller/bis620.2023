#' Conduct a two-sample t-test to determine whether adverse events differ by treatment.
#'
#' @details
#' This function takes a database table of patient data (`d`) and separates the number of
#' adverse events by treatment. Then the two resulting groups undergo a two-sample t-test
#' which will have p<0.05 if their number of adverse events is statistically different.
#'
#' @param d A database table containing information about patients, including `ATRT` and `adverse_events`.
#'
#' @return A t-test showing whether adverse events differ by treatment.
#'
#' @importFrom dplyr filter select
#'
#' @export
test_adverse_events_treatment <- function(d) {
  folfox_adverse_events <- d |>
    filter(ATRT == "FOLFOX alone") |>
    select(adverse_events)
  
  folxfox_panitumumab_adverse_events <- d |>
    filter(ATRT == "Panitumumab + FOLFOX") |>
    select(adverse_events)
  
  return(t.test(folfox_adverse_events, folxfox_panitumumab_adverse_events, var.equal = TRUE))
}