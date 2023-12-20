#' Conduct a two-sample t-test to determine whether survival days differ by treatment.
#'
#' @details
#' This function takes a database table of patient data (`d`) and separates the number of
#' survival days by treatment. Then the two resulting groups undergo a two-sample t-test which
#' will have p<0.05 if their average number of survival days is statistically different.
#'
#' @param d A database table containing information about patients, including `ATRT` and `DTHDY`.
#'
#' @return A t-test showing whether survival days differ by treatment.
#'
#' @importFrom dplyr filter select
#'
#' @export
test_survival_days_treatment <- function(d) {
  folfox_survival_days <- d |>
    filter(ATRT == "FOLFOX alone") |>
    select(DTHDY)
  
  folxfox_panitumumab_survival_days <- d |>
    filter(ATRT == "Panitumumab + FOLFOX") |>
    select(DTHDY)
  
  return(t.test(folfox_survival_days, folxfox_panitumumab_survival_days, var.equal = TRUE))
}