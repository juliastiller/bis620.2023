#' Conduct a two-sample t-test to determine whether adverse events differ by genotype.
#'
#' @details
#' This function takes a database table of patient data (`d`) and separates the number of
#' adverse events by genotype. Then the two resulting groups undergo a two-sample t-test
#' which will have p<0.05 if their number of adverse events is statistically different.
#'
#' @param d A database table containing information about patients, including `Genotype` and `adverse_events`.
#'
#' @return A t-test showing whether adverse events differ by genotype.
#'
#' @importFrom dplyr filter select
#'
#' @export
test_adverse_events_genotype <- function(d) {
  mutant_adverse_events <- d |>
    filter(Genotype == "Mutant") |>
    select(adverse_events)
  
  wild_type_adverse_events <- d |>
    filter(Genotype == "Wild-type") |>
    select(adverse_events)
  
  return(t.test(mutant_adverse_events, wild_type_adverse_events, var.equal = TRUE))
}