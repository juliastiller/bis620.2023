#' Conduct a two-sample t-test to determine whether survival days differ by genotype.
#'
#' @details
#' This function takes a database table of patient data (`d`) and separates the number of
#' survival days by genotype. Then the two resulting groups undergo a two-sample t-test which
#' will have p<0.05 if their average number of survival days is statistically different.
#'
#' @param d A database table containing information about patients, including `Genotype` and `DTHDY`.
#'
#' @return A t-test showing whether survival days differ by genotype.
#'
#' @importFrom dplyr filter select
#'
#' @export
test_survival_days_genotype <- function(d) {
  mutant_survival_days <- d |>
    filter(Genotype == "Mutant") |>
    select(DTHDY)
  
  wild_type_survival_days <- d |>
    filter(Genotype == "Wild-type") |>
    select(DTHDY)
  
  return(t.test(mutant_survival_days, wild_type_survival_days, var.equal = TRUE))
}