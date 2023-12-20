# Function to test whether survival days differ by treatment
test_survival_days_treatment <- function(d) {
  folfox_survival_days <- d |>
    filter(ATRT == "FOLFOX alone") |>
    select(DTHDY)
  
  folxfox_panitumumab_survival_days <- d |>
    filter(ATRT == "Panitumumab + FOLFOX") |>
    select(DTHDY)
  
  return(t.test(folfox_survival_days, folxfox_panitumumab_survival_days, var.equal = TRUE))
}