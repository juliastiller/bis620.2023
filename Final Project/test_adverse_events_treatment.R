# Function to test whether adverse events differ by treatment
test_adverse_events_treatment <- function(d) {
  folfox_adverse_events <- d |>
    filter(ATRT == "FOLFOX alone") |>
    select(adverse_events)
  
  folxfox_panitumumab_adverse_events <- d |>
    filter(ATRT == "Panitumumab + FOLFOX") |>
    select(adverse_events)
  
  return(t.test(folfox_adverse_events, folxfox_panitumumab_adverse_events, var.equal = TRUE))
}