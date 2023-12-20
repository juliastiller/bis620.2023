# Function to test whether adverse events differ by genotype
test_adverse_events_genotype <- function(d) {
  mutant_adverse_events <- d |>
    filter(Genotype == "Mutant") |>
    select(adverse_events)
  
  wild_type_adverse_events <- d |>
    filter(Genotype == "Wild-type") |>
    select(adverse_events)
  
  return(t.test(mutant_adverse_events, wild_type_adverse_events, var.equal = TRUE))
}