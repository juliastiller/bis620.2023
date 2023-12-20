# Function to determine whether survival days differ by genotype
survival_days_genotype <- function(d) {
  mutant_survival_days <- d |>
    filter(Genotype == "Mutant") |>
    select(DTHDY)
  
  wild_type_survival_days <- d |>
    filter(Genotype == "Wild-type") |>
    select(DTHDY)
  
  return(t.test(mutant_survival_days, wild_type_survival_days, var.equal = TRUE))
}