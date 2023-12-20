# Function to determine genotype
get_genotype <- function(subjid) {
  sublist <- as.list(filtered_biomark[filtered_biomark$SUBJID == subjid, ][, c(3, 5, 7, 9)])
  if ("Mutant" %in% sublist) {
    return("Mutant")
  } else if ((sum(sublist == "Wild-type") > sum(sublist == "Unknown")) | 
             (sum(sublist == "Wild-type") > sum(sublist == "Failure")) |
             (sum(sublist == "Wild-type") > sum(sublist == "")) # OH: need to include empty set
  ) {
    return("Wild-type")
  } else {
    return("Unknown")
  }
}