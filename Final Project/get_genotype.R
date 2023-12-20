#' Determine the genotype for each subject ID that is passed to the function.
#'
#' This function takes a subject ID (`subjid`) and finds all of their biomarkers. A patient will be considered 
#' “Mutant” if there is at least one “Mutant” biomarker in KRAS exons 2, 3, 4. Patients will be considered 
#' “Wild-type” if they are not “Mutant” and they have more “Wild-type” markers than “Unknown” or “Failure”.
#'
#' @param subjid A string containing information about which subject's biomarkers to examine.
#' @return A string of "Mutant", "Wild-type", or "Unknown" representing the subject's genotype.
#'
#' @details
#' The function works by first extracting the subject's unique biomarkers (`sublist`). It then
#' runs through the sublist, and determines "Mutant" if there is at least one “Mutant” biomarker, or
#' "Wild-type"  if they are not “Mutant” and they have more “Wild-type” markers than “Unknown” or “Failure”.
#' The result is a string representing the determined genotype.
#'
#' @export
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