#' Create a plot of survival days by genotype.
#'
#' @details
#' This function takes a database table of patient data (`d`) and generates a bar chart
#' representing the number of survival days by genotype. The x-axis corresponds to the genotypes,
#' and the y-axis represents the average number of survival days in each category.
#'
#' @param d A database table containing information about patients, including `Genotype` and `avg_survival_days`.
#'
#' @return A bar chart showing the average number of survival days by genotype.
#'
#' @importFrom dplyr filter group_by summarize n
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal theme element_text geom_text
#'
#' @export
plot_survival_days_genotype <- function(d) {
  d |>
    filter(Genotype != "Unknown") |> #ignore unknown genotypes
    group_by(Genotype) |>
    summarize(count = n(), avg_survival_days = round(sum(DTHDY)/n(), 2)) |>
    ggplot(aes(x = Genotype, y = avg_survival_days, fill = Genotype)) +
    geom_bar(stat = "identity") +
    labs(title = "KRAS Mutation Status Avg Survival", x = "Genotype", y = "Average Survival (Days)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
    geom_text(aes(label = avg_survival_days), vjust = -0.5)
}