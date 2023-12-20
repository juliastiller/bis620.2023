#' Create a plot of adverse events by genotype.
#'
#' @details
#' This function takes a database table of patient data (`d`) and generates a bar chart
#' representing the number of adverse events by genotype. The x-axis corresponds to the genotypes,
#' and the y-axis represents the count of adverse events in each category.
#'
#' @param d A database table containing information about patients, including `Genotype` and `adverse_events`.
#'
#' @return A bar chart showing the number of adverse events by genotype.
#'
#' @importFrom dplyr filter group_by summarize n
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal theme element_text geom_text
#'
#' @export
plot_adverse_events_genotype <- function(d) {
  d |>
    filter(Genotype != "Unknown") |> #ignore unknown genotypes
    group_by(Genotype) |>
    summarize(count = n(), avg_adverse_events = round(sum(adverse_events)/n(), 2)) |>
    ggplot(aes(x = Genotype, y = avg_adverse_events, fill = Genotype)) +
    geom_bar(stat = "identity") +
    labs(title = "KRAS Mutation Status Avg Adverse Events", x = "Genotype", y = "Average Number of Adverse Events") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
    geom_text(aes(label = avg_adverse_events), vjust = -0.5)
}