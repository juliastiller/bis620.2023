#' Create a plot of adverse events by treatment.
#'
#' @details
#' This function takes a database table of patient data (`d`) and generates a bar chart
#' representing the number of adverse events by treatment. The x-axis corresponds to the treatments,
#' and the y-axis represents the count of adverse events in each category.
#'
#' @param d A database table containing information about patients, including `ATRT` and `adverse_events`.
#'
#' @return A bar chart showing the number of adverse events by treatment.
#'
#' @importFrom dplyr filter group_by summarize n
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal theme element_text geom_text
#'
#' @export
plot_adverse_events_treatment <- function(d) {
  d |>
    filter(Genotype != "Unknown") |> #ignore unknown genotypes
    group_by(ATRT) |>
    summarize(count = n(), avg_adverse_events = round(sum(adverse_events)/n(), 2)) |>
    ggplot(aes(x = ATRT, y = avg_adverse_events, fill = ATRT)) +
    geom_bar(stat = "identity") +
    labs(title = "Treatment Avg Adverse Events", x = "Treatment", y = "Average Number of Adverse Events") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
    geom_text(aes(label = avg_adverse_events), vjust = -0.5)
}