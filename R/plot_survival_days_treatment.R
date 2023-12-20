# Function to show survival days by treatment

#' Create a plot of survival days by treatment.
#'
#' @details
#' This function takes a database table of patient data (`d`) and generates a bar chart
#' representing the number of survival days by treatment. The x-axis corresponds to the treatments,
#' and the y-axis represents the average number of survival days in each category.
#'
#' @param d A database table containing information about patients, including `ATRT` and `avg_survival_days`.
#'
#' @return A bar chart showing the average number of survival days by treatment.
#'
#' @importFrom dplyr filter group_by summarize
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal theme element_text geom_text
#'
#' @export
plot_survival_days_treatment <- function(d) {
  d |>
    filter(Genotype != "Unknown") |> #ignore unknown genotypes
    group_by(ATRT) |>
    summarize(count = n(), avg_survival_days = round(sum(DTHDY)/n(), 2)) |>
    ggplot(aes(x = ATRT, y = avg_survival_days, fill = ATRT)) +
    geom_bar(stat = "identity") +
    labs(title = "Treatment Avg Survival", x = "Treatment", y = "Average Survival (Days)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
    geom_text(aes(label = avg_survival_days), vjust = -0.5)
}