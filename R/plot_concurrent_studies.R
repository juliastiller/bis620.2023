#' Create a plot of concurrent studies in the query.
#'
#' @details
#' This function takes a database table of studies (`studies`) and generates a line plot
#' representing the number of concurrent trials over time. The x-axis corresponds to the dates,
#' and the y-axis represents the count of concurrent trials at each date.
#'
#' @param studies A database table containing information about studies, including `start_date` and `completion_date`.
#'
#' @return A line plot showing the trend of concurrent trials over time.
#'
#' @importFrom dplyr select
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal element_text
#'
#' @seealso \code{\link{get_concurrent_trials}}
#'
#' @export
plot_concurrent_studies = function(studies) {
  studies |>
    select(start_date, completion_date) |>
    get_concurrent_trials() |>
    ggplot(aes(x = date, y = count)) +
    geom_line(color = "blue") +  # Customize line color
    xlab("Date") +
    ylab("Count") +
    labs(title = "Concurrent Trials Over Time",  # Add title
         caption = "Source: https://clinicaltrials.gov/") +  # Add caption
    theme_minimal() +  # Use a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
}
