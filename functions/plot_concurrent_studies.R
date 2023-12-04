#' Create a plot of the concurrent studies in the query
#' @param studies the database table.
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