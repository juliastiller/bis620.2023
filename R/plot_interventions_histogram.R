#' Create a histogram of the intervention types that trials in a query are coming from.
#'
#' @details
#' This function takes a database table of trials (`data`) and generates a histogram
#' depicting the distribution of intervention types in the trials.
#'
#' @param data A database table containing information about trials, including `intervention_type`.
#'
#' @return A histogram showing the distribution of intervention types in the trials.
#'
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal element_text theme xlab ylab
#'
#' @export
plot_interventions_histogram = function(data) {
  ggplot(data, aes(x = intervention_type)) +
    geom_bar(fill = "skyblue", color = "black") +
    xlab("Intervention") +
    ylab("Count") +
    labs(title = "Clinical Trial Intervention Distribution",  # Add title
         caption = "Source: https://clinicaltrials.gov/") +  # Add caption
    theme_minimal() + # Use a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
}
