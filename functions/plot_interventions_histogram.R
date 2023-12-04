#' Create a histogram of the intervention types that trials in a query are coming from
#' @param data the database table.
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