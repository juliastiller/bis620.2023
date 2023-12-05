#' Create a histogram of the phases returned by a brief title keyword search.
#'
#' @details
#' This function takes a database table of trials (`x`) and generates a histogram
#' depicting the distribution of phases in the trials based on a brief title keyword search.
#'
#' @param x A database table containing information about trials, including `phase`.
#'
#' @return A histogram showing the distribution of phases in the trials.
#'
#' @importFrom ggplot2 ggplot aes geom_col labs scale_x_discrete theme_minimal element_text theme xlab ylab
#' @importFrom scales wrap_format
#'
#' @export
plot_phase_histogram = function(x) {
  # Define a fixed set of phases
  x$phase[is.na(x$phase)] = "NA"
  # Problem 1: Fix the phase histogram so that the x-axis values are uniform regardless of the query.
  fixed_phases <- c("Early Phase 1", "Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3",
                    "Phase 3", "Phase 4", "Not Applicable", "NA")  # Include all possible phases

  # Count phase frequencies
  phase_counts <- table(factor(x$phase, levels = fixed_phases))

  # Create a data frame with the fixed phases and their counts
  phase_data <- data.frame(Phase = names(phase_counts), Count = as.numeric(phase_counts))

  # Order phases and create labels
  phase_data$Phase <- factor(phase_data$Phase, levels = fixed_phases)

  # Create the phase histogram
  ggplot(phase_data, aes(x = Phase, y = Count)) +
    geom_col(fill = "skyblue", color = "black") +  # Customize fill and border colors
    xlab("Phase") +
    ylab("Count") +
    labs(title = "Clinical Trial Phase Distribution",  # Add title
         caption = "Source: https://clinicaltrials.gov/") +  # Add caption
    scale_x_discrete(labels = scales::wrap_format(width = 10)) +  # Wrap x-axis labels for better presentation
    theme_minimal() +  # Use a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
}
