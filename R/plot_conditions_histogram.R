#' Create a histogram of the conditions that trials in a query are examining.
#'
#' @details
#' This function takes a database table of trials (`data`) and generates a histogram
#' depicting the distribution of conditions examined in the trials. The user can specify
#' the number of top conditions (`num_top_conditions`) they want to visualize.
#'
#' @param data A database table containing information about trials, including `condition_name`.
#' @param num_top_conditions The number of top conditions to visualize.
#'
#' @return A histogram showing the distribution of conditions in the trials.
#'
#' @importFrom dplyr group_by summarize arrange
#' @importFrom ggplot2 ggplot aes geom_col labs scale_x_discrete scale_y_log10 theme_minimal element_text theme xlab ylab
#' @importFrom scales wrap_format
#'
#' @seealso \code{\link{get_concurrent_trials}}
#'
#' @export
plot_conditions_histogram = function(data, num_top_conditions) {
  # Find the top n most common conditions
  x_grouped <- data |>
    group_by(condition_name) |>
    summarize(n=n()) |>
    arrange(desc(n))

  top_conditions <- x_grouped$condition_name |>
    head(num_top_conditions)

  # Create a new column that determines whether the study is in one of those top n conditions or not
  x_grouped$condition_group <- ifelse(x_grouped$condition_name %in% top_conditions,
                                      x_grouped$condition_name,
                                      "Other")

  # Define a fixed set of conditions
  fixed_conditions <- append(top_conditions, "Other")

  # Count condition frequencies
  condition_counts <- x_grouped |>
    group_by(condition_group)  |>
    summarize(total= sum(n))

  # Create a data frame with the fixed conditions and their counts
  condition_data <- data.frame(Condition = condition_counts$condition_group, Count = as.numeric(condition_counts$total))

  # Order conditions and create labels
  condition_data$Condition <- factor(condition_data$Condition, levels = fixed_conditions)

  # Plot
  ggplot(condition_data, aes(x = Condition, y = Count)) +
    geom_col(fill = "skyblue", color = "black") +
    xlab("Condition") +
    ylab("Count") +
    labs(title = "Clinical Trial Condition Distribution",  # Add title
         caption = "Source: https://clinicaltrials.gov/") +  # Add caption
    scale_x_discrete(labels = scales::wrap_format(width = 15)) +  # Wrap x-axis labels for better presentation
    scale_y_log10() + # Scale y to better see smaller buckets
    theme_minimal() + # Use a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
}
