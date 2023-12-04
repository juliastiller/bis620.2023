#' Create a histogram of the countries that trials in a query are coming from
#' @param data the database table.
#' @param num_top_countries the number countries the user wants to visualize
plot_countries_frequency = function(data, num_top_countries) {
  # Find the top n most common countries
  country_grouped <- data |>
    group_by(country_name) |>
    summarize(n = n()) |>
    arrange(desc(n))
  
  top_countries <- country_grouped$country_name |>
    head(num_top_countries)
  
  # Create a new column that determines whether the study is in one of those top n countries or not
  country_grouped$country_group <- ifelse(country_grouped$country_name %in% top_countries, 
                                          country_grouped$country_name, 
                                          "Other")
  
  # Define a fixed set of countries
  fixed_countries <- append(top_countries, "Other")
  
  # Count country frequencies
  country_counts <- country_grouped |>
    group_by(country_group)  |>
    summarize(total = sum(n))
  
  # Create a data frame with the fixed countries and their counts
  country_data <- data.frame(Country = country_counts$country_group, Count = as.numeric(country_counts$total))
  
  # Order countries and create labels
  country_data$Country <- factor(country_data$Country, levels = fixed_countries)
  
  # Plot
  ggplot(country_data, aes(x = Country, y = Count)) +
    geom_col(fill = "skyblue", color = "black") +
    xlab("Country") +
    ylab("Count") +
    labs(title = "Clinical Trial Country Distribution",  # Add title
         caption = "Source: https://clinicaltrials.gov/") +  # Add caption
    scale_x_discrete(labels = scales::wrap_format(width = 15)) +  # Wrap x-axis labels for better presentation
    theme_minimal() + # Use a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
}