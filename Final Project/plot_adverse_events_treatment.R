# Function to show adverse_events by treatment
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