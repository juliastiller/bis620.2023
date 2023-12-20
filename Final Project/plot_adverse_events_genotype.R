# Function to show adverse events by genotype
plot_adverse_events_genotype <- function(d) {
  d |>
    filter(Genotype != "Unknown") |> #ignore unknown genotypes
    group_by(Genotype) |>
    summarize(count = n(), avg_adverse_events = round(sum(adverse_events)/n(), 2)) |>
    ggplot(aes(x = Genotype, y = avg_adverse_events, fill = Genotype)) +
    geom_bar(stat = "identity") +
    labs(title = "KRAS Mutation Status Avg Adverse Events", x = "Genotype", y = "Average Number of Adverse Events") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
    geom_text(aes(label = avg_adverse_events), vjust = -0.5)
}