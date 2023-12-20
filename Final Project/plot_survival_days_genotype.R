# Function to show survival days by genotype
plot_survival_days_genotype <- function(d) {
  d |>
    filter(Genotype != "Unknown") |> #ignore unknown genotypes
    group_by(Genotype) |>
    summarize(count = n(), avg_survival_days = round(sum(DTHDY)/n(), 2)) |>
    ggplot(aes(x = Genotype, y = avg_survival_days, fill = Genotype)) +
    geom_bar(stat = "identity") +
    labs(title = "KRAS Mutation Status Avg Survival", x = "Genotype", y = "Average Survival (Days)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
    geom_text(aes(label = avg_survival_days), vjust = -0.5)
}