# Function to show survival days by treatment
plot_survival_days_treatment <- function(d) {
  d |>
    filter(Genotype != "Unknown") |> #ignore unknown genotypes
    group_by(ATRT) |>
    summarize(count = n(), avg_survival_days = round(sum(DTHDY)/n(), 2)) |>
    ggplot(aes(x = ATRT, y = avg_survival_days, fill = ATRT)) +
    geom_bar(stat = "identity") +
    labs(title = "Treatment Avg Survival", x = "Treatment", y = "Average Survival (Days)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
    geom_text(aes(label = avg_survival_days), vjust = -0.5)
}