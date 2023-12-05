test_that("plot_interventions_histogram() generates a histogram", {
  library(ggplot2)
  data("interventions")
  vdiffr::expect_doppelganger(
    "interventions_histogram", plot_interventions_histogram(interventions)
  )
})