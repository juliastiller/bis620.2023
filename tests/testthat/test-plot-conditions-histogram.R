test_that("plot_conditions_histogram() generates a histogram", {
  library(ggplot2)
  data("conditions")
  conditions <- conditions |> rename(condition_name = name)
  vdiffr::expect_doppelganger(
    "conditions_histogram", plot_conditions_histogram(conditions, 5)
  )
})