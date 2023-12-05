test_that("plot_concurrent_studies() generates a line chart", {
  library(ggplot2)
  data("studies")
  vdiffr::expect_doppelganger(
    "concurrent_studies_plot", plot_concurrent_studies(studies)
  )
})
