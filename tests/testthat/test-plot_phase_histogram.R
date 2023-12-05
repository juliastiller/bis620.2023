test_that("plot_phase_histogram() generates a histogram", {
  library(ggplot2)
  data("studies")
  vdiffr::expect_doppelganger(
    "phase_histogram", plot_phase_histogram(studies)
  )
})
