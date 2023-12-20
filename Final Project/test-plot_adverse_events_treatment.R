test_that("plot_adverse_events_treatment() generates a bar chart", {
  library(ggplot2)
  data("d")
  vdiffr::expect_doppelganger(
    "ae_treat_chart", plot_adverse_events_treatment(d)
  )
})