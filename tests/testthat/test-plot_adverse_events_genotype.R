test_that("plot_adverse_events_genotype() generates a bar chart", {
  library(ggplot2)
  data("d")
  vdiffr::expect_doppelganger(
    "ae_gen_chart", plot_adverse_events_genotype(d)
  )
})
