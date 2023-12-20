test_that("plot_survival_days_genotype() generates a bar chart", {
  library(ggplot2)
  data("d")
  vdiffr::expect_doppelganger(
    "sd_gen_chart", plot_survival_days_genotype(d)
  )
})