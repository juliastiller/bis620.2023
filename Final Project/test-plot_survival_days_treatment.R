test_that("plot_survival_days_treatment() generates a bar chart", {
  library(ggplot2)
  data("d")
  vdiffr::expect_doppelganger(
    "sd_treat_chart", plot_survival_days_treatment(d)
  )
})