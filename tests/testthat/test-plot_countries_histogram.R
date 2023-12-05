test_that("plot_countries_frequency() generates a histogram", {
  library(ggplot2)
  data("countries")
  countries <- countries |> rename(country_name = name)
  vdiffr::expect_doppelganger(
    "countries_histogram", plot_countries_frequency(countries, 5)
  )
})
