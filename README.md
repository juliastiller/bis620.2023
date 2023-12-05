
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bis620.2023

<!-- badges: start -->

[![R-CMD-check](https://github.com/juliastiller/bis620.2023/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/juliastiller/bis620.2023/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/juliastiller/bis620.2023/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/juliastiller/bis620.2023/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

The goal of bis620.2023 is to empower users to explore clinical trial
data with ease. Through filtering options such as keywords, sponsors,
status, and FDA regulation, users can efficiently extract information
via the ClinicalTrials.gov API. This package caters to researchers,
clinicians, and data scientists, offering seamless retrieval and dynamic
visualization capabilities. Users can gain valuable insights by
exploring trends across different dimensions, including phase,
concurrency, condition, country, and intervention.

## Installation

You can install the development version of bis620.2023 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("juliastiller/bis620.2023")
```

``` r
library(covr)
print(package_coverage())
#> bis620.2023 Coverage: 15.98%
#> R/plot_conditions_histogram.R: 0.00%
#> R/plot_countries_frequency.R: 0.00%
#> R/query_keywords.R: 0.00%
#> R/run_shiny_app.R: 0.00%
#> R/spectral.R: 85.71%
#> R/get_concurrent_trials.R: 100.00%
#> R/hello.R: 100.00%
#> R/plot.R: 100.00%
#> R/plot_concurrent_studies.R: 100.00%
#> R/plot_interventions_histogram.R: 100.00%
#> R/plot_phase_histogram.R: 100.00%
```
