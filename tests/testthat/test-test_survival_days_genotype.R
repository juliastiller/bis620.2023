test_that("test_survival_days_genotype() generates a t-test", {
  data("d")
  expect_equal(class(test_survival_days_genotype(d)), "htest")
})