test_that("test_adverse_events_genotype() generates a t-test", {
  data("d")
  expect_equal(type(test_adverse_events_genotype(d)), "htest")
})
