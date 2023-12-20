test_that("test_adverse_events_treatment() generates a t-test", {
  data("d")
  expect_equal(class(test_adverse_events_treatment(d)), "htest")
})