test_that("get_genotypes() generates a string", {
  expect_equal(get_genotype("000001"), "Unknown")
  expect_equal(get_genotype("000003"), "Mutant")
  expect_equal(get_genotype("000018"), "Wild-type")
})