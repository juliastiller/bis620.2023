test_that("get_genotypes() generates a string", {
  get_genotype("000003")
  expect_equal(get_genotype("000003"), "Mutant")
})