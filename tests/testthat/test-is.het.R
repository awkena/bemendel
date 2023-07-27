
test_that("is.het function works", {
  # skip_on_cran()

  expect_true(is.het(split_geno("AaBb")$Loci[[1]]) == TRUE)
  expect_true(is.het(split_geno("AABb")$Loci[[1]]) == FALSE)


})
