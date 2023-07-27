
test_that("Split_geno function works", {
  # skip_on_cran()

    expect_equal(split_geno("AaBb")$Loci[[1]], c("A", "a"))
    expect_equal(split_geno("AaBb")$Loci[[2]], c("B", "b"))

})

test_that("split_geno code input errors", {
  # skip_on_cran()
  expect_error(split_geno("AaB"),
               "Number of characters for inputted genotype must be a multiple of 2")



})
