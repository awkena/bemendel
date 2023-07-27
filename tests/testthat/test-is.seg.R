test_that("is.seg function works", {
  # skip_on_cran()
  expect_invisible(x <- split_geno("AABb")$Loci)
  expect_equal(is.seg(x[[1]]), c("Locus A is homozygous, hence would not segregate."))
  expect_equal(is.seg(x[[2]]), c("Locus B is heterozygous, hence would segregate."))

})

