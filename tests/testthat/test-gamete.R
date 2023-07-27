test_that("gamete function works", {
  # skip_on_cran()
  expect_invisible(x <- split_geno("AaBb")$Loci)
  expect_equal(gamete(x), factor(c("AB", "aB", "Ab", "ab"),
                                 levels = c("AB", "aB", "Ab", "ab")))

})
