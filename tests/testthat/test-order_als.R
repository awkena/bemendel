
test_that("order_als function works", {
  # skip_on_cran()
  expect_invisible(x <- split_geno("AaBb")$Loci)
  expect_equal(order_als(x$Locus_1), c("Aa"))
  expect_equal(order_als(x$Locus_2), c("Bb"))

})
