
test_that("pun2df function works", {
  # skip_on_cran()
  expect_invisible(pun1 <- do_pun(female.geno = "AaBb", male.geno = "AaBb"))
  expect_invisible(long_df <- pun2df(pun1$punnett_square))

  expect_equal(dim(long_df), c(16, 4))
  expect_equal(long_df[1, 4], c("A_B_"))

})
