
test_that("do_pun function works", {
  # skip_on_cran()
  expect_invisible(pun1 <- do_pun(female.geno = "AaBb", male.geno = "AaBb"))
  expect_equal(dim(pun1$punnett_square), c(4, 4))
  expect_equal(dim(pun1$pun_summary), c(9, 4))
  expect_equal(pun1$punnett_square[1, 1], c("AABB"))


})
