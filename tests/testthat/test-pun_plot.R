


test_that("pun_plot function works", {

  expect_invisible(pun1 <- do_pun(female.geno = "AaBb", male.geno = "AaBb"))

  expect_invisible(long_df <- pun2df(pun1$punnett_square))

  pp <- pun_plot(pun2df = long_df, text_size = 6)

  vdiffr::expect_doppelganger("plot 1", pp)

})


test_that("pun_plot function works", {

  expect_invisible(epis_de <- epist(female.geno = "AaBb", male.geno = "AaBb",
                                    type = "DE"))

  dd <- pun_plot(pun2df = epis_de, text_size = 6, epistasis = TRUE)

  vdiffr::expect_doppelganger("plot 2", dd)

})
