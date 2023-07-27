

test_that("epist function works", {
  # skip_on_cran()

  expect_invisible(epis_de <- epist(female.geno = "AaBb", male.geno = "AaBb",
                                    type = "DE"))

  expect_equal(dim(epis_de), c(16, 6))
  expect_equal(epis_de[1, 6], c("DE"))

})

test_that("epist input errors", {
  # skip_on_cran()
  expect_error(epist(female.geno = "Aa", male.geno = "AaBb",
                     type = "DE"), "Number of loci for each parent = 2")

  expect_error(epist(female.geno = "AABb", male.geno = "AaBb",
                     type = "DE"), "Both parents must be dihybrids.")


  expect_error(epist(female.geno = "AaBb", male.geno = "AaBb",
                     type = "TE"), "Invalid `epistasis type` value")


})

