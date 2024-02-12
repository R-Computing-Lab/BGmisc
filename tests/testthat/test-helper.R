test_that("efunc returns NA for NULL input", {
  error <- NULL
  expected <- NA
  result <- efunc(error)
  expect_identical(result, expected)
})


test_that("efunc returns NA for character string input", {
  error <- "Some error message"
  expected <- NA
  result <- efunc(error)
  expect_identical(result, expected)
})


test_that("rmvn generates multivariate normal data", {
  n <- 100
  sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  result <- rmvn(n, sigma)
  expect_equal(dim(result), c(n, 2))
})

test_that("nullToNA replaces null values with NA", {
  x <- c(1, NULL, 3, NULL, 5)
  result <- nullToNA(x)
  expect_equal(result, c(1, 3, 5))
})

test_that("try_na fuses nullToNA with efunc", {
  expect_equal(try_na(stop("An error occurred")), NA)
})

test_that("Null computes the null space of a matrix", {
  M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  result <- Null(M)
  expect_equal(dim(result), c(2, 0))
})

test_that("SimPed issues a deprecation warning", {
  expect_warning(SimPed(), "deprecated")
})

test_that("related_coef issues a deprecation warning", {
  expect_warning(related_coef(), "deprecated")
})

test_that("relatedness issues a deprecation warning", {
  expect_warning(relatedness(cor_obs = .5), "deprecated")
})
