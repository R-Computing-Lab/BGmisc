test_that("famSizeCal returns correct number for single generation data", {
  # Test case 1: Ngen = 1
  set.seed(123)
  kpc <- .5
  Ngen <- 1
  marR <- 0.8

  result <- famSizeCal(kpc = kpc, Ngen = Ngen, marR = marR)
  expect_equal(result, 2)

  kpc <- 5

  result <- famSizeCal(kpc = kpc, Ngen = Ngen, marR = marR)
  expect_equal(result, 2)
  kpc <- 0

  result <- famSizeCal(kpc = kpc, Ngen = Ngen, marR = marR)
  expect_equal(result, 2)

})
test_that("famSizeCal throws error when numbers out of bounds", {
  set.seed(1231)
  kpc <- .5
  Ngen <- 0
  marR <- 0.8

  expect_error(famSizeCal(kpc = kpc, Ngen = Ngen, marR = marR))
})
test_that("famSizeCal returns increasingly large  numbers for multi-generation data", {
  set.seed(123)
  kpc <- .5
  Ngen <- 3
  marR <- 0.8
  result_A <- famSizeCal(kpc = kpc, Ngen = Ngen, marR = marR)

  Ngen <- 4
  result_B <- famSizeCal(kpc = kpc, Ngen = Ngen, marR = marR)

  expect_gt(result_B, result_A)

})
test_that("famSizeCal returns increasingly large  numbers for increased kpc", {
  set.seed(123)
 kpc <- 0
  Ngen <- 3
  marR <- 0.8
  result <- famSizeCal(kpc = kpc, Ngen = Ngen, marR = marR)
  expect_equal(result, 2)

  kpc <- .5
  result_A <- famSizeCal(kpc = kpc, Ngen = Ngen, marR = marR)

  kpc <- 5
  result_B <- famSizeCal(kpc = kpc, Ngen = Ngen, marR = marR)

  expect_gt(result_B, result_A)

})
