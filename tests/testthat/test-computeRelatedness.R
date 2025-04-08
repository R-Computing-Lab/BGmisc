# Test for calculateRelatedness and inferRelatedness functions
test_that("calculateRelatedness function for full siblings", {
  result <- calculateRelatedness(generations = 1, full = TRUE)
  expect_equal(result, 0.5)
})

test_that("calculateRelatedness function for half siblings", {
  result <- calculateRelatedness(generations = 1, full = FALSE)
  expect_equal(result, 0.25)
})

test_that("calculateRelatedness function with segregating genes", {
  result <- calculateRelatedness(generations = 1, segregating = FALSE)
  expect_equal(result, 0.995, tolerance = 1e-11)
})

test_that("calculateRelatedness function with empirical", {
  result <- calculateRelatedness(generations = 2, empirical = TRUE)
  expect_equal(result, 0.125, tolerance = 0)
})

test_that("calculateRelatedness function with empirical", {
  result <- calculateRelatedness(generations = 2, empirical = TRUE, maternal = TRUE)
  expect_equal(result, 0.125 + .000002426, tolerance = 1e-8)
})


# Test 7: empirical divide by zero

test_that("calculateH handles divide by zero for empirical", {
expect_error(
  calculateRelatedness(generations = 2,
                                   empirical = TRUE, total_a = 0,
                                   total_m = 0))

})
test_that("inferRelatedness performs as expected", {
  result <- inferRelatedness(0, aceA = .9, aceC = 0, sharedC = 0)
  expect_equal(result, 0)
  expect_error(
    inferRelatedness(0, aceA = 2, aceC = 0, sharedC = 0),
    "aceA and aceC must be proportions between 0 and 1"
  )
})


# Test 1: Basic Functionality Test
test_that("calculateH returns correct heritability estimates", {
  expect_equal(calculateH(0.5, 0.25, 0.4, 0.2), 0.8)
  expect_equal(calculateH(0.5, 0.125, 0.5, 0.25), 2 / 3, tolerance = 1e-8)
})

# Test 2: unusual warning
test_that("provides warning for unusual H value", {
  r1 <- 0.25
  r2 <- 0.125
  obsR1 <- 0.3
  obsR2 <- 0.1
  expected <- 1.6
  expect_warning(expect_equal(calculateH(r1, r2, obsR1, obsR2), expected))
})


# Test 3: Vectorized Input Test
test_that("calculateH handles vectorized inputs correctly", {
  r1 <- c(0.5, 0.5)
  r2 <- c(0.25, 0.125)
  obsR1 <- c(0.4, 0.5)
  obsR2 <- c(0.2, 0.25)
  expected <- c(0.8, 2 / 3)
  expect_equal(calculateH(r1, r2, obsR1, obsR2), expected, tolerance = 1e-8)
})


# Test 4: Equal Relatedness Coefficients Test
test_that("calculateH stops for equal relatedness coefficients", {
  expect_error(
    calculateH(0.5, 0.5, 0.4, 0.2),
    "Relatedness coefficients r1 and r2 must not be equal for any pair."
  )
})

# Test 5: Negative and Positive Correlation Test
test_that("calculateH handles both negative and positive correlations", {
  # Test for negative correlations with expected warnings about negative heritability values
  expect_warning(
    expect_equal(calculateH(0.5, 0.25, -0.4, -0.2), -0.8),
    regexp = "Some calculated heritability values are negative"
  )
  # Test for a scenario leading to a positive heritability estimate

  expect_warning(
    expect_warning(
      expect_equal(calculateH(0.5, 0.25, 0.2, -0.1), 1.2),
      regexp = "The correlations should not have opposite signs."
    ),
    regexp = "Some calculated heritability values are greater than 1"
  )
})

# Test 6: illegal correlation values
test_that("calculateH stops for illegal coefficients", {
  expect_warning(
    calculateH(0.5, 0.25, 1.4, 1.4),
    "The observed correlations should be between -1 and 1"
  )
})

