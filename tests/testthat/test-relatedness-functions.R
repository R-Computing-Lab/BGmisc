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


test_that("inferRelatedness performs as expected", {
  result <- inferRelatedness(0, aceA = .9, aceC = 0, sharedC = 0)
  expect_equal(result, 0)
  expect_error(
    inferRelatedness(0, aceA = 2, aceC = 0, sharedC = 0),
    "aceA and aceC must be proportions between 0 and 1"
  )
})
