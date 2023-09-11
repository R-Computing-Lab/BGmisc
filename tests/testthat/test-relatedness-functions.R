# Test for calculateRelatedness and inferRelatedness functions
test_that("calculateRelatedness function for full siblings", {
  result <- calculateRelatedness(generations = 1, full = TRUE)
  expect_equal(result, 0.5, tolerance = 1e-8)
})

test_that("calculateRelatedness function for half siblings", {
  result <- calculateRelatedness(generations = 1, full = FALSE)
  expect_equal(result, 0.25, tolerance = 1e-8)
})

