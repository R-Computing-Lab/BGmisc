# Test Case 1: Basic functionality with m < n
test_that("evenInsert inserts elements of m into n when m < n", {
  result_observed <- evenInsert(1:3, 4:6)
  result_expected <- c(1, 4, 2, 5, 3, 6)

  expect_equal(result_observed, result_expected)
})

# # Test Case 2: Functionality with m > n (should swap m and n)
# test_that("evenInsert swaps and inserts elements when m > n", {
#   result_observed <- evenInsert(4:6, 1:3)
#   result_expected <- c(1, 4, 2, 5, 3, 6)
#
#   expect_equal(result_observed,result_expected)
# })
#
# # Test Case 3: Edge case with one empty vector
# test_that("evenInsert handles one empty vector correctly", {
#   result_observed <- evenInsert(numeric(0), 1:3)
#   result_expected <- 1:3
#   expect_equal(result_observed,result_expected)
# })
#
# # Test Case 4: Edge case with both vectors empty
# test_that("evenInsert handles two empty vectors correctly", {
#   result_observed <- evenInsert(numeric(0), numeric(0))
#   result_expected <- numeric(0)
#
#   expect_equal(result_observed,result_expected)
# })

# Test Case 5: Verbose mode
test_that("Verbose mode work for evenInsert", {
  expect_output(evenInsert(1:3, 4:6, verbose = TRUE),
    regexp = "1 2 3 \\n1 2 3 \\n1 2 3 \\n4 5 6 "
  )
})


# Test Case 6: Vectors of equal length
test_that("evenInsert handles vectors of equal length correctly", {
  result_observed <- evenInsert(1:3, 4:6)
  result_expected <- c(1, 4, 2, 5, 3, 6)


  expect_equal(result_observed, result_expected)
})
