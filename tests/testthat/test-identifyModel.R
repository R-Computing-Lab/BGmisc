# Test cases for model identification and fitting functions

# Test that a fully specified model with adequate parameters is correctly identified

test_that("identified model is identified", {
  set.seed(5)

  expect_output(identifyComponentModel(
    A = list(matrix(c(1, .5, .5, 1), 2, 2), matrix(1, 2, 2)),
    C = list(matrix(1, 2, 2), matrix(1, 2, 2)),
    E = diag(1, 4)
  ))

  expect_true(identifyComponentModel(
    A = list(matrix(c(1, .5, .5, 1), 2, 2), matrix(1, 2, 2)),
    C = list(matrix(1, 2, 2), matrix(1, 2, 2)),
    E = diag(1, 4)
  )$identified)
})

# Test for error when a model is underidentified due to insufficient parameters

test_that("underidentified model is unidentified", {
  set.seed(5)

  expect_false(identifyComponentModel(
    A = list(matrix(1, 2, 2)),
    C = list(matrix(1, 2, 2)),
    E = diag(1, 2)
  )$identified)
})

# Test that fitComponentModel returns expected coefficients with valid input data
test_that("fitComponentModel work", {
  # Initial example using commented out real data loading
  # data(twinData, package = "OpenMx")
  # selVars <- c("ht1", "ht2")
  # mzData <- subset(twinData, zyg %in% c(1), c(selVars, "zyg"))
  # dzData <- subset(twinData, zyg %in% c(3), c(selVars, "zyg"))
  # covmat = list(cov(mzData[, selVars], use = "pair"), cov(dzData[, selVars], use = "pair")),

  covmat <- list(
    matrix(c(
      0.004406810, 0.003872128,
      0.003872128, 0.004417694
    ), nrow = 2),
    matrix(c(
      0.004817455, 0.002051937,
      0.002051937, 0.004531923
    ), nrow = 2)
  )


  result <- fitComponentModel(
    covmat = covmat,
    A = list(matrix(1, nrow = 2, ncol = 2), matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)),
    C = list(matrix(1, nrow = 2, ncol = 2), matrix(1, nrow = 2, ncol = 2)),
    E = list(diag(1, nrow = 2), diag(1, nrow = 2))
  )
  expect_equal(result$coefficients, c(compmA = 0.0036404, compmC = 0.0002317, compmE = 0.0006713), tolerance = 1e-4)
})

# Test for incorrect input types in comp2vech
test_that("comp2vech handles incorrect input types", {
  expect_error(comp2vech("some non-matrix input"), "x is neither a list nor a matrix")
})

# Ensure default names are correctly assigned
test_that("default names are assigned correctly", {
  result <- identifyComponentModel(matrix(1, 2, 2), matrix(1, 2, 2))
  expect_equal(result$nidp, c("Comp1", "Comp2"))
})



# Test for list handling in comp2vech
test_that("comp2vech handles lists correctly", {
  list_input <- list(matrix(c(1, 0.5, 0.5, 1), 2, 2), matrix(1, 2, 2))
  expect_length(comp2vech(list_input, include.zeros = TRUE), 10)  # Adjust based on expected vector length
})


# Test for incorrect input types in comp2vech
test_that("comp2vech handles incorrect input types", {
  expect_error(comp2vech("some non-matrix input"), "x is neither a list nor a matrix")
})
