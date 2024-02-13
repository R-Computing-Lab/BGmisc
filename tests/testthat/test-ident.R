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

test_that("underidentified model is unidentified", {
  set.seed(5)

  expect_false(identifyComponentModel(
    A = list(matrix(1, 2, 2)),
    C = list(matrix(1, 2, 2)),
    E = diag(1, 2)
  )$identified)
})

test_that("fitComponentModel work", {
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
