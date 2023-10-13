test_that("identified model is identified", {
  set.seed(5)

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
