# Test that cleaning functions work
test_that("Check Sex", {
  data(potter)
  df_fam <- potter
  df_fam$sex[df_fam$name=="Vernon Dursley"] <-0

  checkSex(
    df_fam,
    code_male = 1,
    verbose = TRUE,
    repair = TRUE
  )


  result <- calculateRelatedness(generations = 1, full = TRUE)
  expect_equal(result, 0.5, tolerance = 1e-8)
})

test_that("calculateRelatedness function for half siblings", {
  result <- calculateRelatedness(generations = 1, full = FALSE)
  expect_equal(result, 0.25, tolerance = 1e-8)
})
