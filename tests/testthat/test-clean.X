# Test that cleaning functions work
test_that("Check Sex", {
  data(potter)
  df_fam <- potter
  df_fam$sex[df_fam$name=="Vernon Dursley"] <- 0

  result <-   checkSex(
    df_fam,
    code_male = 1,
    verbose = FALSE,
    repair = TRUE
  )
  expect_equal(result[[2]]$sex[result[[2]][["name"]]=="Vernon Dursley"], "M", tolerance = 1e-8)
})

