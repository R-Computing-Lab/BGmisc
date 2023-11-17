# Test that cleaning functions work
test_that("Check Sex", {
  data(potter)
  df_fam <- potter
  df_fam$sex[df_fam$name=="Vernon Dursley"] <-0

  checkSex(
    df_fam,
    code_male = 1,
    verbose = TRUE,
    repair = FALSE
  )

})
