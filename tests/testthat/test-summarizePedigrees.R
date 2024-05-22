# Test Case 1:
test_that("Counts the correct number people", {
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  result_observed <- summarizeFamilies(df, famID = "newFamID", personID = "personID" )$family_summary$count
  result_expected <- nrow(potter)
  expect_equal(result_observed, result_expected)
})

