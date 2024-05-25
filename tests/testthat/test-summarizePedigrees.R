# Test Case 1:
test_that("Counts the correct number people", {
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  df_summarized <-summarizeFamilies(df, famID = "newFamID", personID = "personID")
  result_observed <- df_summarized$family_summary$count
  result_expected <- nrow(potter)
  expect_equal(result_observed, result_expected)
})


# Test Case 2: Multiple families
test_that("summarizeFamilies() works with multiple families", {
  df <- ped2fam(inbreeding, famID = "newFamID", personID = "ID")
  nbiggest <- 5
  df_summarized <-summarizeFamilies(df, famID = "newFamID", personID = "ID",nbiggest=nbiggest)
  # is the total count from the family summary the same as the raw data?
  result_observed <- sum(df_summarized$family_summary$count)
  result_expected <- nrow(inbreeding)
  expect_equal(result_observed, result_expected)
  # is the count of the summarized data frame equal to the number of unique families in the input data frame?
  result_observed <- length(df_summarized$family_summary$count)
  result_expected <- length(unique(df$newFamID))
  expect_equal(result_observed, result_expected)
  # is the count of the biggest families equal to the number of unique families in the input data frame?
  result_observed <- nrow(df_summarized$biggest_families)
  expect_equal(result_observed, nbiggest)


})
