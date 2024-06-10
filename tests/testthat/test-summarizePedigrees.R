# Test Case 1: Counts the correct number of people for summarizeFamilies
test_that("Counts the correct number people", {
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  df_summarized <- summarizeFamilies(df, famID = "newFamID", personID = "personID")
  result_observed <- df_summarized$family_summary$count
  result_expected <- nrow(potter)
  expect_equal(result_observed, result_expected)
})


# Test Case 2: Multiple families
test_that("summarizeFamilies() works with multiple families", {
  df <- ped2fam(inbreeding, famID = "newFamID", personID = "ID")
  nbiggest <- 5
  df_summarized <- summarizeFamilies(df, famID = "newFamID", personID = "ID",nbiggest=nbiggest)
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

# Test Case 3: 5 number summary work on all the same variables?
test_that("summarizeFamilies() works with additional summary stats", {
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  df_summarized <- summarizeFamilies(df, famID = "newFamID", personID = "personID",five_num_summary=TRUE)
  # is the total count from the family summary the same as the raw data?
  names(df_summarized$family_summary)
  result_5num <- sum(grepl("_Q1", names(df_summarized$family_summary)),
                         grepl("_Q3", names(df_summarized$family_summary)))
  result_minmax <- sum(grepl("_max", names(df_summarized$family_summary)),
      grepl("_min", names(df_summarized$family_summary)))

  expect_equal(result_5num, result_minmax)

  # at minimum, we should have 4 columns a.k.a. they should all exist for at least one variable
  expect_gte(result_5num + result_minmax, 4)
})


# Test Case 4: Does this function work for summarizeMatrilines
test_that("summarizeMatrilines() works", {
  nbiggest <- 2
  df <- ped2fam(potter, famID = "newFamID", personID = "personID") %>% ped2maternal(personID = "personID")
  df_summarized <- summarizeMatrilines(df, famID = "newFamID", personID = "personID",nbiggest=nbiggest)
  # is the total count from the family summary the same as the raw data?
  result_observed <- sum(df_summarized$maternal_summary$count)
  result_expected <- nrow(potter)
  expect_equal(result_observed, result_expected)
  # is the count of the summarized data frame equal to the number of unique families in the input data frame?
  result_observed <- length(df_summarized$maternal_summary$count)
  result_expected <- length(unique(df$matID))
  expect_equal(result_observed, result_expected)
  # is the count of the biggest families equal to the number of unique families in the input data frame?
  result_observed <- nrow(df_summarized$biggest_maternal)
  expect_equal(result_observed, nbiggest)
})

# Test Case 5: Does this function work for summarizePatrilines
test_that("summarizePatrilines() works", {
  nbiggest <- 4
  df <- ped2fam(potter, famID = "newFamID", personID = "personID") %>% ped2paternal(personID = "personID")
  df_summarized <- summarizePatrilines(df, famID = "newFamID", personID = "personID",nbiggest=nbiggest)
  # is the total count from the family summary the same as the raw data?
  result_observed <- sum(df_summarized$paternal_summary$count)
  result_expected <- nrow(potter)
  expect_equal(result_observed, result_expected)
  # is the count of the summarized data frame equal to the number of unique families in the input data frame?
  result_observed <- length(df_summarized$paternal_summary$count)
  result_expected <- length(unique(df$patID))
  expect_equal(result_observed, result_expected)
  # is the count of the biggest families equal to the number of unique families in the input data frame?
  result_observed <- nrow(df_summarized$biggest_paternal)
  expect_equal(result_observed, nbiggest)
})
