# Test Case 1: Counts the correct number of people for summarizeFamilies
test_that("Counts the correct number people", {
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  df_summarized <- summarizeFamilies(df, famID = "newFamID", personID = "personID")
  result_observed <- df_summarized$family_summary$count
  result_expected <- nrow(potter)
  expect_equal(result_observed, result_expected)
})

# Test: SummarizeFamilies is used when SummariseFamilies
test_that("SummarizeFamilies works like SummariseFamilies", {
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  df_summarized <- summarizeFamilies(df, famID = "newFamID", personID = "personID")
  df_summarised <- summariseFamilies(df, famID = "newFamID", personID = "personID")
  expect_equal(df_summarised, df_summarized)
})
# Test Case 2: Multiple families
test_that("summarizeFamilies() works with multiple families", {
  df <- ped2fam(inbreeding, famID = "newFamID", personID = "ID")
  n_biggest <- 5
  df_summarized <- summarizeFamilies(df, famID = "newFamID", personID = "ID", n_biggest = n_biggest)
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
  expect_equal(result_observed, n_biggest)
})

# Test Case 3: 5 number summary work on all the same variables?
test_that("summarizeFamilies() works with additional summary stats", {
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  df_summarized <- summarizeFamilies(df, famID = "newFamID", personID = "personID", five_num_summary = TRUE)
  # is the total count from the family summary the same as the raw data?
  names(df_summarized$family_summary)
  result_5num <- sum(
    grepl("_Q1", names(df_summarized$family_summary)),
    grepl("_Q3", names(df_summarized$family_summary))
  )
  result_minmax <- sum(
    grepl("_max", names(df_summarized$family_summary)),
    grepl("_min", names(df_summarized$family_summary))
  )

  expect_equal(result_5num, result_minmax)

  # at minimum, we should have 4 columns a.k.a. they should all exist for at least one variable
  expect_gte(result_5num + result_minmax, 4)
})


# Test Case 4: Does this function work for summarizeMatrilines
test_that("summarizeMatrilines() works", {
  n_biggest <- 2
  df <- ped2fam(potter, famID = "newFamID", personID = "personID") %>%
    ped2maternal(personID = "personID")
  df_summarized <- summarizeMatrilines(df,
    famID = "newFamID",
    personID = "personID",
    n_biggest = n_biggest
  )
  # is the total count from the family summary the same as the raw data?
  result_observed <- sum(df_summarized$maternal_summary$count)
  result_expected <- nrow(potter)
  expect_equal(result_observed, result_expected)
  # is the count of the summarized data frame equal to the number of
  # unique families in the input data frame?
  result_observed <- length(df_summarized$maternal_summary$count)
  result_expected <- length(unique(df$matID))
  expect_equal(result_observed, result_expected)
  # is the count of the biggest families equal to the number of
  # unique families in the input data frame?
  result_observed <- nrow(df_summarized$biggest_maternal)
  expect_equal(result_observed, n_biggest)
})
# Test: SummarizeMatrilines is used when SummariseMatrilines
test_that("SummarizeMatrilines works like SummariseMatrilines", {
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  df_summarized <- summarizeMatrilines(df,
    famID = "newFamID", personID = "personID",
    verbose = TRUE
  )
  df_summarised <- summariseMatrilines(df,
    famID = "newFamID",
    personID = "personID",
    verbose = TRUE
  )
  expect_equal(df_summarised, df_summarized)
})
# Test Case 5: Does this function work for summarizePatrilines
test_that("summarizePatrilines() works", {
  n_biggest <- 4
  df <- ped2fam(potter, famID = "newFamID", personID = "personID") %>%
    ped2paternal(personID = "personID")
  df_summarized <- summarizePatrilines(df,
    famID = "newFamID",
    personID = "personID",
    n_biggest = n_biggest,
    verbose = TRUE
  )
  # is the total count from the family summary the same as the raw data?
  result_observed <- sum(df_summarized$paternal_summary$count)
  result_expected <- nrow(potter)
  expect_equal(result_observed, result_expected)
  # is the count of the summarized data frame equal to the number of
  # unique families in the input data frame?
  result_observed <- length(df_summarized$paternal_summary$count)
  result_expected <- length(unique(df$patID))
  expect_equal(result_observed, result_expected)
  # is the count of the biggest families equal to the number of
  # unique families in the input data frame?
  result_observed <- nrow(df_summarized$biggest_paternal)
  expect_equal(result_observed, n_biggest)
})

# Test: summarizePatrilines is used when SummarisePatrilines
test_that("summarizePatrilines works like SummarisePatrilines", {
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  df_summarized <- summarizePatrilines(df, famID = "newFamID", personID = "personID")
  df_summarised <- summarisePatrilines(df, famID = "newFamID", personID = "personID")
  expect_equal(df_summarised, df_summarized)
})
# Test Case 6: Handling of missing values in critical columns
test_that("summarizePedigrees() handles missing values correctly", {
  df <- data.frame(
    ID = 1:6,
    momID = c(NA, 1, 1, NA, 4, 4),
    dadID = c(NA, 2, 2, NA, 5, 5),
    famID = c(1, 1, 1, 2, 2, 2),
    byr = c(1920, 1945, 1950, 1930, 1960, 1965)
  )

  df_summarized <- summarizePedigrees(df, byr = "byr")

  expect_true(!any(is.na(df_summarized$family_summary$count)))
  expect_true(!any(is.na(df_summarized$oldest_families$byr)))
})

# Test Case 7: When all variables are skipped
test_that("summarizePedigrees works when all numeric variables are skipped", {
  df <- data.frame(
    ID = 1:5,
    momID = c(NA, 1, 1, NA, 4),
    dadID = c(NA, 2, 2, NA, 5),
    famID = c(1, 1, 1, 2, 2),
    age = c(30, 40, 50, 60, 70)
  )

  df_summarized <- summarizePedigrees(df, skip_var = c("age"), verbose = TRUE)
  expect_true(all(!grepl("age", names(df_summarized$family_summary))))
})

# Test Case 8: Handling invalid column names
test_that("summarizePedigrees() throws error on invalid column names", {
  df <- data.frame(
    ID = 1:5, momID = c(NA, 1, 1, NA, 4),
    dadID = c(NA, 2, 2, NA, 5), famID = c(1, 1, 1, 2, 2)
  )
  expect_error(summarizePedigrees(df, byr = "unknown_column"))
})


# Test Case 9: Handling empty dataset
# test_that("summarizePedigrees() handles empty dataset gracefully", {
#  df <- data.frame(ID = integer(), momID = integer(), dadID = integer(), famID = integer())
#  df_summarized <- summarizePedigrees(df)
#  expect_true(length(df_summarized) == 0) # if the function were graceful...
#  expect_true(all(sapply(df_summarized, function(x) is.null(x) || (is.data.frame(x) && nrow(x) == 0))))
#  expect_false("biggest_families" %in% names(df_summarized) && nrow(df_summarized$biggest_families) > 0)
#  expect_false("biggest_maternal" %in% names(df_summarized) && nrow(df_summarized$biggest_maternal) > 0)
#  expect_false("biggest_paternal" %in% names(df_summarized) && nrow(df_summarized$biggest_paternal) > 0)
# })

# Test Case 10: Handling single entry pedigree
test_that("summarizePedigrees() works for single-entry pedigree", {
  df <- data.frame(ID = 1, momID = NA, dadID = NA, famID = 1, byr = 1920)
  df_summarized <- summarizePedigrees(df, byr = "byr", verbose = TRUE)
  expect_equal(nrow(df_summarized$family_summary), 1)


  expect_equal(df_summarized$oldest_families$byr_mean, 1920)
})

# network check
test_that("summarizePedigrees() works for network pedigree", {
  df <- data.frame(
    ID = 1:5,
    momID = c(NA, 1, 1, NA, 4),
    dadID = c(NA, 2, 2, NA, 5),
    famID = c(1, 1, 1, 2, 2),
    byr = c(1922, 1945, 1950, 1930, 1960)
  )

  df_summarized <- summarizePedigrees(df, byr = "byr", network_checks = TRUE)
  expect_equal(nrow(df_summarized$family_summary), 2)
  expect_equal(df_summarized$family_summary$byr_mean, c(1939, 1945))
})

# Test: summarizePedigrees is used when SummarisePedigrees
test_that("SummarizePedigrees works like SummarisePedigrees", {
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  df_summarized <- summarizePedigrees(df, famID = "newFamID", personID = "personID")
  df_summarised <- summarisePedigrees(df, famID = "newFamID", personID = "personID")
  expect_equal(df_summarised, df_summarized)
})


# these need to be tests: summarizePedigrees <- function(ped, famID = "famID", personID = "ID",


test_that("SummarizePedigrees data fast fails", {
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  expect_error(summarizePedigrees(df, famID = "newFamID", personID = "perxsonID"))
  expect_error(summarizePedigrees(df, famID = "newFamID", personID = "momID"))
  expect_error(summarizePedigrees(df %>% select(-momID), famID = "newFamID", personID = "personID"))
  expect_error(summarizePedigrees(df, famID = "newFamID", personID = "personID", verbose = TRUE, type = NULL))

  expect_error(summarizePedigrees(df, famID = "newFamID", personID = "personID", founder_sort_var = "NOTHERE"))
})

test_that("SummarizePedigrees verboses", {
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")

  expect_message(summarizeFamilies(df, famID = "newFamID", personID = "personID", verbose = TRUE))
  expect_message(summarizeMatrilines(df, famID = "newFamID", personID = "personID", verbose = TRUE))
  expect_message(summarizePatrilines(df, famID = "newFamID", personID = "personID", verbose = TRUE))

  expect_message(
    summarizePedigrees(df,
      famID = "newFamID", personID = "personID",
      verbose = TRUE,
      network_checks = TRUE, type = c("families")
    )
  )
  expect_message(
    summarizePedigrees(df,
      famID = "newFamID",
      personID = "personID",
      verbose = TRUE,
      network_checks = TRUE,
      type = c("matrilines")
    )
  )
  expect_message(
    summarizePedigrees(df,
      famID = "newFamID",
      personID = "personID",
      verbose = TRUE,
      network_checks = TRUE,
      type = c("patrilines")
    )
  )
})
