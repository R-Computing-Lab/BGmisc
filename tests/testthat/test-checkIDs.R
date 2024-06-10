# Test checkIDs function
test_that("checkIDs on cleaned data set", {
  # Create a sample dataset
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")

  # Call the checkIDs function, should report that ids are unique
  result <- checkIDs(df, repair = FALSE)
  expect_equal(result$all_unique_ids, TRUE)
  expect_equal(result$total_non_unique_ids, 0)
})
test_that("checkIDs with a between-person duplicate", {
  # Create a sample dataset
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  df$personID[df$name == "Vernon Dursley"] <- df$personID[df$name == "Marjorie Dursley"]
  result <- checkIDs(df, repair = FALSE)
  expect_equal(result$all_unique_ids, FALSE)
  expect_equal(result$non_unique_ids, 2)
  expect_equal(result$total_non_unique_ids, 2)
  #  df$momID[df$name=="Vernon Dursley"]<-df$dadID[df$name=="Vernon Dursley"]
})

test_that("checkIDs with a within-person duplicate mom", {
  # Create a sample dataset
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")

  df$momID[df$name == "Vernon Dursley"] <- df$personID[df$name == "Vernon Dursley"]
  df$momID[df$name == "Marjorie Dursley"] <- df$personID[df$name == "Marjorie Dursley"]
  result <- checkIDs(df, repair = FALSE)
  expect_equal(result$all_unique_ids, TRUE)
  expect_null(result$non_unique_ids)
  expect_equal(result$total_non_unique_ids, 0)
  expect_equal(result$within_row_duplicates, TRUE)
  expect_equal(result$total_within_row_duplicates, 2)
  expect_equal(result$total_own_mother, result$total_within_row_duplicates)
  expect_equal(result$is_own_mother_ids, c(1, 2))
  expect_null(result$is_own_father_ids)
})


test_that("checkIDs with a within-person duplicate dad", {
  # Create a sample dataset
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")

  df$dadID[df$name == "Vernon Dursley"] <- df$personID[df$name == "Vernon Dursley"]
  df$dadID[df$name == "Marjorie Dursley"] <- df$personID[df$name == "Marjorie Dursley"]
  result <- checkIDs(df, repair = FALSE)
  expect_equal(result$all_unique_ids, TRUE)
  expect_null(result$non_unique_ids)
  expect_equal(result$total_non_unique_ids, 0)
  expect_equal(result$within_row_duplicates, TRUE)
  expect_equal(result$total_within_row_duplicates, 2)
  expect_equal(result$total_own_father, result$total_within_row_duplicates)
  expect_equal(result$is_own_father_ids, c(1, 2))
  expect_null(result$is_own_mother_ids)
})

test_that("repair with a between-person duplicate", {
  # Create a sample dataset
  df <- standardizeColnames(ped2fam(potter, famID = "newFamID", personID = "personID"))
  df_bound <- rbind(df, df[df$name == "Vernon Dursley", ])
  result <- checkIDs(df_bound, repair = TRUE)
  expect_equal(df, result)
})





test_that("checkIDs verbose prints updates", {
  #  skip_on_cran(message = "Skipping test that only checks for verbose output")
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  df_bound <- rbind(df, df[df$name == "Vernon Dursley", ])
  expect_output(checkIDs(df, verbose = TRUE, repair = TRUE),
    regexp = "Changes Made:\\nlist\\(\\)"
  )
  expect_output(checkIDs(df_bound, verbose = TRUE, repair = TRUE),
    regexp = "Changes Made:\\n\\$ID1\\n\\[1\\] "
  )
})
