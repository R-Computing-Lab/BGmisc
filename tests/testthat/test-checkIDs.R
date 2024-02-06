# Test checkIDs function
test_that("checkIDs on cleaned data set", {
  # Create a sample dataset
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")

  # Call the checkIDs function, should report that ids are unique
  result <- checkIDs(df, repair = FALSE)
  expect_equal(result$all_unique_ids, TRUE)
  expect_equal(result$total_non_unique_ids, 0)
})
test_that("checkIDs with a duplicate", {
  # Create a sample dataset
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  df$personID[df$name == "Vernon Dursley"] <- df$personID[df$name == "Marjorie Dursley"]
  result <- checkIDs(df, repair = FALSE)
  expect_equal(result$all_unique_ids, FALSE)
  expect_equal(result$non_unique_ids, 2)
  expect_equal(result$total_non_unique_ids, 2)
  #  df$momID[df$name=="Vernon Dursley"]<-df$dadID[df$name=="Vernon Dursley"]
})

test_that("repair with a duplicate", {
  # Create a sample dataset
  df <- standardize_colnames(ped2fam(potter, famID = "newFamID", personID = "personID"))
  df_bound <- rbind(df,df[df$name == "Vernon Dursley",])
  result <- checkIDs(df_bound, repair = TRUE)
  expect_equal(df, result)
})

test_that("checkIDs verbose prints updates", {
  df <- ped2fam(potter, famID = "newFamID", personID = "personID")
  df_bound <- rbind(df,df[df$name == "Vernon Dursley",])
  expect_output(checkIDs(df, verbose = TRUE,repair=TRUE),
                regexp = "Step 1: Checking for unique IDs")
  expect_output(checkIDs(df_bound, verbose = TRUE,repair=TRUE),
                regexp = "Step 2: Attempting to repair non-unique IDs")
})
