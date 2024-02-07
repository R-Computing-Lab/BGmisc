test_that("Check Sex repair functionality", {
  data(potter)
  df_fam <-  potter
  df_fam$sex[df_fam$name == "Vernon Dursley"] <- 0  # Incorrect sex coding

  # Check if Vernon Dursley's sex is correctly repaired to "M"
  result <-   checkSex(
    df_fam,
    code_male = 1,
    verbose = FALSE,
    repair = TRUE
  )
  expect_equal(result[[2]]$sex[result[[2]][["name"]] == "Vernon Dursley"], "M")
})


# Test Case 1: Recoding sex variable without repair
test_that("Sex variable is correctly recoded without repair", {
  data(potter)
  df_fam <- potter
  df_fam$sex[df_fam$sex==1] <- "F"
  df_fam$sex[df_fam$sex==0] <- "M"
  expected_ped <- potter
  #df_recode <- checkSex(df_fam, code_male = "M", recode = TRUE, repair = FALSE)
  df_repair <- checkSex(df_fam, code_male = "M", recode = TRUE, repair = TRUE)
  expect_equal(df_repair, expected_ped)
})
#
# # Test Case 2: Verbose output
# test_that("Verbose output is correct", {
#   ped <- data.frame(ID = 1:3, sex = c("M", "F", "M"), stringsAsFactors = FALSE)
#   # This test assumes capture of output, adjust as necessary for your testing environment
#   expect_output(checkSex(ped, code_male = "M", verbose = TRUE, repair = FALSE), "unique values found.")
# })
#
# # Test Case 3: Return value on repair
# test_that("Function returns a repaired dataframe when repair is TRUE", {
#   ped <- data.frame(ID = 1:3, sex = c("M", "F", "?"), stringsAsFactors = FALSE)
#   result_ped <- checkSex(ped, code_male = "M", repair = TRUE, recode = FALSE)
#   expect_true(all(result_ped$sex %in% c("M", "F")))
# })
#
# # Test Case 4: Handling of NA values
# test_that("NA values in 'sex' column are preserved", {
#   ped <- data.frame(ID = 1:4, sex = c("M", "F", NA, "M"), stringsAsFactors = FALSE)
#   result_ped <- checkSex(ped, code_male = "M", repair = FALSE, recode = TRUE)
#   expect_equal(is.na(result_ped$sex[3]), TRUE)
# })
#
# # Test Case 5: Repair and recode simultaneously
# test_that("Sex coding is correctly repaired and recoded", {
#   ped <- data.frame(ID = 1:3, sex = c("1", "2", "1"), stringsAsFactors = FALSE, dadID = c(NA, 1, 2))
#   expected_ped <- data.frame(ID = 1:3, sex = c("M", "F", "M"), stringsAsFactors = FALSE)
#   result_ped <- checkSex(ped, code_male = "1", verbose = FALSE, repair = TRUE, recode = TRUE)
#   expect_equal(result_ped, expected_ped)
# })
