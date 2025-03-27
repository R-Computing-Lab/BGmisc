# Test Case 1: Validate sex coding without repair
test_that("checkSex identifies sex coding correctly in potter dataset", {
  results <- checkSex(potter,
    code_male = 1,
    code_female = 0, verbose = TRUE, repair = FALSE
  )
  expect_true("sex_unique" %in% names(results))
  expect_equal(results$sex_unique, c(1, 0))
  expect_equal(results$sex_length, 2)
})

# Test Case 2: Validate sex coding without repair
test_that("checkSex identifies potentially problematic sex coding of non-male dad in potter dataset", {
  df_potter <- potter
  df_potter$sex[df_potter$name == "Vernon Dursley"] <- 5

  results <- checkSex(df_potter,
    code_male = 1,
    code_female = 0, verbose = TRUE, repair = FALSE
  )
  expect_true("sex_unique" %in% names(results))

  expect_equal(sort(results$sex_unique), sort(c(5, 1, 0)))

  expect_equal(results$sex_length, 3)

  expect_equal(results[["ID_female_dads"]], df_potter$personID[df_potter$name == "Vernon Dursley"])


df_fix <- checkSex(df_potter,
    code_male = 1,
    code_female = 0, verbose = TRUE, repair = TRUE
  )
  expect_true(dim(df_fix)[1] == dim(df_potter)[1])
  expect_true(dim(df_fix)[2] == dim(df_potter)[2])
})

# Test Case 2: Validate sex coding without repair
test_that("checkSex identifies potentially problematic sex coding of non-female mom in potter dataset", {
  df_potter <- potter
  df_potter$sex[df_potter$name == "Petunia Evans"] <- 5

  results <- checkSex(df_potter,
                      code_male = 1,
                      code_female = 0, verbose = TRUE, repair = FALSE
  )
  expect_true("sex_unique" %in% names(results))

  expect_equal(sort(results$sex_unique), sort(c(5, 1, 0)))

  expect_equal(results$sex_length, 3)

  expect_equal(results[["ID_male_moms"]], df_potter$personID[df_potter$name == "Petunia Evans"])

})



# Test Case 3: Recode sex variable
test_that("recodeSex correctly recodes sex in potter dataset", {
  recoded_potter <- recodeSex(potter, code_male = 1, code_female = 0, recode_male = "M", recode_female = "F")
  expect_true(all(recoded_potter$sex %in% c("M", "F")))
  expect_false(any(is.na(recoded_potter$sex)))
})



# Test Case 4: Handle missing values
test_that("Functions handle missing values gracefully", {
  ped <- data.frame(
    ID = c(1, 2, 3, 4, 5),
    sex = c("M", "F", "M", "F", NA),
    dadID = c(NA, NA, 1, 1, 3),
    momID = c(NA, NA, 2, 2, 4)
  )

  ped_with_na <- ped
  ped_with_na$sex <- NA
  expect_silent(checkSex(ped_with_na, code_male = "M", verbose = FALSE, repair = FALSE))
  expect_silent(repairSex(ped_with_na, verbose = FALSE, code_male = "M"))
  expect_silent(recodeSex(ped_with_na, verbose = FALSE, code_male = "M", code_female = "F"))
})
