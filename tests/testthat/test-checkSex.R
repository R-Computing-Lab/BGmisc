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

  recoded_potter <- recodeSex(potter, code_female = 0, recode_male = "M", recode_female = "F")
  expect_true(all(recoded_potter$sex %in% c("M", "F")))
  expect_false(any(is.na(recoded_potter$sex)))

  recoded_potter <- recodeSex(potter, recode_male = "M", recode_female = "F")
  expect_false(all(recoded_potter$sex %in% c("M", "F")))
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


# Test Case 5: Handle code_unknown parameter with explicit value
test_that("recodeSex handles code_unknown parameter when explicitly provided", {
  # Create pedigree with unknown sex codes
  ped <- data.frame(
    ID = c(1, 2, 3, 4, 5, 6),
    sex = c("M", "F", "M", "F", "U", "U"),
    dadID = c(NA, NA, 1, 1, NA, NA),
    momID = c(NA, NA, 2, 2, NA, NA)
  )

  # Test with code_unknown = "U"
  recoded_ped <- recodeSex(ped,
    code_male = "M",
    code_female = "F",
    code_unknown = "U",
    recode_male = "Male",
    recode_female = "Female",
    recode_unknown = "Unknown"
  )

  # Check that unknown codes are recoded correctly
  expect_equal(recoded_ped$sex[5], "Unknown")
  expect_equal(recoded_ped$sex[6], "Unknown")
  expect_equal(recoded_ped$sex[1], "Male")
  expect_equal(recoded_ped$sex[2], "Female")
})


# Test Case 6: Handle code_unknown when it's NA
test_that("recodeSex handles code_unknown = NA correctly", {
  # Create pedigree where NA represents unknown sex
  ped <- data.frame(
    ID = c(1, 2, 3, 4, 5),
    sex = c("M", "F", "M", "F", NA),
    dadID = c(NA, NA, 1, 1, NA),
    momID = c(NA, NA, 2, 2, NA)
  )

  # Test with code_unknown = NA
  recoded_ped <- recodeSex(ped,
    code_male = "M",
    code_female = "F",
    code_unknown = NA,
    recode_male = "Male",
    recode_female = "Female",
    recode_unknown = "Unknown"
  )

  # Check that NA values are recoded to "Unknown"
  expect_equal(recoded_ped$sex[5], "Unknown")
  expect_equal(recoded_ped$sex[1], "Male")
  expect_equal(recoded_ped$sex[2], "Female")
})


# Test Case 7: Infer unknown values from data when code_unknown not provided
test_that("recodeSex infers unknown values when code_unknown is not provided", {
  # Create pedigree with values that are neither male nor female
  ped <- data.frame(
    ID = c(1, 2, 3, 4, 5, 6),
    sex = c("M", "F", "M", "F", "X", "?"),
    dadID = c(NA, NA, 1, 1, NA, NA),
    momID = c(NA, NA, 2, 2, NA, NA)
  )

  # Test without code_unknown - should infer "X" and "?" as unknown
  recoded_ped <- recodeSex(ped,
    code_male = "M",
    code_female = "F",
    recode_male = "Male",
    recode_female = "Female",
    recode_unknown = "Unknown"
  )

  # Check that values not in code_male/code_female are recoded to unknown
  expect_equal(recoded_ped$sex[5], "Unknown")
  expect_equal(recoded_ped$sex[6], "Unknown")
  expect_equal(recoded_ped$sex[1], "Male")
  expect_equal(recoded_ped$sex[2], "Female")
})


# Test Case 8: Test recode_unknown parameter variations
test_that("recodeSex respects recode_unknown parameter", {
  ped <- data.frame(
    ID = c(1, 2, 3, 4, 5),
    sex = c("M", "F", "M", "F", "U"),
    dadID = c(NA, NA, 1, 1, NA),
    momID = c(NA, NA, 2, 2, NA)
  )

  # Test with custom recode_unknown value
  recoded_ped <- recodeSex(ped,
    code_male = "M",
    code_female = "F",
    code_unknown = "U",
    recode_male = "1",
    recode_female = "0",
    recode_unknown = "9"
  )

  expect_equal(recoded_ped$sex[5], "9")
  expect_equal(recoded_ped$sex[1], "1")
  expect_equal(recoded_ped$sex[2], "0")
})


# Test Case 9: Test code_unknown with only code_male provided
test_that("recodeSex handles code_unknown with only code_male", {
  ped <- data.frame(
    ID = c(1, 2, 3, 4),
    sex = c("M", "F", "M", "U"),
    dadID = c(NA, NA, 1, NA),
    momID = c(NA, NA, 2, NA)
  )

  # Test with only code_male and code_unknown
  recoded_ped <- recodeSex(ped,
    code_male = "M",
    code_unknown = "U",
    recode_male = "Male",
    recode_female = "Female",
    recode_unknown = "Unknown"
  )

  # Check recoding: M->Male, F->Female (inferred), U->Unknown
  expect_equal(recoded_ped$sex[1], "Male")
  expect_equal(recoded_ped$sex[2], "Female")
  expect_equal(recoded_ped$sex[4], "Unknown")
})


# Test Case 10: Test code_unknown with only code_female provided
test_that("recodeSex handles code_unknown with only code_female", {
  ped <- data.frame(
    ID = c(1, 2, 3, 4),
    sex = c("M", "F", "F", "U"),
    dadID = c(NA, NA, NA, NA),
    momID = c(NA, NA, NA, NA)
  )

  # Test with only code_female and code_unknown
  recoded_ped <- recodeSex(ped,
    code_female = "F",
    code_unknown = "U",
    recode_male = "Male",
    recode_female = "Female",
    recode_unknown = "Unknown"
  )

  # Check recoding: F->Female, M->Male (inferred), U->Unknown
  expect_equal(recoded_ped$sex[1], "Male")
  expect_equal(recoded_ped$sex[2], "Female")
  expect_equal(recoded_ped$sex[4], "Unknown")
})


# Test Case 11: Test numeric codes with code_unknown
test_that("recodeSex handles numeric code_unknown values", {
  ped <- data.frame(
    ID = c(1, 2, 3, 4, 5),
    sex = c(1, 0, 1, 0, 9),
    dadID = c(NA, NA, 1, 1, NA),
    momID = c(NA, NA, 2, 2, NA)
  )

  # Test with numeric codes
  recoded_ped <- recodeSex(ped,
    code_male = 1,
    code_female = 0,
    code_unknown = 9,
    recode_male = "M",
    recode_female = "F",
    recode_unknown = "U"
  )

  expect_equal(recoded_ped$sex[5], "U")
  expect_equal(recoded_ped$sex[1], "M")
  expect_equal(recoded_ped$sex[2], "F")
})
