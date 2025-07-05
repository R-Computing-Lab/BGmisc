# Test Case 1: Validate sex coding without repair
test_that("checkParentIDs identifies parent coding correctly in potter dataset", {
  results <- checkParentIDs(potter, verbose = TRUE, repair = FALSE)
  expect_false("parents_in_both" %in% names(results))
  expect_false(results$single_parents)
  expect_false(results$missing_parents)
  expect_true(results$female_moms)
  expect_true(results$male_dads)
  expect_equal(results$female_var, 0)
  expect_equal(results$male_var, 1)
  expect_equal(results$mom_sex, 0)
  expect_equal(results$dad_sex, 1)
})

# Test Case 2: Validate sex coding without repair
test_that("checksif single parents found correctly in ASOIAF dataset", {
 library(ggpedigree)
  data(ASOIAF)
  df_asoiaf <- ASOIAF
  results <- checkParentIDs(df_asoiaf, verbose = FALSE, repair = FALSE, personID = "id")
  expect_true(results$single_parents)
  single_dads <- length(df_asoiaf$id[!is.na(df_asoiaf$dadID) & is.na(df_asoiaf$momID)])
  single_moms <- length(df_asoiaf$id[is.na(df_asoiaf$dadID) & !is.na(df_asoiaf$momID)])
  expect_equal(single_moms, length(results$missing_fathers))
  expect_equal(single_dads, length(results$missing_mothers))
  repaired_df <- checkParentIDs(df_asoiaf, verbose = FALSE, repair = TRUE, parentswithoutrow = TRUE)
  expect_equal(nrow(repaired_df), nrow(df_asoiaf) + single_moms + single_dads)
})

test_that("verbose checks", {
  library(ggpedigree)
  data(ASOIAF)
  df_asoiaf <- ASOIAF
  expect_message(checkParentIDs(df_asoiaf, verbose = TRUE, repair = TRUE))
  expect_message(repairParentIDs(df_asoiaf, verbose = TRUE))
  expect_message(checkParentIDs(df_asoiaf, verbose = TRUE, parentswithoutrow = TRUE))
})
