test_that("ped2addFocal returns pedigree with new column", {
  data(hazard)
  result <- ped2addFocal(hazard, focal_id = 1)
  expect_true(is.data.frame(result))
  expect_true("additiveRel_1" %in% colnames(result))
  expect_equal(nrow(result), nrow(hazard))
})

test_that("ped2addFocal default column name encodes focal_id", {
  data(hazard)
  result <- ped2addFocal(hazard, focal_id = 1)
  expect_true("additiveRel_1" %in% colnames(result))

  result2 <- ped2addFocal(hazard, focal_id = 2)
  expect_true("additiveRel_2" %in% colnames(result2))
})

test_that("ped2addFocal respects custom col_name", {
  data(hazard)
  result <- ped2addFocal(hazard, focal_id = 1, col_name = "rel_to_focal")
  expect_true("rel_to_focal" %in% colnames(result))
  expect_false("additiveRel_1" %in% colnames(result))
})

test_that("ped2addFocal errors when focal_id not in pedigree", {
  data(hazard)
  expect_error(
    ped2addFocal(hazard, focal_id = 9999),
    "focal_id '9999' not found"
  )
})

test_that("ped2addFocal focal person self-relatedness is preserved from matrix diagonal", {
  data(hazard)
  result <- ped2addFocal(hazard, focal_id = 1)
  mat    <- ped2add(hazard, sparse = FALSE)
  expect_equal(
    result$additiveRel_1[result$ID == 1],
    mat["1", "1"]
  )
})

test_that("ped2addFocal produces correct parent-child relatedness", {
  data(hazard)
  # IDs 3, 4, 5 are children of parents 1 and 2 in hazard
  result <- ped2addFocal(hazard, focal_id = 1)
  expect_equal(result$additiveRel_1[result$ID == 3], 0.5)
  expect_equal(result$additiveRel_1[result$ID == 4], 0.5)
  expect_equal(result$additiveRel_1[result$ID == 5], 0.5)
})

test_that("ped2addFocal unrelated individuals get zero when no keep_ids supplied", {
  data(hazard)
  result <- ped2addFocal(hazard, focal_id = 1)
  # No NAs when keep_ids is not supplied
  expect_true(all(!is.na(result$additiveRel_1)))
  # All values non-negative
  expect_true(all(result$additiveRel_1 >= 0))
})

test_that("ped2addFocal values match corresponding column of full matrix", {
  data(hazard)
  focal    <- 1
  result   <- ped2addFocal(hazard, focal_id = focal)
  mat      <- ped2add(hazard, sparse = FALSE)
  col_name <- paste0("additiveRel_", focal)

  # Check each individual in the matrix matches
  for (id in rownames(mat)) {
    expect_equal(
      result[[col_name]][result$ID == as.integer(id)],
      mat[id, as.character(focal)],
      label = paste("ID", id)
    )
  }
})

test_that("ped2addFocal excluded individuals get NA when keep_ids supplied", {
  data(hazard)
  kept_ids <- hazard$ID[1:10]
  result   <- ped2addFocal(hazard, focal_id = 1, keep_ids = kept_ids)

  excluded_rows <- !result$ID %in% kept_ids
  expect_true(all(is.na(result$additiveRel_1[excluded_rows])))
})

test_that("ped2addFocal included individuals are not NA when keep_ids supplied", {
  data(hazard)
  kept_ids <- hazard$ID[1:10]
  result   <- ped2addFocal(hazard, focal_id = 1, keep_ids = kept_ids)

  included_rows <- result$ID %in% kept_ids
  expect_true(all(!is.na(result$additiveRel_1[included_rows])))
})

test_that("ped2focal works for mitochondrial component", {
  data(hazard)
  result <- ped2focal(hazard, component = "mitochondrial", focal_id = 1)
  expect_true("mitochondrialRel_1" %in% colnames(result))
  expect_true(all(!is.na(result$mitochondrialRel_1)))
  expect_true(all(result$mitochondrialRel_1 %in% c(0, 1)))
})

test_that("ped2focal errors when focal_id not in pedigree", {
  data(hazard)
  expect_error(
    ped2focal(hazard, component = "additive", focal_id = 9999),
    "focal_id '9999' not found"
  )
})

test_that("ped2addFocal works with potter using personID column", {
  result <- ped2addFocal(potter, focal_id = 1, personID = "personID")
  expect_true(is.data.frame(result))
  expect_true("additiveRel_1" %in% colnames(result))
  expect_equal(nrow(result), nrow(potter))
  # Self-relatedness preserved
  mat <- ped2add(potter, standardize_colnames = TRUE)
  expect_equal(
    result$additiveRel_1[result$personID == 1],
    mat["1", "1"]
  )
})
