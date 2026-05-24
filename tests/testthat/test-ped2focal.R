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
  mat <- ped2add(hazard, sparse = FALSE)
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

test_that("ped2addFocal values are correctly aligned to pedigree rows", {
  data(hazard)
  focal <- hazard$ID[sample(nrow(hazard), 1)] # choose random focal to maximize chance of misalignment if it occurs
  result <- ped2addFocal(hazard, focal_id = focal)
  mat <- ped2add(hazard, sparse = FALSE)
  col_name <- paste0("additiveRel_", focal)

  # For every row in the pedigree, confirm the focal column value matches
  # mat[that person's ID, focal_id] — explicitly checking alignment not just values
  for (i in seq_len(nrow(result))) {
    id <- as.character(result$ID[i])
    expect_equal(
      result[[col_name]][i],
      mat[id, as.character(focal)],
      label = paste("row", i, "ID", id)
    )
  }
})

test_that("ped2focal mitochondrial values are correctly aligned to pedigree rows", {
  data(hazard)
  focal <- hazard$ID[sample(nrow(hazard), 1)] # choose random focal to maximize chance of misalignment if it occurs
  result <- ped2focal(hazard, component = "mitochondrial", focal_id = focal)
  mat <- ped2mit(hazard, sparse = FALSE)
  col_name <- paste0("mitochondrialRel_", focal)

  for (i in seq_len(nrow(result))) {
    id <- as.character(result$ID[i])
    expect_equal(
      result[[col_name]][i],
      mat[id, as.character(focal)],
      label = paste("row", i, "ID", id)
    )
  }
})

test_that("ped2addFocal alignment holds when pedigree rows are shuffled", {
  data(hazard)
  focal <- hazard$ID[nrow(hazard)] # choose focal from last row to maximize chance of misalignment if it occurs
  hazard_shuffled <- hazard[sample(nrow(hazard)), ]
  result <- ped2addFocal(hazard_shuffled, focal_id = focal)
  mat <- ped2add(hazard, sparse = FALSE)
  col_name <- paste0("additiveRel_", focal)

  for (i in seq_len(nrow(result))) {
    id <- as.character(result$ID[i])
    expect_equal(
      result[[col_name]][i],
      mat[id, as.character(focal)],
      label = paste("shuffled row", i, "ID", id)
    )
  }
})

test_that("ped2addFocal excluded individuals get NA when keep_ids supplied", {
  data(hazard)
  kept_ids <- hazard$ID[1:10]
  result <- ped2addFocal(hazard, focal_id = 1, keep_ids = kept_ids)

  excluded_rows <- !result$ID %in% kept_ids
  expect_true(all(is.na(result$additiveRel_1[excluded_rows])))
})

test_that("ped2addFocal included individuals are not NA when keep_ids supplied", {
  data(hazard)
  kept_ids <- hazard$ID[1:10]
  result <- ped2addFocal(hazard, focal_id = 1, keep_ids = kept_ids)

  included_rows <- result$ID %in% kept_ids
  exclude_rows <- !included_rows
  expect_true(all(!is.na(result$additiveRel_1[included_rows])))
  expect_true(all(result$additiveRel_1[included_rows] >= 0))
  expect_true(all(result$additiveRel_1[included_rows] <= 1))
  expect_true(all(is.na(result$additiveRel_1[exclude_rows])))
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

test_that("ped2addFocal values are correctly aligned to pedigree rows for inbreeding dataset", {
  data(inbreeding)
  focal <- 1
  result <- ped2addFocal(inbreeding, focal_id = focal)
  mat <- ped2add(inbreeding, sparse = FALSE)
  col_name <- paste0("additiveRel_", focal)

  for (i in seq_len(nrow(result))) {
    id <- as.character(result$ID[i])
    expect_equal(
      result[[col_name]][i],
      mat[id, as.character(focal)],
      label = paste("inbreeding row", i, "ID", id)
    )
  }
})

test_that("ped2focal mitochondrial values are correctly aligned for inbreeding dataset", {
  data(inbreeding)
  focal <- 1
  result <- ped2focal(inbreeding, component = "mitochondrial", focal_id = focal)
  mat <- ped2mit(inbreeding, sparse = FALSE)
  col_name <- paste0("mitochondrialRel_", focal)

  for (i in seq_len(nrow(result))) {
    id <- as.character(result$ID[i])
    expect_equal(
      result[[col_name]][i],
      mat[id, as.character(focal)],
      label = paste("inbreeding row", i, "ID", id)
    )
  }
})

test_that("ped2addFocal alignment holds when inbreeding pedigree rows are shuffled", {
  data(inbreeding)
  focal <- inbreeding$ID[nrow(inbreeding)]
  inbreeding_shuffled <- inbreeding[sample(nrow(inbreeding)), ]
  result <- ped2addFocal(inbreeding_shuffled, focal_id = focal)
  mat <- ped2add(inbreeding, sparse = FALSE)
  col_name <- paste0("additiveRel_", focal)

  for (i in seq_len(nrow(result))) {
    id <- as.character(result$ID[i])
    expect_equal(
      result[[col_name]][i],
      mat[id, as.character(focal)],
      label = paste("inbreeding shuffled row", i, "ID", id)
    )
  }
})

test_that("ped2mitFocal values are correctly aligned to pedigree rows", {
  data(hazard)
  focal <- 1
  result <- ped2mitFocal(hazard, focal_id = focal)
  mat <- ped2mit(hazard, sparse = FALSE)
  col_name <- paste0("mitochondrialRel_", focal)

  expect_true(col_name %in% colnames(result))
  for (i in seq_len(nrow(result))) {
    id <- as.character(result$ID[i])
    expect_equal(result[[col_name]][i], mat[id, as.character(focal)],
      label = paste("mitFocal row", i, "ID", id)
    )
  }
})

test_that("ped2mtFocal is an alias for ped2mitFocal", {
  data(hazard)
  result_mit <- ped2mitFocal(hazard, focal_id = 1)
  result_mt <- ped2mtFocal(hazard, focal_id = 1)
  expect_equal(result_mit, result_mt)
})

test_that("ped2cnFocal values are correctly aligned to pedigree rows", {
  data(hazard)
  focal <- 1
  result <- ped2cnFocal(hazard, focal_id = focal)
  mat <- ped2cn(hazard, sparse = FALSE)
  col_name <- paste0("common_nuclearRel_", focal)

  expect_true(col_name %in% colnames(result))
  for (i in seq_len(nrow(result))) {
    id <- as.character(result$ID[i])
    expect_equal(result[[col_name]][i], mat[id, as.character(focal)],
      label = paste("cnFocal row", i, "ID", id)
    )
  }
})

test_that("ped2cnFocal alignment holds for inbreeding dataset", {
  data(inbreeding)
  focal <- 1
  result_cn <- ped2cnFocal(inbreeding, focal_id = focal)
  mat_cn <- ped2cn(inbreeding, sparse = FALSE)
  col_cn <- paste0("common_nuclearRel_", focal)

  for (i in seq_len(nrow(result_cn))) {
    id <- as.character(result_cn$ID[i])
    expect_equal(result_cn[[col_cn]][i], mat_cn[id, as.character(focal)],
      label = paste("inbreeding cnFocal row", i, "ID", id)
    )
  }
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

# ped2genFocal

test_that("ped2genFocal values are correctly aligned to pedigree rows", {
  data(hazard)
  focal <- 1
  result <- ped2genFocal(hazard, focal_id = focal)
  mat <- ped2gen(hazard, sparse = FALSE)
  col_name <- paste0("generationRel_", focal)

  expect_true(col_name %in% colnames(result))
  for (i in seq_len(nrow(result))) {
    id <- as.character(result$ID[i])
    expect_equal(result[[col_name]][i], unname(mat[id]),
      label = paste("genFocal row", i, "ID", id)
    )
  }
})
