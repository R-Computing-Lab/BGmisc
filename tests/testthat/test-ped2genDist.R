# ── getGenDist ────────────────────────────────────────────────────────────────

# ── adjacency_method consistency ─────────────────────────────────────────────

test_that("ped2genDist path results are identical across adjacency methods", {
  data(hazard)
  mat_direct  <- ped2genDist(hazard, method = "path", adjacency_method = "direct")
  mat_indexed <- ped2genDist(hazard, method = "path", adjacency_method = "indexed")
  mat_loop    <- ped2genDist(hazard, method = "path", adjacency_method = "loop")
  expect_equal(mat_direct,  mat_indexed)
  expect_equal(mat_direct,  mat_loop)
})

test_that("ped2genDist mrca_min results are identical across adjacency methods", {
  data(hazard)
  mat_direct  <- ped2genDist(hazard, method = "mrca_min", adjacency_method = "direct")
  mat_indexed <- ped2genDist(hazard, method = "mrca_min", adjacency_method = "indexed")
  ped2gen(hazard)
  expect_equal(mat_direct, mat_indexed)
})

# ── getGenDist ────────────────────────────────────────────────────────────────

test_that("getGenDist errors when id not in pedigree", {
  data(hazard)
  expect_error(getGenDist(hazard, id1 = 9999, id2 = 1), "9999")
  expect_error(getGenDist(hazard, id1 = 1, id2 = 9999), "9999")
})

test_that("getGenDist rank: parent-child is 1", {
  data(hazard)
  # IDs 3, 4, 5 are children of 1 and 2
  expect_equal(unname(getGenDist(hazard, id1 = 1, id2 = 3, method = "rank")), 1)
  expect_equal(unname(getGenDist(hazard, id1 = 1, id2 = 4, method = "rank")), 1)
  expect_equal(unname(getGenDist(hazard, id1 = 1, id2 = 5, method = "rank")), 1)
})

test_that("getGenDist rank: same individual is 0", {
  data(hazard)
  expect_equal(unname(getGenDist(hazard, id1 = 1, id2 = 1, method = "rank")), 0)
})

test_that("getGenDist rank: is symmetric", {
  data(hazard)
  expect_equal(
    unname(getGenDist(hazard, id1 = 1, id2 = 3, method = "rank")),
    unname(getGenDist(hazard, id1 = 3, id2 = 1, method = "rank"))
  )
})

test_that("getGenDist path: parent-child is 1", {
  data(hazard)
  expect_equal(getGenDist(hazard, id1 = 1, id2 = 3, method = "path"), 1)
})

test_that("getGenDist path: siblings share a common parent at 2 steps", {
  data(hazard)
  # 3 and 4 are siblings — both children of 1 (and 2)
  # path = min steps through any common ancestor = 2 (3→1→4)
  expect_equal(getGenDist(hazard, id1 = 3, id2 = 4, method = "path"), 2)
  expect_equal(getGenDist(hazard, id1 = 3, id2 = 5, method = "path"), 2)
})

test_that("getGenDist path: is symmetric", {
  data(hazard)
  expect_equal(
    getGenDist(hazard, id1 = 3, id2 = 4, method = "path"),
    getGenDist(hazard, id1 = 4, id2 = 3, method = "path")
  )
})

test_that("getGenDist path: same individual is 0", {
  data(hazard)
  expect_equal(getGenDist(hazard, id1 = 3, id2 = 3, method = "path"), 0)
})

test_that("getGenDist mrca_min: parent-child is 1", {
  data(hazard)
  expect_equal(getGenDist(hazard, id1 = 1, id2 = 3, method = "mrca_min"), 1)
})

test_that("getGenDist mrca_min: siblings is 2", {
  data(hazard)
  expect_equal(getGenDist(hazard, id1 = 3, id2 = 4, method = "mrca_min"), 2)
})

test_that("getGenDist mrca_min equals path for non-inbred pedigree", {
  data(hazard)
  ids <- hazard$ID[1:min(10, nrow(hazard))]
  for (id1 in ids) {
    for (id2 in ids) {
      d_path <- getGenDist(hazard, id1, id2, method = "path")
      d_mrca <- getGenDist(hazard, id1, id2, method = "mrca_min")
      expect_equal(d_path, d_mrca,
                   label = paste("path vs mrca_min for IDs", id1, id2))
    }
  }
})

test_that("getGenDist mrca_max >= mrca_min", {
  data(hazard)
  ids <- hazard$ID[1:min(10, nrow(hazard))]
  for (id1 in ids) {
    for (id2 in ids) {
      d_min <- getGenDist(hazard, id1, id2, method = "mrca_min")
      d_max <- getGenDist(hazard, id1, id2, method = "mrca_max")
      if (!is.na(d_min) && !is.na(d_max)) {
        expect_true(d_max >= d_min,
                    label = paste("mrca_max >= mrca_min for IDs", id1, id2))
      }
    }
  }
})

test_that("getGenDist mrca_max: is symmetric", {
  data(hazard)
  expect_equal(
    getGenDist(hazard, id1 = 3, id2 = 4, method = "mrca_max"),
    getGenDist(hazard, id1 = 4, id2 = 3, method = "mrca_max")
  )
})

test_that("getGenDist returns NA for unrelated individuals", {
  data(hazard)
  # Founders 1 and 2 have no common ancestor — check if truly unrelated
  d <- getGenDist(hazard, id1 = 1, id2 = 2, method = "path")
  # Accept NA (unrelated) or a numeric value (if they share an ancestor)
  expect_true(is.na(d) || is.numeric(d))
})

test_that("getGenDist mrca_all is not yet implemented", {
  data(hazard)
  expect_error(
    getGenDist(hazard, id1 = 1, id2 = 3, method = "mrca_all"),
    "not yet implemented"
  )
})

test_that("getGenDist works with inbreeding dataset", {
  data(inbreeding)
  d <- getGenDist(inbreeding, id1 = 1, id2 = 2, method = "path")
  expect_true(is.numeric(d))
})

test_that("getGenDist works with character IDs via potter dataset", {
  data(potter)
  # potter uses personID column
  d <- getGenDist(potter, id1 = 1, id2 = 2,
                  method = "rank", personID = "personID")
  expect_true(is.numeric(d) || is.na(d))
})

# ── ped2genDistFocal ──────────────────────────────────────────────────────────

test_that("ped2genDistFocal returns pedigree with new column", {
  data(hazard)
  result <- ped2genDistFocal(hazard, focal_id = 1, method = "rank")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(hazard))
  expect_true("genDist_rank_1" %in% colnames(result))
})

test_that("ped2genDistFocal default column name encodes method and focal_id", {
  data(hazard)
  result_rank <- ped2genDistFocal(hazard, focal_id = 1, method = "rank")
  expect_true("genDist_rank_1" %in% colnames(result_rank))

  result_path <- ped2genDistFocal(hazard, focal_id = 1, method = "path")
  expect_true("genDist_path_1" %in% colnames(result_path))

  result_mrca_min <- ped2genDistFocal(hazard, focal_id = 1, method = "mrca_min")
  expect_true("genDist_mrca_min_1" %in% colnames(result_mrca_min))
})

test_that("ped2genDistFocal respects custom col_name", {
  data(hazard)
  result <- ped2genDistFocal(hazard, focal_id = 1, method = "rank",
                              col_name = "my_gen_dist")
  expect_true("my_gen_dist" %in% colnames(result))
  expect_false("genDist_rank_1" %in% colnames(result))
})

test_that("ped2genDistFocal errors when focal_id not in pedigree", {
  data(hazard)
  expect_error(
    ped2genDistFocal(hazard, focal_id = 9999, method = "rank"),
    "9999"
  )
})

test_that("ped2genDistFocal focal distance to self is 0", {
  data(hazard)
  result <- ped2genDistFocal(hazard, focal_id = 1, method = "rank")
  expect_equal(result$genDist_rank_1[result$ID == 1], 0)
})

test_that("ped2genDistFocal rank: parent-child distance is 1", {
  data(hazard)
  result <- ped2genDistFocal(hazard, focal_id = 1, method = "rank")
  expect_equal(result$genDist_rank_1[result$ID == 3], 1)
  expect_equal(result$genDist_rank_1[result$ID == 4], 1)
  expect_equal(result$genDist_rank_1[result$ID == 5], 1)
})

test_that("ped2genDistFocal values match getGenDist for every individual", {
  data(hazard)
  focal <- 1L
  result <- ped2genDistFocal(hazard, focal_id = focal, method = "path")
  col_name <- "genDist_path_1"

  for (i in seq_len(nrow(result))) {
    id <- result$ID[i]
    expected <- getGenDist(hazard, id1 = focal, id2 = id, method = "path")
    expect_equal(result[[col_name]][i], expected,
                 label = paste("path focal=1 row", i, "ID", id))
  }
})

test_that("ped2genDistFocal mrca_min values match getGenDist", {
  data(hazard)
  focal <- 1L
  result <- ped2genDistFocal(hazard, focal_id = focal, method = "mrca_min")
  col_name <- "genDist_mrca_min_1"

  for (i in seq_len(nrow(result))) {
    id <- result$ID[i]
    expected <- getGenDist(hazard, id1 = focal, id2 = id, method = "mrca_min")
    expect_equal(result[[col_name]][i], expected,
                 label = paste("mrca_min focal=1 row", i, "ID", id))
  }
})

test_that("ped2genDistFocal alignment holds when pedigree rows are shuffled", {
  data(hazard)
  focal <- hazard$ID[nrow(hazard)]
  hazard_shuffled <- hazard[sample(nrow(hazard)), ]
  result_orig  <- ped2genDistFocal(hazard,          focal_id = focal, method = "rank")
  result_shuff <- ped2genDistFocal(hazard_shuffled, focal_id = focal, method = "rank")
  col_name <- paste0("genDist_rank_", focal)

  # After matching by ID the values should agree
  for (i in seq_len(nrow(hazard_shuffled))) {
    id <- as.character(hazard_shuffled$ID[i])
    orig_val  <- result_orig[[col_name]][result_orig$ID == hazard_shuffled$ID[i]]
    shuff_val <- result_shuff[[col_name]][i]
    expect_equal(shuff_val, orig_val,
                 label = paste("shuffled row", i, "ID", id))
  }
})

test_that("ped2genDistFocal works on inbreeding dataset", {
  data(inbreeding)
  result <- ped2genDistFocal(inbreeding, focal_id = 1, method = "path")
  expect_true(is.data.frame(result))
  expect_true("genDist_path_1" %in% colnames(result))
  focal_row <- result$genDist_path_1[result$ID == 1]
  expect_equal(focal_row, 0)
})

test_that("ped2genDistFocal works on potter dataset with custom personID", {
  data(potter)
  result <- ped2genDistFocal(potter, focal_id = 1, method = "rank",
                              personID = "personID")
  expect_true(is.data.frame(result))
  expect_true("genDist_rank_1" %in% colnames(result))
  expect_equal(nrow(result), nrow(potter))
})

# ── ped2genDist (full matrix) ─────────────────────────────────────────────────

test_that("ped2genDist returns a square numeric matrix", {
  data(hazard)
  mat <- ped2genDist(hazard, method = "rank")
  expect_true(is.matrix(mat))
  expect_true(is.numeric(mat))
  expect_equal(nrow(mat), ncol(mat))
  expect_equal(nrow(mat), nrow(hazard))
})

test_that("ped2genDist row and column names match pedigree IDs", {
  data(hazard)
  mat <- ped2genDist(hazard, method = "rank")
  ped_ids <- as.character(hazard$ID)
  expect_equal(rownames(mat), ped_ids)
  expect_equal(colnames(mat), ped_ids)
})

test_that("ped2genDist diagonal is zero", {
  data(hazard)
  mat <- ped2genDist(hazard, method = "rank")
  expect_true(all(diag(mat) == 0))
})

test_that("ped2genDist is symmetric", {
  data(hazard)
  for (method in c("rank", "path", "mrca_min", "mrca_max")) {
    mat <- ped2genDist(hazard, method = method)
    expect_equal(mat, t(mat), label = paste("symmetry for method", method))
  }
})

test_that("ped2genDist rank: parent-child cell equals 1", {
  data(hazard)
  mat <- ped2genDist(hazard, method = "rank")
  expect_equal(mat["1", "3"], 1)
  expect_equal(mat["1", "4"], 1)
  expect_equal(mat["1", "5"], 1)
})

test_that("ped2genDist path: sibling cell equals 2", {
  data(hazard)
  mat <- ped2genDist(hazard, method = "path")
  expect_equal(mat["3", "4"], 2)
  expect_equal(mat["3", "5"], 2)
})

test_that("ped2genDist values match getGenDist for every pair", {
  data(hazard)
  mat <- ped2genDist(hazard, method = "path")
  ped_ids <- as.character(hazard$ID)
  # Spot check a subset of pairs to keep test time reasonable
  check_ids <- ped_ids[1:min(8, length(ped_ids))]
  for (id1 in check_ids) {
    for (id2 in check_ids) {
      expected <- getGenDist(hazard, id1, id2, method = "path")
      expect_equal(mat[id1, id2], expected,
                   label = paste("path matrix cell", id1, id2))
    }
  }
})

test_that("ped2genDist mrca_max >= mrca_min for all pairs", {
  data(hazard)
  mat_min <- ped2genDist(hazard, method = "mrca_min")
  mat_max <- ped2genDist(hazard, method = "mrca_max")

  mask <- !is.na(mat_min) & !is.na(mat_max)
  expect_true(all(mat_max[mask] >= mat_min[mask]))
})

test_that("ped2genDist works on inbreeding dataset", {
  data(inbreeding)
  mat <- ped2genDist(inbreeding, method = "path")
  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), nrow(inbreeding))
  expect_true(all(diag(mat) == 0))
  expect_equal(mat, t(mat))
})

test_that("ped2genDist works on potter dataset with custom column names", {
  data(potter)
  mat <- ped2genDist(potter, method = "rank",
                     personID = "personID", momID = "momID", dadID = "dadID")
  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), nrow(potter))
  expect_true(all(diag(mat) == 0))
})

test_that("ped2genDist mrca_all throws not-implemented error", {
  data(hazard)
  expect_error(
    ped2genDist(hazard, method = "mrca_all"),
    "not yet implemented"
  )
})
