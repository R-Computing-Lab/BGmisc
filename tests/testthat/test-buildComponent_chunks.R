# ── chunked tcrossprod per-chunk checkpointing ───────────────────────────────

# Helper: chunk file path matching production code
chunk_path <- function(save_path, i) {
  paste0(file.path(save_path, "tcrossprod_checkpoint.rds"), "_chunk_", i, ".rds")
}
# ── chunked tcrossprod ────────────────────────────────────────────────────────

test_that("chunked tcrossprod matches standard tcrossprod", {
  data(hazard)
  r_standard <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    transpose_method = "tcrossprod"
  )
  r_chunked_line <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    transpose_method = "chunked", chunk_size = 3L
  )

  r_chunked_prop <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    transpose_method = "chunked", chunk_size = .5
  )

  r_chunked <- r_chunked_prop
  expect_equal(as.matrix(r_standard), as.matrix(r_chunked), tolerance = 1e-10)
  expect_equal(r_standard, r_chunked, tolerance = 1e-10)

  r_chunked <- r_chunked_line
  expect_equal(as.matrix(r_standard), as.matrix(r_chunked), tolerance = 1e-10)
  expect_equal(r_standard, r_chunked, tolerance = 1e-10)

  data(inbreeding)
  r_standard <- ped2com(inbreeding,
    component = "additive", sparse = T,
    transpose_method = "tcrossprod"
  )
  r_chunked <- ped2com(inbreeding,
    component = "additive", sparse = T,
    transpose_method = "chunked", chunk_size = 3L, force_symmetric = FALSE
  )

  r_chunked_sym <- ped2com(inbreeding,
    component = "additive", sparse = T,
    transpose_method = "chunked", chunk_size = 3L, force_symmetric = TRUE
  )

  expect_equal(as.matrix(r_standard), as.matrix(r_chunked), tolerance = 1e-10)

  expect_gt( # size of matrix in gb r_chunked should be bigger than r_standard due to chunking overhead
    object.size(r_chunked) / 1e9,
    object.size(r_standard) / 1e9
  )

  expect_equal(Matrix::forceSymmetric(r_chunked),
    r_chunked_sym,
    tolerance = 1e-10
  )
  expect_equal(r_standard,
    r_chunked_sym
    # convert to symmetric matrix for comparison since chunked output may not be perfectly symmetric due to numerical precision
    ,
    tolerance = 1e-10
  )
})

test_that("chunked tcrossprod with chunk_size >= nrow behaves like tcrossprod", {
  data(hazard)
  r_standard <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    transpose_method = "tcrossprod"
  )
  r_chunked <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    transpose_method = "chunked",
    chunk_size = nrow(hazard) + 1L
  )

  expect_equal(as.matrix(r_standard), as.matrix(r_chunked), tolerance = 1e-10)
})


test_that("chunked tcrossprod matches standard tcrossprod when also subsetted", {
  data(inbreeding)

  keep <- as.character(inbreeding$ID[5:10])

  r_full <- ped2com(inbreeding,
    component = "additive", sparse = T,
    keep_ids = NULL
  )
  r_sub <- ped2com(inbreeding,
    component = "additive", sparse = T,
    keep_ids = keep
  )

  expect_equal(dim(r_sub), c(length(keep), length(keep)))
  expect_equal(rownames(r_sub), keep)

  # values in the subset must match the corresponding entries of the full matrix
  expect_equal(r_sub, r_full[keep, keep], tolerance = 1e-10)

  r_chunked <- ped2com(inbreeding,
    component = "additive", sparse = T,
    transpose_method = "chunked", chunk_size = 3L,
    keep_ids = keep, force_symmetric = T
  )

  expect_equal(as.matrix(r_sub), as.matrix(r_chunked), tolerance = 1e-10)
  expect_equal(r_sub, r_chunked, tolerance = 1e-10)
  expect_equal(r_full[keep, keep], r_chunked, tolerance = 1e-10)
})
test_that("chunked tcrossprod saves per-chunk files when saveable = TRUE and size is line-based", {
  data(hazard)
  save_path <- file.path(tempdir(), "test_chunk_save")
  unlink(save_path, recursive = TRUE)
  dir.create(save_path, showWarnings = FALSE)
  on.exit(unlink(save_path, recursive = TRUE))

  chunk_size <- 3L
  r_full <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    transpose_method = "chunked", chunk_size = chunk_size,
    saveable = TRUE, resume = FALSE, save_path = save_path
  )

  n_chunks <- ceiling(nrow(r_full) / chunk_size)
  for (i in seq_len(n_chunks)) {
    expect_true(file.exists(chunk_path(save_path, i)),
      info = paste("chunk file", i, "should exist")
    )
  }
})

test_that("chunked tcrossprod saves per-chunk files when saveable = TRUE and size is proportion", {
  data(hazard)
  save_path <- file.path(tempdir(), "test_chunk_save")

  dir.create(save_path, showWarnings = FALSE)
  on.exit(unlink(save_path, recursive = TRUE))

  chunk_size <- .32
  r_full <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    transpose_method = "chunked", chunk_size = chunk_size,
    saveable = TRUE, resume = FALSE, save_path = save_path
  )

  n_chunks <- ceiling(nrow(r_full) / ceiling(nrow(r_full) * chunk_size))
  for (i in seq_len(n_chunks)) {
    expect_true(file.exists(chunk_path(save_path, i)),
      info = paste("chunk file", i, "should exist")
    )
  }
})


test_that("chunked tcrossprod resumes from all saved chunk files", {
  data(hazard)
  save_path <- file.path(tempdir(), "test_chunk_resume_all")
  unlink(save_path, recursive = TRUE)
  dir.create(save_path, showWarnings = FALSE)
  on.exit(unlink(save_path, recursive = TRUE))

  chunk_size <- 3L

  r1 <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    transpose_method = "chunked", chunk_size = chunk_size,
    saveable = TRUE, resume = FALSE, save_path = save_path
  )

  # Remove assembled checkpoint so resume must reconstruct from chunk files
  unlink(file.path(save_path, "tcrossprod_checkpoint.rds"))
  unlink(file.path(save_path, "final_matrix.rds"))

  r2 <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    transpose_method = "chunked", chunk_size = chunk_size,
    saveable = FALSE, resume = TRUE, save_path = save_path
  )

  expect_equal(as.matrix(r1), as.matrix(r2), tolerance = 1e-10)
})

test_that("chunked tcrossprod partial resume recomputes missing chunks and gives correct result", {
  data(hazard)
  save_path <- file.path(tempdir(), "test_chunk_resume_partial")
  unlink(save_path, recursive = TRUE)
  dir.create(save_path, showWarnings = FALSE)
  on.exit(unlink(save_path, recursive = TRUE))

  chunk_size <- 3L

  r_reference <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    transpose_method = "chunked", chunk_size = chunk_size,
    saveable = TRUE, resume = FALSE, save_path = save_path
  )

  n_chunks <- ceiling(nrow(r_reference) / chunk_size)

  # Delete even-numbered chunks to simulate an interrupted run
  for (i in seq(2, n_chunks, by = 2)) unlink(chunk_path(save_path, i))
  unlink(file.path(save_path, "tcrossprod_checkpoint.rds"))
  unlink(file.path(save_path, "final_matrix.rds"))

  r_partial <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    transpose_method = "chunked", chunk_size = chunk_size,
    saveable = FALSE, resume = TRUE, save_path = save_path
  )

  expect_equal(as.matrix(r_reference), as.matrix(r_partial), tolerance = 1e-10)
})

test_that("chunked tcrossprod resume loads chunk files not recompute them", {
  data(hazard)
  save_path <- file.path(tempdir(), "test_chunk_load_proof")
  unlink(save_path, recursive = TRUE)
  dir.create(save_path, showWarnings = FALSE)
  on.exit(unlink(save_path, recursive = TRUE))

  chunk_size <- 3L

  ped2com(hazard,
    component = "additive", sparse = FALSE,
    transpose_method = "chunked", chunk_size = chunk_size,
    saveable = TRUE, resume = FALSE, save_path = save_path
  )

  # Replace chunk 1 with a sentinel (all zeros) to prove it gets loaded not recomputed
  sentinel <- readRDS(chunk_path(save_path, 1)) * 0
  saveRDS(sentinel, file = chunk_path(save_path, 1))

  unlink(file.path(save_path, "tcrossprod_checkpoint.rds"))
  unlink(file.path(save_path, "final_matrix.rds"))

  r_resumed <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    transpose_method = "chunked", chunk_size = chunk_size,
    saveable = FALSE, resume = TRUE, save_path = save_path
  )

  # Rows from chunk 1 must be all-zero (loaded from sentinel)
  expect_true(all(as.matrix(r_resumed)[1:chunk_size, ] == 0),
    info = "rows from chunk 1 should be all-zero (sentinel)"
  )
  # Rows from chunk 2 must be non-zero (computed normally)
  expect_true(any(as.matrix(r_resumed)[chunk_size + 1L, ] != 0),
    info = "rows from chunk 2 should be non-zero"
  )
})

# ── tcrossprod_ids checkpoint validation ──────────────────────────────────────

test_that("tcrossprod checkpoint is reused when keep_ids matches saved ids", {
  data(hazard)
  keep <- as.character(hazard$ID[1:5])
  save_path <- file.path(tempdir(), "test_tcp_ids_match")
  dir.create(save_path, showWarnings = FALSE)
  on.exit(unlink(save_path, recursive = TRUE))

  # First run: saves tcrossprod_checkpoint and tcrossprod_ids
  r1 <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    keep_ids = keep, saveable = TRUE, resume = FALSE,
    save_path = save_path
  )

  expect_true(file.exists(file.path(save_path, "tcrossprod_ids.rds")))
  expect_equal(readRDS(file.path(save_path, "tcrossprod_ids.rds")), keep)

  # Second run: same keep_ids → should load checkpoint, not recompute
  r2 <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    keep_ids = keep, saveable = FALSE, resume = TRUE,
    save_path = save_path
  )

  expect_equal(r1, r2)
})

test_that("tcrossprod checkpoint is recomputed with warning when keep_ids changes", {
  data(hazard)
  keep1 <- as.character(hazard$ID[1:5])
  keep2 <- as.character(hazard$ID[6:10])
  save_path <- file.path(tempdir(), "test_tcp_ids_mismatch")
  unlink(save_path, recursive = TRUE) # guarantee clean state
  dir.create(save_path, showWarnings = FALSE)


  ped2com(hazard,
    component = "additive", sparse = FALSE,
    keep_ids = keep1, saveable = TRUE, resume = FALSE,
    save_path = save_path
  )

  # verify the setup saved what we expect before testing the warning
  expect_true(file.exists(file.path(save_path, "tcrossprod_checkpoint.rds")))
  expect_equal(readRDS(file.path(save_path, "tcrossprod_ids.rds")), keep1)

  unlink(file.path(save_path, "final_matrix.rds")) # ensure we're testing the checkpoint loading, not final matrix loading


  expect_warning(
    r2 <- ped2com(hazard,
      component = "additive", sparse = FALSE,
      keep_ids = keep2, saveable = FALSE, resume = TRUE, verbose = TRUE,
      save_path = save_path
    ),
    "keep_ids do not match"
  )
  expect_equal(rownames(r2), keep2)
  on.exit(unlink(save_path, recursive = TRUE))
})

test_that("tcrossprod checkpoint saved with keep_ids=NULL is reused on NULL resume", {
  data(hazard)
  save_path <- file.path(tempdir(), "test_tcp_ids_null")
  dir.create(save_path, showWarnings = FALSE)
  on.exit(unlink(save_path, recursive = TRUE))

  r1 <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    keep_ids = NULL, saveable = TRUE, resume = FALSE,
    save_path = save_path
  )

  expect_null(readRDS(file.path(save_path, "tcrossprod_ids.rds")))

  r2 <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    keep_ids = NULL, saveable = FALSE, resume = TRUE,
    save_path = save_path
  )

  expect_equal(r1, r2)
})

test_that("tcrossprod checkpoint saved with NULL warns when resumed with keep_ids", {
  data(hazard)
  keep <- as.character(hazard$ID[1:5])
  save_path <- file.path(tempdir(), "test_tcp_ids_null_mismatch")
  unlink(save_path, recursive = TRUE) # guarantee clean state
  dir.create(save_path, showWarnings = FALSE)
  on.exit(unlink(save_path, recursive = TRUE))

  ped2com(hazard,
    component = "additive", sparse = FALSE,
    keep_ids = NULL, saveable = TRUE, resume = FALSE,
    save_path = save_path
  )

  # verify the setup saved what we expect before testing the warning
  expect_true(file.exists(file.path(save_path, "tcrossprod_checkpoint.rds")))
  expect_null(readRDS(file.path(save_path, "tcrossprod_ids.rds")))
  unlink(file.path(save_path, "final_matrix.rds")) # ensure we're testing the checkpoint loading, not final matrix loading

  expect_warning(
    ped2com(hazard,
      component = "additive", sparse = FALSE,
      keep_ids = keep, saveable = FALSE, resume = TRUE,
      save_path = save_path
    ),
    "keep_ids do not match"
  )
})
