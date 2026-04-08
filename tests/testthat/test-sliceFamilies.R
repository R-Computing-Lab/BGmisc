library(testthat)
library(withr)

# Helper to create a small test CSV for sliceFamilies input
.make_test_input <- function(file_path) {
  test_data <- data.frame(
    ID1 = c(1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
    ID2 = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
    addRel = c(0.5, 0.25, 0.125, 0.0625, 1.0, 0.5, 0.25, 0.125, 0.5, 0.25),
    mitRel = c(1, 0, 1, 0, 1, 1, 0, 1, 0, 1),
    cnuRel = c(0.5, 0.25, 0.125, 0.0625, 1.0, 0.5, 0.25, 0.125, 0.5, 0.25)
  )
  data.table::fwrite(test_data, file = file_path, sep = ",", col.names = TRUE)
  test_data
}

test_that("sliceFamilies creates correct output files for both mitRel = 1 and 0", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  # Create test input data
  data(hazard)
  ad_ped_matrix <- ped2com(hazard,
    component = "additive",
    adjacency_method = "direct", sparse = TRUE
  )
  mit_ped_matrix <- ped2com(hazard,
    component = "mitochondrial",
    adjacency_method = "direct", sparse = TRUE
  )
  cn_ped_matrix <- ped2com(hazard,
    component = "common nuclear",
    adjacency_method = "indexed", sparse = TRUE
  )

  result <- com2links(
    ad_ped_matrix = ad_ped_matrix,
    mit_ped_matrix = mit_ped_matrix,
    cn_ped_matrix = cn_ped_matrix,
    writetodisk = TRUE,
    rel_pairs_file = "dataRelatedPairs.csv"
  )
  # Run function
  sliceFamilies(
    outcome_name = "testout",
    biggest = TRUE,
    bin_width = 0.10,
    degreerelatedness = 12,
    chunk_size = 30, # small chunk size to force chunk logic
    max_lines = 1000,
    input_file = "dataRelatedPairs.csv",
    progress_csv = "progress.csv",
    progress_status = "progress.txt",
    error_handling = TRUE,
    verbose = TRUE,
    data_directory = "testout/data/links_10/" # ,
    # file_column_names = names(test_data)
  )


  # Validate output structure
  output_dir <- file.path("testout", "data", "links_10")
  expect_true(dir.exists(output_dir))
  files <- list.files(output_dir, pattern = "\\.csv$", full.names = TRUE)
  expect_gt(length(files), 0)

  # Check contents of a file
  test_data <- data.table::fread("dataRelatedPairs.csv")
  all_data <- data.table::rbindlist(lapply(files, data.table::fread),
    fill = TRUE
  )
  expect_true(all(all_data$ID1 %in% test_data$ID1))
  expect_true(all(all_data$mitRel %in% c(0, 1)))

  # Check progress log
  expect_true(file.exists("progress.csv"))
  progress <- data.table::fread("progress.csv")
  expect_true("start_line" %in% names(progress))
  expect_true("total_lines" %in% names(progress))

  # Check progress status file
  expect_true(file.exists("progress.txt"))
  stat <- readLines("progress.txt")
  expect_true(any(grepl("Done!", stat)))

  # remove.file("dataRelatedPairs.csv")
  # delete.file(files)
  unlink(output_dir, recursive = TRUE)
  unlink("progress.csv")
  unlink("progress.txt")
})

test_that("sliceFamilies works with biggest = FALSE and data_directory = NULL", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  # Create input file matching the biggest=FALSE naming convention
  input_data <- .make_test_input("myoutcome_dataAllbutBiggestRelatedPairsTake2.csv")

  sliceFamilies(
    outcome_name = "myoutcome",
    biggest = FALSE,
    bin_width = 0.10,
    degreerelatedness = 12,
    chunk_size = 100,
    max_lines = 100,
    input_file = NULL, # should auto-construct filename
    data_directory = NULL, # should auto-construct directory
    progress_csv = "progress_false.csv",
    progress_status = "progress_false.txt",
    verbose = FALSE
  )

  # Directory should contain "links_allbut_" in the path
  expected_dir <- file.path("myoutcome", "data", "links_allbut_10")
  expect_true(dir.exists(expected_dir))

  # Should have created output files
  files <- list.files(expected_dir, pattern = "\\.csv$")
  expect_gt(length(files), 0)

  # Progress files should exist
  expect_true(file.exists("progress_false.csv"))
  expect_true(file.exists("progress_false.txt"))
  stat <- readLines("progress_false.txt")
  expect_true(any(grepl("Done!", stat)))
})

test_that("sliceFamilies works with biggest = TRUE and data_directory = NULL", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  # Create input file matching the biggest=TRUE naming convention
  input_data <- .make_test_input("myoutcome_dataBiggestRelatedPairsTake2.csv")

  sliceFamilies(
    outcome_name = "myoutcome",
    biggest = TRUE,
    bin_width = 0.10,
    degreerelatedness = 12,
    chunk_size = 100,
    max_lines = 100,
    input_file = NULL,
    data_directory = NULL,
    progress_csv = "progress_big.csv",
    progress_status = "progress_big.txt",
    verbose = FALSE
  )

  # Directory should contain "links_" (not "links_allbut_")
  expected_dir <- file.path("myoutcome", "data", "links_10")
  expect_true(dir.exists(expected_dir))
  files <- list.files(expected_dir, pattern = "\\.csv$")
  expect_gt(length(files), 0)
})

test_that("sliceFamilies errors on nonexistent input_file", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  expect_error(
    sliceFamilies(
      outcome_name = "test",
      input_file = "does_not_exist.csv",
      data_directory = "out"
    ),
    "Input file does not exist"
  )
})

test_that("sliceFamilies resumes from existing progress.csv", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  input_data <- .make_test_input("input.csv")

  # First run: process first chunk only (chunk_size=5 of 10 rows)
  sliceFamilies(
    outcome_name = "resume_test",
    biggest = TRUE,
    chunk_size = 5,
    max_lines = 5, # stop after first chunk
    input_file = "input.csv",
    data_directory = "resume_out",
    progress_csv = "resume_progress.csv",
    progress_status = "resume_progress.txt",
    verbose = FALSE
  )

  # progress.csv should exist from first run
  expect_true(file.exists("resume_progress.csv"))
  progress1 <- data.table::fread("resume_progress.csv")

  # Second run: should resume from where we left off
  sliceFamilies(
    outcome_name = "resume_test",
    biggest = TRUE,
    chunk_size = 5,
    max_lines = 20, # small enough to finish quickly
    input_file = "input.csv",
    data_directory = "resume_out",
    progress_csv = "resume_progress.csv",
    progress_status = "resume_progress.txt",
    verbose = TRUE
  )

  # Should complete
  stat <- readLines("resume_progress.txt")
  expect_true(any(grepl("Done!", stat)))
})

test_that(".safe_fread returns data on success", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  test_data <- data.frame(a = 1:5, b = 6:10)
  data.table::fwrite(test_data, "test_safe.csv", col.names = TRUE)

  result <- BGmisc:::.safe_fread("test_safe.csv", start_line = 2, chunk_size = 10)
  expect_false(is.null(result$data))
  expect_equal(nrow(result$data), 5)
  expect_equal(result$chunk_size, 10)
})

test_that(".safe_fread returns NULL on missing file without error_handling", {
  result <- BGmisc:::.safe_fread("nonexistent_file.csv",
    start_line = 1, chunk_size = 10,
    error_handling = FALSE
  )
  expect_null(result$data)
  expect_equal(result$chunk_size, 10)
})

test_that(".safe_fread retries with halved chunk_size when error_handling = TRUE", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  # Create a progress log to verify error logging
  result <- BGmisc:::.safe_fread(
    "nonexistent_file.csv",
    start_line = 1,
    chunk_size = 100,
    error_handling = TRUE,
    max_retries = 2,
    progress_status = "err_log.txt"
  )

  # After 2 retries (halving each time): 100 -> 50 -> 25
  expect_null(result$data)
  expect_equal(result$chunk_size, 25)

  # Error log should exist and contain error messages
  expect_true(file.exists("err_log.txt"))
  log_content <- readLines("err_log.txt")
  expect_gt(length(log_content), 0)
})

test_that(".write_bin_data creates file only when matching data exists", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  dir.create("bin_test")

  test_dt <- data.table::data.table(
    ID1 = 1:3,
    ID2 = 4:6,
    addRel = c(0.5, 0.5, 0.25),
    mitRel = c(1, 0, 1),
    cnuRel = c(0.5, 0.5, 0.25)
  )

  # Write mitRel=1 bin that matches addRel ~0.5
  BGmisc:::.write_bin_data(test_dt,
    range_min = 0.45, range_max = 0.55, mit_val = 1,
    data_directory = "bin_test", verbose = FALSE
  )

  files <- list.files("bin_test", pattern = "\\.csv$")
  expect_equal(length(files), 1)
  expect_true(grepl("df_mt1_r", files[1]))

  written <- data.table::fread(file.path("bin_test", files[1]))
  expect_equal(nrow(written), 1) # only ID1=1 matches addRel=0.5 & mitRel=1

  # Write mitRel=1 bin with no matches - should NOT create file
  BGmisc:::.write_bin_data(test_dt,
    range_min = 0.9, range_max = 1.1, mit_val = 1,
    data_directory = "bin_test", verbose = FALSE
  )

  files_after <- list.files("bin_test", pattern = "\\.csv$")
  expect_equal(length(files_after), 1) # still only 1 file
})

test_that("sliceFamilies uses file.path correctly for output paths (no trailing slash needed)", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  input_data <- .make_test_input("input.csv")

  # Pass data_directory WITHOUT trailing slash
  sliceFamilies(
    outcome_name = "pathtest",
    biggest = TRUE,
    chunk_size = 100,
    max_lines = 100,
    input_file = "input.csv",
    data_directory = "pathtest/output", # no trailing slash
    progress_csv = "path_progress.csv",
    progress_status = "path_progress.txt",
    verbose = FALSE
  )

  # Output files should be inside the directory, not adjacent to it
  expect_true(dir.exists("pathtest/output"))
  files <- list.files("pathtest/output", pattern = "\\.csv$")
  expect_gt(length(files), 0)

  # No files should be created at the parent level with mangled names
  parent_files <- list.files("pathtest", pattern = "^df_mt")
  expect_equal(length(parent_files), 0)
})
