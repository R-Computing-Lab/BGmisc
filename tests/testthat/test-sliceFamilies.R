library(testthat)
library(data.table)
library(withr)

test_that("sliceFamilies creates correct output files for both mitRel = 1 and 0", {
   local_tempdir() -> tmp
   old_wd <- getwd()
   setwd(tmp)
  # Create test input data
  data(hazard)
  ad_ped_matrix <- ped2com(hazard, component = "additive", adjacency_method = "direct", sparse = TRUE)
  mit_ped_matrix <- ped2com(hazard, component = "mitochondrial", adjacency_method = "direct", sparse = TRUE)
  cn_ped_matrix <- ped2com(hazard, component = "common nuclear", adjacency_method = "indexed", sparse = TRUE)

  result <- com2links(
    ad_ped_matrix = ad_ped_matrix,
    mit_ped_matrix = mit_ped_matrix, cn_ped_matrix = cn_ped_matrix,
    writetodisk = TRUE, rel_pairs_file = "dataRelatedPairs.csv"
  )



  # Run function
  sliceFamilies(
    outcome_name = "testout",
    biggest = TRUE,
    bin_width = 0.10,
    degreerelatedness = 12,
    chunk_size = 30,  # small chunk size to force chunk logic
    max_lines = 1000,
    input_file = "dataRelatedPairs.csv",
    progress_csv = "progress.csv",
    progress_status = "progress.txt",
    data_directory = "testout/data/links_10/"#,
   # file_column_names = names(test_data)
  )


  # Validate output structure
  output_dir <- file.path("testout", "data", "links_10")
  expect_true(dir.exists(output_dir))
  files <- list.files(output_dir, pattern = "\\.csv$", full.names = TRUE)
  expect_gt(length(files), 0)

  # Check contents of a file
  test_data <- fread("dataRelatedPairs.csv")
  all_data <- rbindlist(lapply(files, fread), fill = TRUE)
  expect_true(all(all_data$ID1 %in% test_data$ID1))
  expect_true(all(all_data$mitRel %in% c(0, 1)))

  # Check progress log
  expect_true(file.exists("progress.csv"))
  progress <- fread("progress.csv")
  expect_true("start_line" %in% names(progress))
  expect_true("total_lines" %in% names(progress))

  # Check progress status file
  expect_true(file.exists("progress.txt"))
  stat <- readLines("progress.txt")
  expect_true(any(grepl("Done!", stat)))

#remove.file("dataRelatedPairs.csv")
#delete.file(files)
  unlink(output_dir, recursive = TRUE)
  unlink("progress.csv")
  unlink("progress.txt")

  setwd(old_wd)
})

#test_that("sliceFamilies correctly defaults to filename based on `biggest`", {
#  local_tempdir() -> tmp
 # old_wd <- getwd()
 # setwd(tmp)

#  dummy_file_big <- "AD_demo_datacnmitBiggestRelatedPairs_2nddegree_take1.csv"
#  dummy_file_all <- "AD_demo_dataAllbutBiggestRelatedPairsTake2.csv"

#  data <- data.frame(ID1 = 1, ID2 = 2, addRel = 0.25, mitRel = 1, cnuRel = 0.5)
#  fwrite(data, dummy_file_big)
#  fwrite(data, dummy_file_all)

  # Run with biggest = TRUE and no input_file
 # sliceFamilies(outcome_name = "AD_demo", biggest = TRUE, chunk_size = 10, max_lines = 50)

  # Run with biggest = FALSE and no input_file
 # sliceFamilies(outcome_name = "AD_demo", biggest = FALSE, chunk_size = 10, max_lines = 50)

#  expect_true(file.exists("AD_demo/data/links_10/progress.csv") ||
#              file.exists("AD_demo/data/links_allbut_10/progress.csv"))

#  setwd(old_wd)
#})
