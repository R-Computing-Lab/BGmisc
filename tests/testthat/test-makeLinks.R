test_that("com2links handles missing matrices properly", {
  expect_error(
    com2links(ad_ped_matrix = NULL, mit_ped_matrix = NULL, cn_ped_matrix = NULL),
    "At least one of 'ad_ped_matrix', 'mit_ped_matrix', or 'cn_ped_matrix' must be provided."
  )
})




test_that("com2links rejects invalid matrix types", {
  fake_matrix <- data.frame(A = c(1, 2), B = c(3, 4))
  expect_error(com2links(ad_ped_matrix = fake_matrix), "The 'ad_ped_matrix' must be a matrix or dgCMatrix.")
})

test_that("com2links produces correct output with a single relationship matrix (hazard dataset)", {
  data(hazard)
  ad_ped_matrix <- ped2add(hazard, sparse = TRUE)

  result <- com2links(ad_ped_matrix = ad_ped_matrix, writetodisk = FALSE)

  expect_true(is.data.frame(result))
  expect_true(all(c("ID1", "ID2", "addRel") %in% colnames(result)))
  expect_equal(ncol(result), 3) # Expect ID1, ID2, and addRel
  expect_true(all(result$addRel >= 0)) # Relatedness values should be non-negative
})

test_that("com2links produces correct output with mt_ped_matrix", {
  data(hazard)
  mit_ped_matrix <- ped2mit(hazard, sparse = TRUE)

  result <- com2links(mt_ped_matrix = mit_ped_matrix, writetodisk = FALSE)

  expect_true(is.data.frame(result))
  expect_true(all(c("ID1", "ID2", "mitRel") %in% colnames(result)))
  expect_equal(ncol(result), 3) # Expect ID1, ID2, and addRel
  expect_true(all(result$addRel >= 0)) # Relatedness values should be non-negative
})

test_that("com2links processes multiple matrices correctly (hazard dataset)", {
  data(hazard)
  ad_ped_matrix <- ped2add(hazard, sparse = TRUE)
  mit_ped_matrix <- ped2mit(hazard, sparse = TRUE)
  cn_ped_matrix <- ped2cn(hazard, sparse = TRUE)

  result <- com2links(ad_ped_matrix = ad_ped_matrix, mit_ped_matrix = mit_ped_matrix, cn_ped_matrix = cn_ped_matrix, writetodisk = FALSE)

  expect_true(is.data.frame(result))
  expect_true(all(c("ID1", "ID2", "addRel", "mitRel", "cnuRel") %in% colnames(result)))
  expect_equal(ncol(result), 5) # Expect ID1, ID2, addRel, mitRel, and cnuRel
  expect_true(all(result$addRel >= 0))
  expect_true(all(result$mitRel %in% c(0, 1))) # Mitochondrial should be binary
  expect_true(all(result$cnuRel >= 0))
})


test_that("com2links written version matchs", {
  data(hazard)
  ad_ped_matrix <- ped2com(hazard, component = "additive", adjacency_method = "direct", sparse = TRUE)
  mit_ped_matrix <- ped2com(hazard, component = "mitochondrial", adjacency_method = "direct", sparse = TRUE)
  cn_ped_matrix <- ped2com(hazard, component = "common nuclear", adjacency_method = "indexed", sparse = TRUE)

  result <- com2links(
    ad_ped_matrix = ad_ped_matrix,
    mit_ped_matrix = mit_ped_matrix, cn_ped_matrix = cn_ped_matrix,
    writetodisk = TRUE, rel_pairs_file = "dataRelatedPairs_new.csv"
  )
  expect_true(is.null(result))

  written_data <- read.csv("dataRelatedPairs_new.csv")
  # remove the file
  expect_true(file.remove("dataRelatedPairs_new.csv"))
  expect_true(all(c("ID1", "ID2", "addRel", "mitRel", "cnuRel") %in% colnames(written_data)))

  result <- com2links(
    ad_ped_matrix = ad_ped_matrix,
    mit_ped_matrix = mit_ped_matrix, cn_ped_matrix = cn_ped_matrix,
    writetodisk = FALSE
  )

  expect_true(is.data.frame(result))
  expect_true(all(c("ID1", "ID2", "addRel", "mitRel", "cnuRel") %in% colnames(result)))

  # Drop row names to avoid mismatches in expect_equal
  rownames(result) <- NULL
  rownames(written_data) <- NULL


  # Final comparison between written versions
  expect_equal(written_data, result)
})
test_that("com2links legacy works", {
  data(hazard)
  ad_ped_matrix <- ped2com(hazard, component = "additive", adjacency_method = "direct", sparse = TRUE)
  mit_ped_matrix <- ped2com(hazard, component = "mitochondrial", adjacency_method = "direct", sparse = TRUE)
  cn_ped_matrix <- ped2com(hazard, component = "common nuclear", adjacency_method = "indexed", sparse = TRUE)

  resultlegacy <- com2links(
    ad_ped_matrix = ad_ped_matrix,
    mit_ped_matrix = mit_ped_matrix, cn_ped_matrix = cn_ped_matrix,
    legacy = TRUE
  )
  expect_true(is.null(resultlegacy))
  expect_true(file.exists("dataRelatedPairs.csv"))
  written_data <- read.csv("dataRelatedPairs.csv")
  # remove the file
  expect_true(file.remove("dataRelatedPairs.csv"))

  expect_true(all(c("ID1", "ID2", "addRel", "mitRel", "cnuRel") %in% colnames(written_data)))

  result <- com2links(
    ad_ped_matrix = ad_ped_matrix,
    mit_ped_matrix = mit_ped_matrix, cn_ped_matrix = cn_ped_matrix,
    writetodisk = FALSE
  )

  expect_true(is.data.frame(result))

  # Drop row names to avoid mismatches in expect_equal
  rownames(result) <- NULL
  rownames(written_data) <- NULL

  # Final comparison between written versions
  expect_equal(written_data, result)
})


test_that("com2links correctly handles missing matrices", {
  data(hazard)
  #  ad_ped_matrix <- ped2add(hazard)

  expect_error(
    com2links(ad_ped_matrix = NULL, mit_ped_matrix = NULL, cn_ped_matrix = NULL),
    "At least one of 'ad_ped_matrix', 'mit_ped_matrix', or 'cn_ped_matrix' must be provided."  )

  expect_error(com2links(ad_ped_matrix = hazard), "The 'ad_ped_matrix' must be a matrix or dgCMatrix.")
})

test_that("com2links correctly processes inbreeding dataset", {
  data(inbreeding)
  ad_ped_matrix <- ped2add(inbreeding, sparse = TRUE)
  mit_ped_matrix <- ped2mit(inbreeding, sparse = TRUE)
  cn_ped_matrix <- ped2cn(inbreeding, sparse = TRUE)

  result <- com2links(
    ad_ped_matrix = ad_ped_matrix,
    mit_ped_matrix = mit_ped_matrix,
    cn_ped_matrix = cn_ped_matrix,
    writetodisk = FALSE
  )

  expect_true(is.data.frame(result))
  expect_true(all(c("ID1", "ID2", "addRel", "mitRel", "cnuRel") %in% colnames(result)))
  expect_equal(ncol(result), 5)
  expect_true(all(result$addRel >= 0))
  expect_true(all(result$mitRel %in% c(0, 1))) # Mitochondrial should be binary
  expect_true(all(result$cnuRel >= 0))
})


test_that("com2links writes correct data to disk", {
  data(hazard)
  ad_ped_matrix <- ped2add(hazard, sparse = TRUE)

  temp_file <- tempfile(fileext = ".csv")
  com2links(ad_ped_matrix = ad_ped_matrix, rel_pairs_file = temp_file, writetodisk = TRUE)

  expect_true(file.exists(temp_file))
  written_data <- read.csv(temp_file)
  expect_true(all(c("ID1", "ID2", "addRel") %in% colnames(written_data)))
})

test_that("com2links handles large batch writing correctly", {
  set.seed(123)
  kpc <- 4
  Ngen <- 4
  marR <- 0.8
  sexR <- 0.5
  df_fam <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)

  ad_ped_matrix <- ped2add(df_fam, sparse = TRUE)

  temp_file <- tempfile(fileext = ".csv")
  com2links(ad_ped_matrix = ad_ped_matrix, rel_pairs_file = temp_file, writetodisk = TRUE, verbose = TRUE)

  expect_true(file.exists(temp_file))
  written_data <- read.csv(temp_file)
  expect_true(nrow(written_data) > 1000) # Ensuring batch writing logic works
})

test_that("com2links garbage collection does not affect output, using two components", {
  data(hazard)
  ad_ped_matrix <- ped2add(hazard, sparse = TRUE)
  mit_ped_matrix <- ped2mit(hazard, sparse = TRUE)
  cn_ped_matrix <- ped2cn(hazard, sparse = TRUE)

  result_gc <- com2links(
    ad_ped_matrix = ad_ped_matrix,
    mit_ped_matrix = mit_ped_matrix,
    gc = TRUE, writetodisk = FALSE
  )
  result_no_gc <- com2links(
    ad_ped_matrix = ad_ped_matrix,
    mit_ped_matrix = mit_ped_matrix,
    gc = FALSE, writetodisk = FALSE
  )

  expect_equal(result_gc, result_no_gc)

  result_gc <- com2links(
    ad_ped_matrix = ad_ped_matrix,
    cn_ped_matrix = cn_ped_matrix,
    gc = TRUE, writetodisk = FALSE
  )
  result_no_gc <- com2links(
    ad_ped_matrix = ad_ped_matrix,
    cn_ped_matrix = cn_ped_matrix,
    gc = FALSE, writetodisk = FALSE
  )

  expect_equal(result_gc, result_no_gc)

  result_gc <- com2links(
    mit_ped_matrix = mit_ped_matrix,
    cn_ped_matrix = cn_ped_matrix,
    gc = TRUE, writetodisk = FALSE
  )
  result_no_gc <- com2links(
    mit_ped_matrix = mit_ped_matrix,
    cn_ped_matrix = cn_ped_matrix,
    gc = FALSE, writetodisk = FALSE
  )

  expect_equal(result_gc, result_no_gc)
})
