test_that("com2links processes creates same length for cn with 3, 2, and 1 matrices are used", {
  data(hazard)
  ad_ped_matrix <- ped2add(hazard, sparse = TRUE)
  mit_ped_matrix <- ped2mit(hazard, sparse = TRUE)
  cn_ped_matrix <- ped2cn(hazard, sparse = TRUE)


  result1_legacy <- .com2links.legacy(cn_ped_matrix = cn_ped_matrix, writetodisk = FALSE)
  expect_true(is.data.frame(result1_legacy))
  expect_true(all(c("ID1", "ID2", "cnuRel") %in% colnames(result1_legacy)))
  expect_equal(ncol(result1_legacy), 3) # Expect ID1, ID2, and cnuRel
  expect_true(all(result1_legacy$cnuRel >= 0))
})

test_that("com2links legacy works", {
  data(hazard)
  ad_ped_matrix <- ped2com(hazard, component = "additive", adjacency_method = "direct", sparse = TRUE)
  mit_ped_matrix <- ped2com(hazard, component = "mitochondrial", adjacency_method = "direct", sparse = TRUE)
  cn_ped_matrix <- ped2com(hazard, component = "common nuclear", adjacency_method = "indexed", sparse = TRUE)

  resultlegacy <- .com2links.legacy(
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


  result_beta <- com2links(
    ad_ped_matrix = ad_ped_matrix,
    mit_ped_matrix = mit_ped_matrix, cn_ped_matrix = cn_ped_matrix,
    writetodisk = FALSE
  )

  expect_true(is.data.frame(result_beta))
  expect_true(all(c("ID1", "ID2", "addRel", "mitRel", "cnuRel") %in% colnames(result_beta)))


  result <- .com2links.legacy(
    ad_ped_matrix = ad_ped_matrix,
    mit_ped_matrix = mit_ped_matrix, cn_ped_matrix = cn_ped_matrix,
    writetodisk = FALSE
  )

  expect_true(is.data.frame(result))
  expect_true(all(c("ID1", "ID2", "addRel", "mitRel", "cnuRel") %in% colnames(result)))

  # Drop row names to avoid mismatches in expect_equal
  rownames(result) <- NULL
  rownames(written_data) <- NULL
  rownames(result_beta) <- NULL

  # Final comparison between written versions
  expect_equal(written_data, result)
  expect_equal(result_beta, result)
})
