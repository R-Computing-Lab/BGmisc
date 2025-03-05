test_that("ped2add produces correct matrix dims, values, and dimnames for hazard", {
  tolerance <- 1e-10
  data(hazard)
  add <- ped2add(hazard)
  # Check dimension
  expect_equal(dim(add), c(nrow(hazard), nrow(hazard)))
  # Check several values
  # expect_true(all(diag(add) == 1))
  expect_true(sum((diag(add) - 1)^2) < tolerance)
  expect_equal(add, t(add))
  expect_equal(add[2, 1], 0)
  expect_equal(add[10, 1], .25)
  expect_equal(add[9, 1], 0)
  expect_equal(add["5", "6"], .5)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(hazard$ID))
})

test_that("ped2add produces correct matrix dims, values, and dimnames for alternative transpose", {
  tolerance <- 1e-10
  data(hazard)
  add <- ped2add(hazard, tcross.alt.crossprod = TRUE)
  # Check dimension
  expect_equal(dim(add), c(nrow(hazard), nrow(hazard)), tolerance = tolerance)
  # Check several values
  # expect_true(all(diag(add) == 1))
  expect_true(sum((diag(add) - 1)^2) < tolerance)
  expect_equal(add, t(add), tolerance = tolerance)
  expect_equal(add[2, 1], 0, tolerance = tolerance)
  expect_equal(add[10, 1], .25, tolerance = tolerance)
  expect_equal(add[9, 1], 0, tolerance = tolerance)
  expect_equal(add["5", "6"], .5)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(hazard$ID))
})
# to do, combine the sets that are equalivant. shouldn't need to run 1000 expect equals

test_that("ped2add produces correct matrix dims, values, and dimnames for inbreeding data", {
  tolerance <- 1e-10
  data(inbreeding)
  add <- ped2add(inbreeding)
  # Check dimension
  expect_equal(dim(add), c(nrow(inbreeding), nrow(inbreeding)), tolerance = tolerance)
  # Check several values
  expect_true(all(diag(add) >= 1 - tolerance))
  expect_equal(add, t(add), tolerance = tolerance)
  expect_equal(add[2, 1], 0, tolerance = tolerance)
  expect_equal(add[6, 1], .5, tolerance = tolerance)
  expect_equal(add[113, 113], 1.1250, tolerance = tolerance)
  expect_equal(add["113", "112"], 0.62500)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})


test_that("ped2add produces correct matrix dims, values, and dimnames for inbreeding data with alternative transpose", {
  tolerance <- 1e-10
  data(inbreeding)
  add <- ped2add(inbreeding, transpose_method = "star")
  # Check dimension
  expect_equal(dim(add), c(nrow(inbreeding), nrow(inbreeding)))
  # Check several values
  expect_true(all(diag(add) >= 1))
  expect_equal(add, t(add), tolerance = tolerance)
  expect_equal(add[2, 1], 0, tolerance = tolerance)
  expect_equal(add[6, 1], .5, tolerance = tolerance)
  expect_equal(add[113, 113], 1.1250, tolerance = tolerance)
  expect_equal(add["113", "112"], 0.62500)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})

test_that("ped2add produces correct matrix dims, values, and dimnames for inbreeding data with 2nd alternative transpose", {
  tolerance <- 1e-10
  data(inbreeding)
  add <- ped2add(inbreeding, transpose_method = "crossprod")
  # Check dimension
  expect_equal(dim(add), c(nrow(inbreeding), nrow(inbreeding)))
  # Check several values
  expect_true(all(diag(add) >= 1))
  expect_equal(add, t(add), tolerance = tolerance)
  expect_equal(add[2, 1], 0, tolerance = tolerance)
  expect_equal(add[6, 1], .5, tolerance = tolerance)
  expect_equal(add[113, 113], 1.1250, tolerance = tolerance)
  expect_equal(add["113", "112"], 0.62500)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})


test_that("ped2add flattens diagonal for inbreeding data", {
  tolerance <- 1e-10
  data(inbreeding)
  add <- ped2add(inbreeding, flatten.diag = TRUE)
  # Check dimension
  expect_equal(dim(add), c(nrow(inbreeding), nrow(inbreeding)), tolerance = tolerance)
  # Check several values
  # expect_true(all(diag(add) == 1))
  expect_true(sum((diag(add) - 1)^2) < tolerance)
  expect_equal(add, t(add), tolerance = tolerance)
  expect_equal(add[2, 1], 0, tolerance = tolerance)
  expect_equal(add[6, 1], .5, tolerance = tolerance)
  expect_equal(add[113, 113], 1, tolerance = tolerance)
  expect_equal(add["113", "112"], 0.62500)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})
test_that("ped2mit produces correct matrix dims, values, and dimnames for inbreeding", {
  tolerance <- 1e-10
  # Check dimension
  data(inbreeding)
  mit <- ped2mit(inbreeding)
  # Check dimension
  expect_equal(dim(mit), c(nrow(inbreeding), nrow(inbreeding)))
  # Check several values
  # expect_true(all(diag(mit) == 1))
  expect_true(sum((diag(mit) - 1)^2) < tolerance)
  expect_equal(mit, t(mit), tolerance = tolerance)
  expect_equal(mit[2, 1], 0, tolerance = tolerance)
  expect_equal(mit[6, 1], 1, tolerance = tolerance)
  expect_equal(mit[113, 113], 1, tolerance = tolerance)
  expect_equal(mit["113", "112"], 1, tolerance = tolerance)
  # Check that dimnames are correct
  dn <- dimnames(mit)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})

test_that("ped2mit produces correct matrix dims, values, and dimnames for inbreeding", {
  tolerance <- 1e-10
  # Check dimension
  data(inbreeding)
  mit <- ped2mit(inbreeding)
  # Check dimension
  expect_equal(dim(mit), c(nrow(inbreeding), nrow(inbreeding)), tolerance = tolerance)
  # Check several values
  # expect_true(all(diag(mit) == 1))
  expect_true(sum((diag(mit) - 1)^2) < tolerance)
  expect_equal(mit, t(mit), tolerance = tolerance)
  expect_equal(mit[2, 1], 0, tolerance = tolerance)
  expect_equal(mit[6, 1], 1, tolerance = tolerance)
  expect_equal(mit[113, 113], 1, tolerance = tolerance)
  expect_equal(mit["113", "112"], 1, tolerance = tolerance)
  # Check that dimnames are correct
  dn <- dimnames(mit)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})

test_that("ped2cn produces correct matrix dims, values, and dimnames", {
  tolerance <- 1e-10

  # Check dimension
  data(inbreeding)
  cn <- ped2cn(inbreeding)
  expect_equal(dim(cn), c(
    nrow(inbreeding),
    nrow(inbreeding)
  ),
  tolerance = tolerance
  )
  # Check several values
  # expect_true(all(diag(cn) == 1))
  expect_true(sum((diag(cn) - 1)^2) < tolerance)
  expect_equal(cn, t(cn), tolerance = tolerance)
  expect_equal(cn[2, 1], 0, tolerance = tolerance)
  expect_equal(cn[6, 1], 0, tolerance = tolerance)
  expect_equal(cn[113, 113], 1, tolerance = tolerance)
  expect_equal(cn["113", "112"], 1, tolerance = tolerance)
  # Check that dimnames are correct
  dn <- dimnames(cn)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
  #  expect_silent(data(inbreeding))
})

test_that("ped2ce produces correct matrix dims, values, and dimnames", {
  tolerance <- 1e-10
  data(inbreeding)
  ce <- ped2ce(inbreeding)
  expect_equal(dim(ce), c(nrow(inbreeding), nrow(inbreeding)), tolerance = tolerance)
  # Check several values
  # expect_true(all(diag(ce) == 1))
  expect_true(sum((diag(ce) - 1)^2) < tolerance)
  expect_equal(ce, t(ce), tolerance = tolerance)
  expect_equal(ce[2, 1], 1, tolerance = tolerance)
  expect_equal(ce[6, 1], 1, tolerance = tolerance)
  expect_equal(ce[113, 113], 1, tolerance = tolerance)
  expect_equal(ce["113", "112"], 1, tolerance = tolerance)
  # Check that dimnames are correct
  dn <- dimnames(ce)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})

test_that("ped2add verbose prints updates", {
  data(hazard)
  expect_output(ped2add(hazard, verbose = TRUE), regexp = "Family Size =")
})



test_that("ped2maternal/paternal produces correct matrix dims", {
  data(hazard)
  tolerance <- 1e-10
  mat <- ped2maternal(hazard)
  expect_equal(dim(mat), c(nrow(hazard), ncol(hazard) + 1))
  data(hazard)
  pat <- ped2paternal(hazard)
  expect_equal(dim(pat), c(nrow(hazard), ncol(hazard) + 1))

  expect_lt(cor(pat$patID, mat$matID), 1)
})

test_that("ped2com handles checkpoint saving and resuming", {
  save_path <- tempdir() # Use temporary directory for saving checkpoints
  data(hazard)

  ped_add_saved <- ped2com(hazard,
    component = "additive", saveable = TRUE, save_path = save_path,
    save_rate_gen = 1,
    save_rate_parlist = 10,
    adjacency_method = "direct"
  )

  checkpoint_files_v0 <- list(
    parList = file.path(save_path, "parList.rds"),
    lens = file.path(save_path, "lens.rds"),
    isPar = file.path(save_path, "isPar.rds"),
    iss = file.path(save_path, "iss.rds"),
    jss = file.path(save_path, "jss.rds"),
    isChild = file.path(save_path, "isChild.rds"),
    r_checkpoint = file.path(save_path, "r_checkpoint.rds"),
    gen_checkpoint = file.path(save_path, "gen_checkpoint.rds"),
    newIsPar_checkpoint = file.path(save_path, "newIsPar_checkpoint.rds"),
    mtSum_checkpoint = file.path(save_path, "mtSum_checkpoint.rds"),
    r2_checkpoint = file.path(save_path, "r2_checkpoint.rds"),
    tcrossprod_checkpoint = file.path(save_path, "tcrossprod_checkpoint.rds"),
    count_checkpoint = file.path(save_path, "count_checkpoint.rds"),
    final_matrix = file.path(save_path, "final_matrix.rds")
  )

  # Check if checkpoint files exist
  checkpoint_files_v1 <- list.files(save_path, pattern = "\\.rds$", full.names = TRUE)

  expect_equal(length(checkpoint_files_v1), length(checkpoint_files_v0))

  # Resume from checkpoint
  resumed_matrix <- ped2com(hazard, component = "additive", resume = TRUE, save_path = save_path,
                            adjacency_method = "direct")

  expect_equal(dim(resumed_matrix), c(nrow(hazard), nrow(hazard)))
  expect_equal(dim(resumed_matrix), dim(ped_add_saved))
  # Cleanup
  unlink(save_path, recursive = TRUE)
})

#  adjacency_method = "indexed" and "loop" produce the same results", {
test_that("adjacency_method  'indexed', 'loop', and direct produce the same results for additive matrix", {
  data(inbreeding)
  tolerance <- 1e-10
  ped_add_indexed <- ped2com(hazard, component = "additive", adjacency_method = "indexed")
  ped_add_loop <- ped2com(hazard, component = "additive", adjacency_method = "loop")
  ped_add_direct <- ped2com(hazard, component = "additive", adjacency_method = "direct")
  expect_equal(ped_add_indexed, ped_add_loop, tolerance = tolerance)
  expect_equal(ped_add_loop, ped_add_direct, tolerance = tolerance)
  expect_equal(ped_add_indexed, ped_add_direct, tolerance = tolerance)

})

test_that("adjacency_method  'indexed', 'loop', and direct produce the same results for mtdna  matrix", {
  data(hazard)
  tolerance <- 1e-10

  ped_mit_indexed <- ped2com(hazard, component = "mitochondrial", adjacency_method = "indexed")
  ped_mit_loop <- ped2com(hazard, component = "mitochondrial", adjacency_method = "loop")
  ped_mit_direct <- ped2com(hazard, component = "mitochondrial", adjacency_method = "direct")
  expect_equal(ped_mit_indexed, ped_mit_loop, tolerance = tolerance)
  expect_equal(ped_mit_loop, ped_mit_direct, tolerance = tolerance)
  expect_equal(ped_mit_indexed, ped_mit_direct, tolerance = tolerance)
})


test_that("adjacency_method  'indexed', 'loop', and direct produce the same results for common nuclear matrix", {
  data(hazard)
  tolerance <- 1e-10

  # common nuclear
  ped_common_indexed <- ped2com(hazard, component = "common nuclear", adjacency_method = "indexed")
  ped_common_loop <- ped2com(hazard, component = "common nuclear", adjacency_method = "loop")
  #  ped_common_direct <- ped2com(hazard, component = "common nuclear", adjacency_method = "direct")
  expect_equal(ped_common_indexed, ped_common_loop, tolerance = tolerance)
  # expect_equal(ped_common_loop, ped_common_direct, tolerance = tolerance)
  # expect_equal(ped_common_indexed, ped_common_direct, tolerance = tolerance)

})


test_that("adjacency_method  'indexed', 'loop', and direct produce the same results for generation matrix", {
  data(hazard)
  tolerance <- 1e-10
  #  generation
  ped_gen_indexed <- ped2com(hazard, component = "generation", adjacency_method = "indexed")
  ped_gen_loop <- ped2com(hazard, component = "generation", adjacency_method = "loop")
  ped_gen_direct <- ped2com(hazard, component = "generation", adjacency_method = "direct")

  expect_equal(ped_gen_indexed, ped_gen_loop, tolerance = tolerance)
  expect_equal(ped_gen_loop, ped_gen_direct, tolerance = tolerance)
  expect_equal(ped_gen_indexed, ped_gen_direct, tolerance = tolerance)
})
