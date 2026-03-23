# Test Case 1: findLeaves returns a character vector for the hazard dataset
test_that("findLeaves returns a character vector of leaf IDs for hazard dataset", {
  data(hazard)
  leaves <- findLeaves(hazard)
  expect_true(is.character(leaves))
  expect_true(length(leaves) > 0)
})

# Test Case 2: findLeaves identifies terminal nodes correctly in the hazard dataset
test_that("findLeaves correctly identifies terminal nodes in hazard dataset", {
  data(hazard)
  leaves_terminal <- findLeaves(hazard, include_founder_singletons = FALSE)
  leaves_all <- findLeaves(hazard, include_founder_singletons = TRUE)
  # Terminal-only should be a subset of all leaves
  expect_true(all(leaves_terminal %in% leaves_all))
  # All returned IDs must be present in the pedigree rows
  expect_true(all(leaves_all %in% as.character(hazard$ID)))
})

# Test Case 3: findLeaves include_founder_singletons flag adds more leaves in hazard dataset
test_that("findLeaves finds more leaves with include_founder_singletons = TRUE in hazard dataset", {
  data(hazard)
  leaves_terminal <- findLeaves(hazard, include_founder_singletons = FALSE)
  leaves_all <- findLeaves(hazard, include_founder_singletons = TRUE)
  expect_true(length(leaves_all) >= length(leaves_terminal))
})

# Test Case 4: findLeaves verbose prints messages for hazard dataset
test_that("findLeaves verbose prints messages for hazard dataset", {
  data(hazard)
  expect_message(findLeaves(hazard, verbose = TRUE), "terminal nodes")
  expect_message(findLeaves(hazard, verbose = TRUE), "total leaf nodes")
})

# Test Case 5: findLeaves works with non-default column names using potter dataset
test_that("findLeaves works with non-default personID column in potter dataset", {
  data(potter)
  potter$ID <- potter$personID
  leaves <- findLeaves(potter, personID = "ID", momID = "momID", dadID = "dadID")
  expect_true(is.character(leaves))
  expect_true(all(leaves %in% as.character(potter$ID)))
})

# Test Case 6: trimPedigree reduces the size of the hazard dataset
test_that("trimPedigree reduces pedigree size for hazard dataset", {
  data(hazard)
  trimmed <- trimPedigree(hazard)
  expect_true(nrow(trimmed) < nrow(hazard))
  expect_true(nrow(trimmed) >= 0)
})

# Test Case 7: trimPedigree produces no dangling parent references in hazard dataset
test_that("trimPedigree leaves no dangling parent references in hazard dataset", {
  data(hazard)
  trimmed <- trimPedigree(hazard)
  remaining_ids <- as.character(trimmed$ID)
  dad_refs <- as.character(trimmed$dadID[!is.na(trimmed$dadID)])
  mom_refs <- as.character(trimmed$momID[!is.na(trimmed$momID)])
  expect_true(all(dad_refs %in% remaining_ids))
  expect_true(all(mom_refs %in% remaining_ids))
})

# Test Case 8: trimPedigree with max_iter = 1 removes only one layer of leaves in hazard dataset
test_that("trimPedigree with max_iter = 1 removes fewer individuals than full trim in hazard dataset", {
  data(hazard)
  trimmed_one <- trimPedigree(hazard, max_iter = 1)
  trimmed_full <- trimPedigree(hazard, max_iter = Inf)
  expect_true(nrow(trimmed_one) >= nrow(trimmed_full))
})

# Test Case 9: trimPedigree verbose prints iteration messages for hazard dataset
test_that("trimPedigree verbose prints iteration messages for hazard dataset", {
  data(hazard)
  expect_message(trimPedigree(hazard, verbose = TRUE), "Iteration")
})

# Test Case 10: trimPedigree with remove_ids removes specified individuals before trimming in hazard dataset
test_that("trimPedigree remove_ids removes specified individuals before leaf trimming in hazard dataset", {
  data(hazard)
  target_id <- hazard$ID[1]
  trimmed <- trimPedigree(hazard, remove_ids = target_id)
  expect_false(target_id %in% trimmed$ID)
})

# Test Case 11: trimPedigree returns a data.frame with the same columns as input for hazard dataset
test_that("trimPedigree preserves input column structure for hazard dataset", {
  data(hazard)
  trimmed <- trimPedigree(hazard)
  expect_true(inherits(trimmed, "data.frame"))
  expect_true(all(names(hazard) %in% names(trimmed)))
})

# Test Case 12: trimPedigree with include_founder_singletons = FALSE trims fewer in hazard dataset
test_that("trimPedigree trims more with founder singletons than without in hazard dataset", {
  data(hazard)
  trimmed_no_founders <- trimPedigree(hazard, include_founder_singletons = FALSE)
  trimmed_with_founders <- trimPedigree(hazard, include_founder_singletons = TRUE)
  expect_true(nrow(trimmed_with_founders) <= nrow(trimmed_no_founders))
})

# Test Case 13: findLeaves returns a character vector for the inbreeding dataset
test_that("findLeaves returns a character vector of leaf IDs for inbreeding dataset", {
  data(inbreeding)
  leaves <- findLeaves(inbreeding)
  expect_true(is.character(leaves))
  expect_true(length(leaves) > 0)
  expect_true(all(leaves %in% as.character(inbreeding$ID)))
})

# Test Case 14: trimPedigree reduces pedigree size and preserves column structure for inbreeding dataset
test_that("trimPedigree reduces pedigree size and preserves columns for inbreeding dataset", {
  data(inbreeding)
  trimmed <- trimPedigree(inbreeding)
  expect_true(nrow(trimmed) < nrow(inbreeding))
  expect_true(inherits(trimmed, "data.frame"))
  expect_true(all(names(inbreeding) %in% names(trimmed)))
})

# Test Case 15: trimPedigree leaves no dangling parent references in inbreeding dataset
test_that("trimPedigree leaves no dangling parent references in inbreeding dataset", {
  data(inbreeding)
  trimmed <- trimPedigree(inbreeding)
  remaining_ids <- as.character(trimmed$ID)
  dad_refs <- as.character(trimmed$dadID[!is.na(trimmed$dadID)])
  mom_refs <- as.character(trimmed$momID[!is.na(trimmed$momID)])
  expect_true(all(dad_refs %in% remaining_ids))
  expect_true(all(mom_refs %in% remaining_ids))
})

# Test Case 16: findLeaves identifies terminal nodes (outdegree == 0) in a known minimal pedigree
test_that("findLeaves identifies terminal nodes correctly in a minimal pedigree", {
  ped <- data.frame(
    ID    = c(1, 2, 3),
    dadID = c(NA, 1, 2),
    momID = c(NA, NA, NA)
  )
  leaves <- findLeaves(ped, include_founder_singletons = FALSE)
  expect_true("3" %in% leaves)
  expect_false("1" %in% leaves)
})

# Test Case 17: findLeaves identifies founder singletons (indegree == 0, outdegree == 1) in a minimal pedigree
test_that("findLeaves identifies founder singletons correctly in a minimal pedigree", {
  ped <- data.frame(
    ID    = c(1, 2, 3),
    dadID = c(NA, NA, 1),
    momID = c(NA, NA, 2)
  )
  leaves_all      <- findLeaves(ped, include_founder_singletons = TRUE)
  leaves_terminal <- findLeaves(ped, include_founder_singletons = FALSE)
  expect_true("1" %in% leaves_all)
  expect_true("2" %in% leaves_all)
  expect_false("1" %in% leaves_terminal)
  expect_false("2" %in% leaves_terminal)
})

# Test Case 18: trimPedigree respects max_iter step-by-step on a known chain
test_that("trimPedigree respects max_iter on a known linear chain", {
  ped <- data.frame(
    ID    = c(1, 2, 3, 4),
    dadID = c(NA, 1, 2, 3),
    momID = c(NA, NA, NA, NA)
  )
  trimmed_1 <- trimPedigree(ped, include_founder_singletons = FALSE, max_iter = 1)
  expect_false(4 %in% trimmed_1$ID)
  expect_true(3 %in% trimmed_1$ID)

  trimmed_2 <- trimPedigree(ped, include_founder_singletons = FALSE, max_iter = 2)
  expect_false(4 %in% trimmed_2$ID)
  expect_false(3 %in% trimmed_2$ID)
  expect_true(2 %in% trimmed_2$ID)
})

# Test Case 19: trimPedigree remove_ids forces removal before leaf trimming in a minimal pedigree
test_that("trimPedigree remove_ids forces removal before leaf trimming in a minimal pedigree", {
  ped <- data.frame(
    ID    = c(1, 2, 3, 4),
    dadID = c(NA, NA, 1, 3),
    momID = c(NA, NA, 2, NA)
  )
  trimmed <- trimPedigree(ped, remove_ids = 4,
                          include_founder_singletons = FALSE, max_iter = 1)
  expect_false(4 %in% trimmed$ID)
  expect_false(3 %in% trimmed$ID)
})

# Test Case 20: trimPedigree with full iteration removes all nodes from a fully trimable pedigree
test_that("trimPedigree with full iteration removes all nodes from a fully trimmable pedigree", {
  ped <- data.frame(
    ID    = c(1, 2, 3),
    dadID = c(NA, NA, 1),
    momID = c(NA, NA, 2)
  )
  trimmed <- trimPedigree(ped, include_founder_singletons = TRUE, max_iter = Inf)
  expect_equal(nrow(trimmed), 0L)
})

# Test Case 21: findLeaves with include_terminal = FALSE excludes terminal nodes in a minimal pedigree
test_that("findLeaves with include_terminal = FALSE excludes terminal nodes in a minimal pedigree", {
  ped <- data.frame(
    ID    = c(1, 2, 3),
    dadID = c(NA, NA, 1),
    momID = c(NA, NA, 2)
  )
  # With include_terminal = FALSE: node 3 (outdegree 0) should not appear
  leaves <- findLeaves(ped, include_terminal = FALSE, include_founder_singletons = TRUE)
  expect_false("3" %in% leaves)
  # Founder singletons 1 and 2 should still appear
  expect_true("1" %in% leaves)
  expect_true("2" %in% leaves)
})

# Test Case 22: findLeaves errors when both include flags are FALSE
test_that("findLeaves errors when both include_terminal and include_founder_singletons are FALSE", {
  data(hazard)
  expect_error(
    findLeaves(hazard, include_terminal = FALSE, include_founder_singletons = FALSE),
    "At least one"
  )
})

# Test Case 23: trimPedigree with include_terminal = FALSE does not remove terminal nodes in hazard dataset
test_that("trimPedigree with include_terminal = FALSE does not remove terminal nodes in hazard dataset", {
  data(hazard)
  # Identify terminal IDs before trimming
  terminal_ids <- findLeaves(hazard, include_terminal = TRUE, include_founder_singletons = FALSE)
  trimmed <- trimPedigree(hazard, include_terminal = FALSE, include_founder_singletons = TRUE)
  # None of the original terminal nodes should have been removed
  expect_true(all(terminal_ids %in% as.character(trimmed$ID)))
})

# Test Case 24: trimPedigree min_size prevents trimming below threshold in hazard dataset
test_that("trimPedigree min_size prevents pedigree from falling below threshold in hazard dataset", {
  data(hazard)
  min_n <- nrow(hazard) - 5L
  trimmed <- trimPedigree(hazard, min_size = min_n)
  expect_true(nrow(trimmed) >= min_n)
})

# Test Case 25: trimPedigree min_size = 0 behaves identically to default in hazard dataset
test_that("trimPedigree min_size = 0 behaves identically to default in hazard dataset", {
  data(hazard)
  trimmed_default  <- trimPedigree(hazard)
  trimmed_min_zero <- trimPedigree(hazard, min_size = 0L)
  expect_equal(trimmed_default, trimmed_min_zero)
})

# Test Case 26: trimPedigree min_size verbose message fires when threshold would be breached
test_that("trimPedigree min_size prints stopping message when threshold would be breached", {
  ped <- data.frame(
    ID    = c(1, 2, 3),
    dadID = c(NA, NA, 1),
    momID = c(NA, NA, 2)
  )
  expect_message(
    trimPedigree(ped, include_founder_singletons = TRUE, min_size = 2L, verbose = TRUE),
    "min_size"
  )
})

# Test Case 27: findLeaves errors when keep_var is not a column in the pedigree
test_that("findLeaves errors when keep_var column does not exist", {
  data(hazard)
  expect_error(findLeaves(hazard, keep_var = "not_a_column"), "not found")
})

# Test Case 28: findLeaves with keep_var protects individuals with non-missing phenotype in a minimal pedigree
test_that("findLeaves with keep_var protects individuals with non-missing phenotype in a minimal pedigree", {
  ped <- data.frame(
    ID       = c(1, 2, 3, 4),
    dadID    = c(NA, NA, 1, 1),
    momID    = c(NA, NA, 2, 2),
    affected = c(NA, NA, 1, NA)
  )
  # Node 3 and 4 are terminal; node 3 has affected = 1 so should be protected
  leaves <- findLeaves(ped, keep_var = "affected")
  expect_false("3" %in% leaves)
  expect_true("4" %in% leaves)
})

# Test Case 29: findLeaves with keep_var and keep_vals protects only matching values in a minimal pedigree
test_that("findLeaves with keep_var and keep_vals protects only specified values in a minimal pedigree", {
  ped <- data.frame(
    ID       = c(1, 2, 3, 4),
    dadID    = c(NA, NA, 1, 1),
    momID    = c(NA, NA, 2, 2),
    affected = c(NA, NA, 1, 0)
  )
  # Protect only affected == 1; node 4 (affected = 0) remains removable
  leaves_protect_1 <- findLeaves(ped, keep_var = "affected", keep_vals = 1)
  expect_false("3" %in% leaves_protect_1)
  expect_true("4" %in% leaves_protect_1)

  # Protect only affected == 0; node 3 (affected = 1) remains removable
  leaves_protect_0 <- findLeaves(ped, keep_var = "affected", keep_vals = 0)
  expect_true("3" %in% leaves_protect_0)
  expect_false("4" %in% leaves_protect_0)
})

# Test Case 30: findLeaves with keep_vals = NA protects individuals with missing phenotype in a minimal pedigree
test_that("findLeaves with keep_vals = NA protects individuals with missing phenotype in a minimal pedigree", {
  ped <- data.frame(
    ID       = c(1, 2, 3, 4),
    dadID    = c(NA, NA, 1, 1),
    momID    = c(NA, NA, 2, 2),
    affected = c(NA, NA, 1, NA)
  )
  # Protect those with missing phenotype; node 4 (NA) protected, node 3 (value=1) removable
  leaves <- findLeaves(ped, keep_var = "affected", keep_vals = NA)
  expect_true("3" %in% leaves)
  expect_false("4" %in% leaves)
})

# Test Case 31: trimPedigree with keep_var never removes protected individuals from hazard dataset
test_that("trimPedigree with keep_var never removes individuals with non-missing phenotype in hazard dataset", {
  data(hazard)
  # Add a synthetic phenotype: mark the first 10 individuals as affected
  hazard$affected <- NA
  hazard$affected[1:10] <- 1
  protected_ids <- as.character(hazard$ID[1:10])

  trimmed <- trimPedigree(hazard, keep_var = "affected")
  # All protected individuals must still be present
  expect_true(all(protected_ids %in% as.character(trimmed$ID)))
})

# Test Case 32: trimPedigree with keep_var removes more individuals than without in hazard dataset
test_that("trimPedigree with keep_var removes fewer individuals than without in hazard dataset", {
  data(hazard)
  hazard$affected <- NA
  hazard$affected[1:10] <- 1

  trimmed_no_keep <- trimPedigree(hazard)
  trimmed_keep    <- trimPedigree(hazard, keep_var = "affected")
  # Protecting phenotyped individuals means fewer are removed overall
  expect_true(nrow(trimmed_keep) >= nrow(trimmed_no_keep))
})
