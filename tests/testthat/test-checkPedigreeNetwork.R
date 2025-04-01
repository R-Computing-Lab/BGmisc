# Test Case 1: Validate correct network structure in the original 'potter' dataset
test_that("checkPedigreeNetwork identifies valid pedigree network correctly in potter dataset", {
  ped <- potter
  results <- checkPedigreeNetwork(ped, personID = "personID", momID = "momID", dadID = "dadID", verbose = TRUE)
  expect_true(results$is_acyclic)
  expect_equal(length(results$individuals_with_excess_parents), 0)
  expect_null(results$cyclic_relationships)
})

# Test Case 2: Validate detection of a cycle in 'potter' dataset by introducing a cyclic relationship
test_that("checkPedigreeNetwork detects cyclic relationships correctly in potter dataset", {
  cyclic_potter <- potter
  # Introduce a cycle: making Harry Potter his own ancestor
  cyclic_potter$dadID[cyclic_potter$personID == cyclic_potter$dadID[cyclic_potter$name == "Harry Potter"]] <- cyclic_potter$personID[cyclic_potter$name == "Harry Potter"]

  results <- checkPedigreeNetwork(cyclic_potter, personID = "personID", momID = "momID", dadID = "dadID", verbose = FALSE)
  expect_false(results$is_acyclic)
  expect_true(!is.null(results$cyclic_relationships))
  expect_true(any(cyclic_potter$personID[cyclic_potter$name == "Harry Potter"] %in% results$cyclic_relationships))
})

# Test Case 3: Validate detection of individuals with more than two parents in 'potter' dataset
test_that("checkPedigreeNetwork detects individuals with excess parents in potter dataset", {
  data("potter")
  excess_parents_potter <- potter
  # Artificially create an excess parent situation for Harry Potter
  new_parent <- data.frame(
    personID = "999",
    famID = "1",
    name = "New Parent",
    gen = 1,
    momID = NA,
    dadID = NA,
    spouseID = NA,
    sex = "M"
  )

  excess_parents_potter <- rbind(excess_parents_potter, new_parent)

  # Duplicate Harry Potter row to simulate excess parentage scenario
  extra_row <- excess_parents_potter[excess_parents_potter$name == "Harry Potter", ]
  extra_row$dadID <- "999" # new parent
  excess_parents_potter <- rbind(excess_parents_potter, extra_row)

  results <- checkPedigreeNetwork(excess_parents_potter, personID = "personID", momID = "momID", dadID = "dadID", verbose = TRUE)

  expect_true("Harry Potter" %in% excess_parents_potter$name[excess_parents_potter$personID %in% results$individuals_with_excess_parents])
})

# Test Case 4: Validate detection of duplicate edges in 'potter' dataset
test_that("checkPedigreeNetwork detects duplicate edges in potter dataset", {
  dup_potter <- potter
  dup_row <- potter[1, ]
  dup_potter <- rbind(dup_potter, dup_row)

  results <- checkPedigreeNetwork(dup_potter,
    personID = "personID",
    momID = "momID",
    dadID = "dadID", verbose = TRUE
  )

  expect_true(nrow(results$duplicate_edges) > 0)
})
