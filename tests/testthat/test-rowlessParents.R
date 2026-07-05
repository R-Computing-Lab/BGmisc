ped_rowless <- data.frame(
  ID    = c("C1", "C2", "C3", "C4"),
  momID = c("M1", "M1", "M1", NA),
  dadID = c("D1", "D1", NA, NA),
  sex   = c(1, 0, 0, 1),
  stringsAsFactors = FALSE
)
# M1 and D1 are referenced as parents but never appear as their own row
# (e.g., breeding stock imported from outside the recorded population).
# C1 & C2 are full sibs via M1+D1, C3 is a maternal half-sib via M1 only,
# C4 is an unrelated founder.

test_that(".findRowlessParents identifies parent IDs with no row of their own", {
  expect_equal(sort(.findRowlessParents(ped_rowless)), c("D1", "M1"))
  expect_length(.findRowlessParents(data.frame(ID = "C1", momID = NA, dadID = NA)), 0)
})

test_that("ped2add warns when momID/dadID reference parents with no row", {
  expect_warning(
    ped2add(ped_rowless, sparse = FALSE),
    "no matching row"
  )
})

test_that("repair_rowless_parents = TRUE fixes diagonal and sibling relatedness without warning, and does not grow the returned matrix", {
  r <- expect_warning(
    ped2add(ped_rowless, sparse = FALSE, repair_rowless_parents = TRUE),
    NA
  )

  # Returned matrix stays at the original 4 individuals -- the placeholder
  # founder rows added for M1/D1 are used internally but not returned.
  expect_equal(sort(rownames(r)), c("C1", "C2", "C3", "C4"))
  expect_equal(dim(r), c(4L, 4L))

  # Diagonal should be 1 for everyone now, regardless of how many of their
  # parents have their own row.
  expect_equal(unname(diag(r)), rep(1, 4))

  # Full sibs via the repaired M1+D1 founders
  expect_equal(r["C1", "C2"], 0.5)
  # Maternal half-sibs via the repaired M1 founder only
  expect_equal(r["C1", "C3"], 0.25)
  expect_equal(r["C2", "C3"], 0.25)
  # Unrelated founder
  expect_equal(r["C1", "C4"], 0)
  expect_equal(r["C2", "C4"], 0)
  expect_equal(r["C3", "C4"], 0)
})

test_that("repair_rowless_parents = TRUE is a no-op when there are no rowless parents", {
  ped_complete <- potter
  r_repaired <- ped2add(ped_complete, sparse = FALSE, repair_rowless_parents = TRUE)
  r_plain <- ped2add(ped_complete, sparse = FALSE)
  expect_equal(r_repaired, r_plain)
})
