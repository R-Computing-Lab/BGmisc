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

test_that("rowless_parents_method = 'schur' matches 'rows' exactly without adding any rows", {
  r_rows <- ped2add(ped_rowless, sparse = FALSE, repair_rowless_parents = TRUE, rowless_parents_method = "rows")
  # This toy pedigree has zero resolvable edges among the recorded individuals
  # (every parent is rowless), which independently triggers a pre-existing,
  # unrelated "empty isPar" warning regardless of rowless-parent handling --
  # suppress it here since it's not what this test is checking.
  r_schur <- suppressWarnings(
    ped2add(ped_rowless, sparse = FALSE, repair_rowless_parents = TRUE, rowless_parents_method = "schur")
  )

  expect_equal(dim(r_schur), c(4L, 4L))
  expect_equal(sort(rownames(r_schur)), c("C1", "C2", "C3", "C4"))
  expect_equal(r_schur[sort(rownames(r_schur)), sort(rownames(r_schur))],
               r_rows[sort(rownames(r_rows)), sort(rownames(r_rows))])

  expect_equal(unname(diag(r_schur)), rep(1, 4))
  expect_equal(r_schur["C1", "C2"], 0.5)
  expect_equal(r_schur["C1", "C3"], 0.25)
  expect_equal(r_schur["C2", "C3"], 0.25)
  expect_equal(r_schur["C1", "C4"], 0)
})

test_that("rowless_parents_method = 'schur' propagates correctly to a grandchild generation", {
  # F1 (rowless) -> C1 x D2 -> G1, G2 (full sibs); C1 x D3 -> G3 (paternal half-sib of G1/G2 via C1)
  ped_deep <- data.frame(
    ID    = c("C1", "D2", "D3", "G1", "G2", "G3"),
    momID = c("F1", NA,   NA,   "C1", "C1", "C1"),
    dadID = c(NA,   NA,   NA,   "D2", "D2", "D3"),
    sex   = c(0, 1, 1, 0, 1, 0),
    stringsAsFactors = FALSE
  )

  r_rows <- ped2add(ped_deep, sparse = FALSE, repair_rowless_parents = TRUE, rowless_parents_method = "rows")
  r_schur <- ped2add(ped_deep, sparse = FALSE, repair_rowless_parents = TRUE, rowless_parents_method = "schur")

  ids <- ped_deep$ID
  expect_equal(r_schur[ids, ids], r_rows[ids, ids])
  expect_equal(unname(diag(r_schur)), rep(1, 6))
  expect_equal(r_schur["G1", "G2"], 0.5)
  expect_equal(r_schur["G1", "G3"], 0.25)
})

test_that("rowless_parents_method = 'schur' errors for unsupported components", {
  expect_error(
    ped2com(ped_rowless, component = "distance", sparse = FALSE, repair_rowless_parents = TRUE,
            rowless_parents_method = "schur"),
    "additive"
  )
})
