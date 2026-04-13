test_that("normalizeIDs creates sequential integer IDs", {
  ped <- data.frame(
    ID = c("A", "B", "C", "D"),
    momID = c(NA, NA, "A", "A"),
    dadID = c(NA, NA, "B", "B"),
    stringsAsFactors = FALSE
  )

  result <- normalizeIDs(ped)

  # ID column should be sequential integers

  expect_type(result$ID, "integer")
  expect_type(result$momID, "integer")
  expect_type(result$dadID, "integer")

  # Should have an id_map attribute
  map <- attr(result, "id_map")
  expect_false(is.null(map))
  expect_true("original_id" %in% names(map))
  expect_true("numeric_id" %in% names(map))

  # Map should contain all unique IDs
  expect_equal(nrow(map), 4L) # A, B, C, D
})

test_that("normalizeIDs preserves parent-child relationships", {
  ped <- data.frame(
    ID = c(100, 200, 300),
    momID = c(NA, NA, 100),
    dadID = c(NA, NA, 200)
  )

  result <- normalizeIDs(ped)

  # Child's momID should map to the same numeric ID as the mom's ID
  mom_numeric <- result$ID[ped$ID == 100]
  expect_equal(result$momID[3], mom_numeric)

  # Child's dadID should map to the same numeric ID as the dad's ID
  dad_numeric <- result$ID[ped$ID == 200]
  expect_equal(result$dadID[3], dad_numeric)
})

test_that("normalizeIDs handles NAs correctly", {
  ped <- data.frame(
    ID = c("A", "B", "C"),
    momID = c(NA, NA, "A"),
    dadID = c(NA, NA, "B"),
    stringsAsFactors = FALSE
  )

  result <- normalizeIDs(ped)

  # Founders should still have NA parents
  expect_true(is.na(result$momID[1]))
  expect_true(is.na(result$dadID[1]))
  expect_true(is.na(result$momID[2]))
  expect_true(is.na(result$dadID[2]))

  # Non-founder should have non-NA parents
  expect_false(is.na(result$momID[3]))
  expect_false(is.na(result$dadID[3]))
})

test_that("restoreIDs roundtrips back to original", {
  ped <- data.frame(
    ID = c("A", "B", "C", "D"),
    momID = c(NA, NA, "A", "A"),
    dadID = c(NA, NA, "B", "B"),
    stringsAsFactors = FALSE
  )

  result <- normalizeIDs(ped)
  restored <- restoreIDs(result)

  expect_equal(restored$ID, ped$ID)
  expect_equal(restored$momID, ped$momID)
  expect_equal(restored$dadID, ped$dadID)
})

test_that("restoreIDs restores original numeric types", {
  ped <- data.frame(
    ID = c(1L, 2L, 3L),
    momID = c(NA_integer_, NA_integer_, 1L),
    dadID = c(NA_integer_, NA_integer_, 2L)
  )

  result <- normalizeIDs(ped)
  restored <- restoreIDs(result)

  expect_type(restored$ID, "integer")
  expect_equal(restored$ID, ped$ID)
  expect_equal(restored$momID, ped$momID)
  expect_equal(restored$dadID, ped$dadID)
})

test_that("normalizeIDs with remap=FALSE attaches map but keeps original IDs", {
  ped <- data.frame(
    ID = c("X", "Y", "Z"),
    momID = c(NA, NA, "X"),
    dadID = c(NA, NA, "Y"),
    stringsAsFactors = FALSE
  )

  result <- normalizeIDs(ped, remap = FALSE)

  # IDs should be unchanged
  expect_equal(result$ID, ped$ID)

  # But map should still be attached
  expect_false(is.null(attr(result, "id_map")))
})

test_that("idMap collects IDs from all specified columns", {
  ped <- data.frame(
    ID = c("A", "B"),
    momID = c(NA, "C"),
    dadID = c(NA, "D"),
    stringsAsFactors = FALSE
  )

  map <- idMap(ped, c("ID", "momID", "dadID"))

  # Should include A, B, C, D (C and D only appear in parent columns)
  expect_equal(nrow(map), 4L)
  expect_true(all(c("A", "B", "C", "D") %in% map$original_id))
})

test_that("normalizeIDs skips missing columns silently", {
  ped <- data.frame(
    ID = c(1, 2, 3),
    momID = c(NA, NA, 1),
    dadID = c(NA, NA, 2)
  )

  # famID doesn't exist, should not error
  result <- normalizeIDs(ped, id_cols = c("ID", "momID", "dadID", "famID"))
  expect_true(is.data.frame(result))
})

test_that("relabelMatrix translates dimnames", {
  m <- matrix(1:9, 3, 3, dimnames = list(1:3, 1:3))
  map <- data.frame(
    original_id = c("Alice", "Bob", "Carol"),
    numeric_id = 1:3,
    stringsAsFactors = FALSE
  )

  result <- relabelMatrix(m, map)
  expect_equal(rownames(result), c("Alice", "Bob", "Carol"))
  expect_equal(colnames(result), c("Alice", "Bob", "Carol"))
  # Values unchanged
  expect_equal(as.vector(result), 1:9)
})

test_that("normalizeIDs works with numeric IDs that aren't sequential", {
  ped <- data.frame(
    ID = c(10, 20, 30, 40),
    momID = c(NA, NA, 10, 10),
    dadID = c(NA, NA, 20, 20)
  )

  result <- normalizeIDs(ped)

  # Should be remapped to 1:4
  expect_true(all(result$ID %in% 1:4))

  # Roundtrip
  restored <- restoreIDs(result)
  expect_equal(as.numeric(restored$ID), ped$ID)
})
