# Tests for describeRelatedness function

test_that("describeRelatedness handles basic relationships without generation info", {
  rel_data <- data.frame(
    ID1 = c(1, 2, 3),
    ID2 = c(2, 3, 4),
    addRel = c(0.5, 0.25, 0.0)
  )

  result <- describeRelatedness(rel_data)

  expect_true("relationship" %in% names(result))
  expect_equal(nrow(result), 3)
  expect_equal(result$relationship[1], "unknown (r=0.5)")
  expect_equal(result$relationship[2], "unknown (r=0.25)")
  expect_equal(result$relationship[3], "unrelated")
})

test_that("describeRelatedness works with pedigree data", {
  # Create simple pedigree
  ped <- data.frame(
    personID = 1:4,
    sex = c(0, 1, 0, 1),
    gen = c(1, 1, 1, 1)
  )
  
  rel_data <- data.frame(
    ID1 = c(1, 3),
    ID2 = c(2, 4),
    addRel = c(0.5, 0.5)
  )

  result <- describeRelatedness(rel_data, ped = ped, gen_col = "gen")
  expect_equal(result$relationship[1], "full siblings")
  expect_equal(result$relationship[2], "full siblings")
})

test_that("describeRelatedness identifies full siblings correctly", {
  rel_data <- data.frame(
    ID1 = c(1),
    ID2 = c(2),
    addRel = c(0.5)
  )
  rel_data$gen1 <- 1
  rel_data$gen2 <- 1

  result <- describeRelatedness(rel_data)
  expect_equal(result$relationship[1], "full siblings")
})

test_that("describeRelatedness identifies half siblings correctly", {
  rel_data <- data.frame(
    ID1 = c(1),
    ID2 = c(2),
    addRel = c(0.25)
  )
  rel_data$gen1 <- 1
  rel_data$gen2 <- 1

  result <- describeRelatedness(rel_data)
  expect_equal(result$relationship[1], "half siblings")
})

test_that("describeRelatedness identifies parent-child relationships", {
  rel_data <- data.frame(
    ID1 = c(1, 2),
    ID2 = c(2, 1),
    addRel = c(0.5, 0.5)
  )
  rel_data$gen1 <- c(1, 2)
  rel_data$gen2 <- c(2, 1)

  result <- describeRelatedness(rel_data)
  expect_equal(result$relationship[1], "parent-child")
  expect_equal(result$relationship[2], "parent-child")
})

test_that("describeRelatedness works with pedigree for sex-specific labels", {
  ped <- data.frame(
    personID = 1:4,
    sex = c(0, 1, 0, 1),
    gen = c(1, 2, 1, 2)
  )
  
  rel_data <- data.frame(
    ID1 = c(1, 1),
    ID2 = c(2, 4),
    addRel = c(0.5, 0.5)
  )

  result <- describeRelatedness(rel_data, ped = ped, use_sex = TRUE, gen_col = "gen")
  expect_equal(result$relationship[1], "mother-son")
  expect_equal(result$relationship[2], "mother-son")
})

test_that("describeRelatedness identifies grandparent-grandchild relationships", {
  rel_data <- data.frame(
    ID1 = c(1, 3),
    ID2 = c(3, 1),
    addRel = c(0.25, 0.25)
  )
  rel_data$gen1 <- c(1, 3)
  rel_data$gen2 <- c(3, 1)

  result <- describeRelatedness(rel_data)
  expect_equal(result$relationship[1], "grandparent-grandchild")
  expect_equal(result$relationship[2], "grandparent-grandchild")
})

test_that("describeRelatedness identifies aunt/uncle-niece/nephew relationships", {
  rel_data <- data.frame(
    ID1 = c(1),
    ID2 = c(2),
    addRel = c(0.25)
  )
  rel_data$gen1 <- 1
  rel_data$gen2 <- 2

  result <- describeRelatedness(rel_data)
  expect_equal(result$relationship[1], "aunt/uncle-niece/nephew")
})

test_that("describeRelatedness identifies first cousins", {
  rel_data <- data.frame(
    ID1 = c(1),
    ID2 = c(2),
    addRel = c(0.125)
  )
  rel_data$gen1 <- 2
  rel_data$gen2 <- 2

  result <- describeRelatedness(rel_data)
  expect_equal(result$relationship[1], "first cousins")
})

test_that("describeRelatedness identifies second cousins", {
  rel_data <- data.frame(
    ID1 = c(1),
    ID2 = c(2),
    addRel = c(0.0625)
  )
  rel_data$gen1 <- 3
  rel_data$gen2 <- 3

  result <- describeRelatedness(rel_data)
  expect_equal(result$relationship[1], "second cousins")
})

test_that("describeRelatedness identifies third cousins", {
  rel_data <- data.frame(
    ID1 = c(1),
    ID2 = c(2),
    addRel = c(0.03125)
  )
  rel_data$gen1 <- 4
  rel_data$gen2 <- 4

  result <- describeRelatedness(rel_data)
  expect_equal(result$relationship[1], "third cousins")
})

test_that("describeRelatedness identifies cousins once removed", {
  rel_data <- data.frame(
    ID1 = c(1, 2),
    ID2 = c(2, 3),
    addRel = c(0.125, 0.0625)
  )
  rel_data$gen1 <- c(2, 3)
  rel_data$gen2 <- c(3, 4)

  result <- describeRelatedness(rel_data)
  expect_equal(result$relationship[1], "first cousins once removed")
  expect_equal(result$relationship[2], "second cousins once removed")
})

test_that("describeRelatedness identifies cousins twice removed", {
  rel_data <- data.frame(
    ID1 = c(1, 2),
    ID2 = c(2, 3),
    addRel = c(0.125, 0.0625)
  )
  rel_data$gen1 <- c(2, 3)
  rel_data$gen2 <- c(4, 5)

  result <- describeRelatedness(rel_data)
  expect_equal(result$relationship[1], "first cousins twice removed")
  expect_equal(result$relationship[2], "second cousins twice removed")
})

test_that("describeRelatedness identifies self", {
  rel_data <- data.frame(
    ID1 = c(1, 2),
    ID2 = c(1, 2),
    addRel = c(1.0, 1.0)
  )
  rel_data$gen1 <- c(1, 2)
  rel_data$gen2 <- c(1, 2)

  result <- describeRelatedness(rel_data)
  expect_equal(result$relationship[1], "self/identical twin")
  expect_equal(result$relationship[2], "self/identical twin")
})

test_that("describeRelatedness handles sex-specific parent-child labels", {
  rel_data <- data.frame(
    ID1 = c(1, 2, 3, 4),
    ID2 = c(5, 6, 7, 8),
    addRel = c(0.5, 0.5, 0.5, 0.5)
  )
  rel_data$gen1 <- c(1, 1, 1, 1)
  rel_data$gen2 <- c(2, 2, 2, 2)
  rel_data$sex1 <- c(0, 0, 1, 1)  # 0 = female, 1 = male
  rel_data$sex2 <- c(0, 1, 0, 1)

  result <- describeRelatedness(rel_data, use_sex = TRUE)
  expect_equal(result$relationship[1], "mother-daughter")
  expect_equal(result$relationship[2], "mother-son")
  expect_equal(result$relationship[3], "father-daughter")
  expect_equal(result$relationship[4], "father-son")
})

test_that("describeRelatedness handles sex-specific child-parent labels", {
  rel_data <- data.frame(
    ID1 = c(1, 2, 3, 4),
    ID2 = c(5, 6, 7, 8),
    addRel = c(0.5, 0.5, 0.5, 0.5)
  )
  rel_data$gen1 <- c(2, 2, 2, 2)
  rel_data$gen2 <- c(1, 1, 1, 1)
  rel_data$sex1 <- c(0, 1, 0, 1)  # 0 = female, 1 = male
  rel_data$sex2 <- c(0, 0, 1, 1)

  result <- describeRelatedness(rel_data, use_sex = TRUE)
  expect_equal(result$relationship[1], "daughter-mother")
  expect_equal(result$relationship[2], "son-mother")
  expect_equal(result$relationship[3], "daughter-father")
  expect_equal(result$relationship[4], "son-father")
})

test_that("describeRelatedness handles sex-specific aunt/uncle labels", {
  rel_data <- data.frame(
    ID1 = c(1, 2, 3, 4),
    ID2 = c(5, 6, 7, 8),
    addRel = c(0.25, 0.25, 0.25, 0.25)
  )
  rel_data$gen1 <- c(1, 1, 1, 1)
  rel_data$gen2 <- c(2, 2, 2, 2)
  rel_data$sex1 <- c(0, 0, 1, 1)  # 0 = female, 1 = male
  rel_data$sex2 <- c(0, 1, 0, 1)

  result <- describeRelatedness(rel_data, use_sex = TRUE)
  expect_equal(result$relationship[1], "aunt-niece")
  expect_equal(result$relationship[2], "aunt-nephew")
  expect_equal(result$relationship[3], "uncle-niece")
  expect_equal(result$relationship[4], "uncle-nephew")
})

test_that("describeRelatedness handles sex-specific grandparent labels", {
  rel_data <- data.frame(
    ID1 = c(1, 2, 3, 4),
    ID2 = c(5, 6, 7, 8),
    addRel = c(0.25, 0.25, 0.25, 0.25)
  )
  rel_data$gen1 <- c(1, 1, 1, 1)
  rel_data$gen2 <- c(3, 3, 3, 3)
  rel_data$sex1 <- c(0, 0, 1, 1)  # 0 = female, 1 = male
  rel_data$sex2 <- c(0, 1, 0, 1)

  result <- describeRelatedness(rel_data, use_sex = TRUE)
  expect_equal(result$relationship[1], "grandmother-granddaughter")
  expect_equal(result$relationship[2], "grandmother-grandson")
  expect_equal(result$relationship[3], "grandfather-granddaughter")
  expect_equal(result$relationship[4], "grandfather-grandson")
})

test_that("describeRelatedness custom column names work", {
  rel_data <- data.frame(
    person1 = c(1),
    person2 = c(2),
    r_coef = c(0.5)
  )
  rel_data$gen1 <- 1
  rel_data$gen2 <- 1

  result <- describeRelatedness(
    rel_data,
    ID1_col = "person1",
    ID2_col = "person2",
    add_col = "r_coef"
  )
  expect_equal(result$relationship[1], "full siblings")
})

test_that("describeRelatedness return_list option works", {
  rel_data <- data.frame(
    ID1 = c(1),
    ID2 = c(2),
    addRel = c(0.5)
  )
  rel_data$gen1 <- 1
  rel_data$gen2 <- 1

  result <- describeRelatedness(rel_data, return_list = TRUE)
  expect_true(is.list(result))
  expect_true("data" %in% names(result))
  expect_true("relationships" %in% names(result))
  expect_equal(result$relationships[1], "full siblings")
})

test_that("describeRelatedness validates input correctly", {
  # Not a data.frame
  expect_error(
    describeRelatedness(c(1, 2, 3)),
    "rel_df must be a data.frame"
  )

  # Missing ID columns
  expect_error(
    describeRelatedness(data.frame(a = 1, b = 2)),
    "rel_df must contain 'ID1' and 'ID2' columns"
  )

  # Missing addRel column
  expect_error(
    describeRelatedness(data.frame(ID1 = 1, ID2 = 2)),
    "Column 'addRel' not found in rel_df"
  )
})

test_that("describeRelatedness handles use_sex warning when sex columns missing", {
  rel_data <- data.frame(
    ID1 = c(1),
    ID2 = c(2),
    addRel = c(0.5)
  )
  rel_data$gen1 <- 1
  rel_data$gen2 <- 2

  expect_warning(
    result <- describeRelatedness(rel_data, use_sex = TRUE),
    "use_sex is TRUE but sex columns are not available"
  )
  expect_equal(result$relationship[1], "parent-child")
})

test_that("describeRelatedness handles custom sex codes", {
  rel_data <- data.frame(
    ID1 = c(1, 2),
    ID2 = c(3, 4),
    addRel = c(0.5, 0.5)
  )
  rel_data$gen1 <- c(1, 1)
  rel_data$gen2 <- c(2, 2)
  rel_data$sex1 <- c("F", "M")
  rel_data$sex2 <- c("M", "F")

  result <- describeRelatedness(
    rel_data,
    use_sex = TRUE,
    code_male = "M",
    code_female = "F"
  )
  expect_equal(result$relationship[1], "mother-son")
  expect_equal(result$relationship[2], "father-daughter")
})

test_that("describeRelatedness handles great-grandparent relationships", {
  rel_data <- data.frame(
    ID1 = c(1),
    ID2 = c(2),
    addRel = c(0.125)
  )
  rel_data$gen1 <- 1
  rel_data$gen2 <- 4

  result <- describeRelatedness(rel_data)
  expect_equal(result$relationship[1], "great-grandparent-great-grandchild")
})

test_that("describeRelatedness handles great-great-grandparent relationships", {
  rel_data <- data.frame(
    ID1 = c(1),
    ID2 = c(2),
    addRel = c(0.0625)
  )
  rel_data$gen1 <- 1
  rel_data$gen2 <- 5

  result <- describeRelatedness(rel_data)
  expect_equal(result$relationship[1], "great-great-grandparent-great-great-grandchild")
})

test_that("describeRelatedness handles multiple relationships in one call", {
  rel_data <- data.frame(
    ID1 = c(1, 1, 1, 2, 2, 3),
    ID2 = c(2, 3, 4, 3, 4, 4),
    addRel = c(0.5, 0.25, 0.125, 0.5, 0.25, 0.5)
  )
  rel_data$gen1 <- c(1, 1, 1, 1, 1, 2)
  rel_data$gen2 <- c(1, 2, 2, 1, 3, 2)

  result <- describeRelatedness(rel_data)
  expect_equal(nrow(result), 6)
  expect_equal(result$relationship[1], "full siblings")
  expect_equal(result$relationship[2], "aunt/uncle-niece/nephew")
  expect_equal(result$relationship[3], "first cousins once removed")
  expect_equal(result$relationship[4], "full siblings")
  expect_equal(result$relationship[5], "grandparent-grandchild")
  expect_equal(result$relationship[6], "parent-child")
})

test_that("describeRelatedness handles unknown relationships gracefully", {
  rel_data <- data.frame(
    ID1 = c(1, 2),
    ID2 = c(2, 3),
    addRel = c(0.15, 0.333)
  )
  rel_data$gen1 <- c(1, 1)
  rel_data$gen2 <- c(1, 1)

  result <- describeRelatedness(rel_data)
  expect_true(grepl("unknown", result$relationship[1]))
  expect_true(grepl("unknown", result$relationship[2]))
})
