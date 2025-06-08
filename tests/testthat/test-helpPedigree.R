test_that("createGenDataFrame works", {
  sizeGens <- c(10, 15, 20)
  genIndex <- 2
  idGen <- 1:15

  df_Ngen <- createGenDataFrame(sizeGens, genIndex, idGen)

  expect_equal(nrow(df_Ngen), sizeGens[genIndex])
  expect_equal(df_Ngen$gen[1], genIndex)
  expect_equal(df_Ngen$fam[1], "fam 1")
  expect_equal(df_Ngen$id[1], idGen[1])
})
test_that("determineSex works", {
  idGen <- 1:15
  sexR <- 0.6
  set.seed(123) # For reproducibility
  df <- determineSex(idGen, sexR)

  expect_equal(length(df), length(idGen))
  expect_true(all(df %in% c("M", "F")))

  expect_equal(sum(df == "M") / length(df), sexR, tolerance = 0.1)
})


test_that("addPersonToPed works as expected", {
  # Initial pedigree data frame
  ped <- data.frame(
    personID = c(1, 2),
    name = c("Alice", "Bob"),
    sex = c("F", "M"),
    momID = c(NA, NA),
    dadID = c(NA, NA),
    twinID = c(NA_integer_, NA_integer_),
    stringsAsFactors = FALSE
  )

  # Add person with all fields specified
  updated <- addPersonToPed(
    ped,
    name = "Charlie",
    sex = "M",
    momID = 1,
    dadID = 2,
    twinID = NA,
    personID = 10
  )

  expect_equal(nrow(updated), 3)
  expect_equal(updated$personID[3], 10)
  expect_equal(updated$name[3], "Charlie")
  expect_equal(updated$sex[3], "M")
  expect_equal(updated$momID[3], 1)
  expect_equal(updated$dadID[3], 2)
  expect_true(is.na(updated$twinID[3]))

  # Add person with generated ID
  updated2 <- addPersonToPed(ped, name = "Dana", sex = "F")
  expect_equal(nrow(updated2), 3)
  expect_equal(updated2$name[3], "Dana")
  expect_equal(updated2$sex[3], "F")
  expect_equal(updated2$personID[3], max(ped$personID, na.rm = TRUE) + 1)

  # Add person with missing optional fields
  updated3 <- addPersonToPed(ped)
  expect_equal(nrow(updated3), 3)
  expect_true(is.na(updated3$name[3]))
  expect_true(is.na(updated3$sex[3]))
  expect_true(is.na(updated3$twinID[3]))
  expect_true(is.na(updated3$momID[3]))
  expect_true(is.na(updated3$dadID[3]))

  expect_equal(updated3$personID[3], max(ped$personID, na.rm = TRUE) + 1)
})

test_that("addPersonToPed works as expected with zygosity", {
  # Initial pedigree data frame
  ped <- data.frame(
    personID = c(1, 2),
    name = c("Alice", "Bob"),
    sex = c("F", "M"),
    momID = c(NA, NA),
    dadID = c(NA, NA),
    twinID = c(NA_integer_, NA_integer_),
    zygosity = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  # Add person with all fields specified
  updated <- addPersonToPed(
    ped,
    name = "Charlie",
    sex = "M",
    momID = 1,
    dadID = 2,
    twinID = NA,
    personID = 10,
    zygosity = NA
  )

  expect_equal(nrow(updated), 3)
  expect_equal(updated$personID[3], 10)
  expect_equal(updated$name[3], "Charlie")
  expect_equal(updated$sex[3], "M")
  expect_equal(updated$momID[3], 1)
  expect_equal(updated$dadID[3], 2)
  expect_true(is.na(updated$twinID[3]))
  expect_true(is.na(updated$zygosity[3]))

  # Add person with generated ID
  updated2 <- addPersonToPed(ped, name = "Dana", sex = "F")
  expect_equal(nrow(updated2), 3)
  expect_equal(updated2$name[3], "Dana")
  expect_equal(updated2$sex[3], "F")
  expect_equal(updated2$personID[3], max(ped$personID, na.rm = TRUE) + 1)
  expect_true(is.na(updated2$zygosity[3]))

  # Add person with missing optional fields
  updated3 <- addPersonToPed(updated2)
  expect_equal(nrow(updated3), 4)
  expect_true(is.na(updated3$name[4]))
  expect_true(is.na(updated3$sex[4]))
  expect_true(is.na(updated3$twinID[4]))
  expect_true(is.na(updated3$momID[4]))
  expect_true(is.na(updated3$dadID[4]))

  expect_equal(updated3$personID[4], max(ped$personID, na.rm = TRUE) + 2)
})
