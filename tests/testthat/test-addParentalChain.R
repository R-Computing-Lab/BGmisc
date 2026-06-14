get_one <- function(x, id_col, id) {
  idx <- which(x[[id_col]] == id)
  testthat::expect_length(idx, 1)
  idx
}

get_chain <- function(x, id_col, id, chain_col) {
  x[[chain_col]][[get_one(x, id_col, id)]]
}

get_value <- function(x, id_col, id, value_col) {
  x[[value_col]][get_one(x, id_col, id)]
}


test_that("addPaternalChain adds ordered paternal chains", {

  ped <- data.frame(
    personID = c("ego", "dad", "pat_gf", "pat_ggf", "mom", "mat_gm"),
    dadID = c("dad", "pat_gf", "pat_ggf", NA, NA, NA),
    momID = c("mom", NA, NA, NA, "mat_gm", NA),
    stringsAsFactors = FALSE
  )

  result <- addPaternalChain(ped)

  expect_true("dadID_chain" %in% names(result))
  expect_true("dadID_chain_string" %in% names(result))

  expect_identical(
    get_chain(result, "personID", "ego", "dadID_chain"),
    c("dad", "pat_gf", "pat_ggf")
  )

  expect_identical(
    get_value(result, "personID", "ego", "dadID_chain_string"),
    "dad|pat_gf|pat_ggf"
  )

  expect_identical(
    get_chain(result, "personID", "pat_ggf", "dadID_chain"),
    character(0)
  )

  expect_true(
    is.na(get_value(result, "personID", "pat_ggf", "dadID_chain_string"))
  )
})



test_that("addMaternalChain adds ordered maternal chains", {

  ped <- data.frame(
    personID = c("ego", "dad", "pat_gf", "mom", "mat_gm", "mat_ggm"),
    dadID = c("dad", "pat_gf", NA, NA, NA, NA),
    momID = c("mom", NA, NA, "mat_gm", "mat_ggm", NA),
    stringsAsFactors = FALSE
  )

  result <- addMaternalChain(ped)

  expect_true("momID_chain" %in% names(result))
  expect_true("momID_chain_string" %in% names(result))

  expect_identical(
    result$momID_chain[[result$personID == "ego"]],
    c("mom", "mat_gm", "mat_ggm")
  )

  expect_identical(
    result$momID_chain_string[result$personID == "ego"],
    "mom|mat_gm|mat_ggm"
  )

  expect_identical(
    result$momID_chain[[result$personID == "mat_ggm"]],
    character(0)
  )

  expect_true(
    is.na(result$momID_chain_string[result$personID == "mat_ggm"])
  )
})


test_that("addParentalChain can add paternal and maternal chains with custom output columns", {

  ped <- data.frame(
    personID = c("ego", "dad", "pat_gf", "mom", "mat_gm"),
    dadID = c("dad", "pat_gf", NA, NA, NA),
    momID = c("mom", NA, NA, "mat_gm", NA),
    stringsAsFactors = FALSE
  )

  paternal_result <- addParentalChain(
    ped = ped,
    chain_col = "custom_pat_chain",
    chain_string_col = "custom_pat_chain_string",
    collapse = " > ",
    component = "dadID"
  )

  maternal_result <- addParentalChain(
    ped = ped,
    chain_col = "custom_mat_chain",
    chain_string_col = "custom_mat_chain_string",
    collapse = " > ",
    component = "momID"
  )

  expect_identical(
    paternal_result$custom_pat_chain[[paternal_result$personID == "ego"]],
    c("dad", "pat_gf")
  )

  expect_identical(
    paternal_result$custom_pat_chain_string[paternal_result$personID == "ego"],
    "dad > pat_gf"
  )

  expect_identical(
    maternal_result$custom_mat_chain[[maternal_result$personID == "ego"]],
    c("mom", "mat_gm")
  )

  expect_identical(
    maternal_result$custom_mat_chain_string[maternal_result$personID == "ego"],
    "mom > mat_gm"
  )
})


test_that("addParentalChain works with custom input column names", {

  ped <- data.frame(
    id = c("ego", "dad", "pat_gf", "mom", "mat_gm"),
    father = c("dad", "pat_gf", NA, NA, NA),
    mother = c("mom", NA, NA, "mat_gm", NA),
    stringsAsFactors = FALSE
  )

  result <- addParentalChain(
    ped = ped,
    personID = "id",
    dadID = "father",
    momID = "mother",
    chain_col = "chain",
    chain_string_col = "chain_string",
    component = "dadID"
  )

  expect_identical(
    result$chain[[result$id == "ego"]],
    c("dad", "pat_gf")
  )

  expect_identical(
    result$chain_string[result$id == "ego"],
    "dad|pat_gf"
  )
})


test_that("addParentalChain coerces numeric IDs to character chains", {

  ped <- data.frame(
    personID = c(1, 2, 3, 4, 5),
    dadID = c(2, 3, NA, NA, NA),
    momID = c(4, NA, NA, 5, NA)
  )

  result <- addPaternalChain(ped)

  expect_identical(
    result$dadID_chain[[result$personID == 1]],
    c("2", "3")
  )

  expect_identical(
    result$dadID_chain_string[result$personID == 1],
    "2|3"
  )
})


test_that("addPaternalLineFlag flags whether anchor appears in paternal chain", {

  ped <- data.frame(
    personID = c("ego", "sibling", "dad", "pat_gf", "unrelated"),
    stringsAsFactors = FALSE
  )

  ped$dadID_chain <- list(
    c("dad", "pat_gf"),
    c("dad", "pat_gf"),
    c("pat_gf"),
    character(0),
    character(0)
  )

  result <- addPaternalLineFlag(
    ped = ped,
    anchor_id = "pat_gf",
    flag_col = "descends_from_pat_gf"
  )

  expect_true("descends_from_pat_gf" %in% names(result))

  expect_identical(
    result$descends_from_pat_gf,
    c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
})


test_that("addMaternalLineFlag flags whether anchor appears in maternal chain", {

  ped <- data.frame(
    personID = c("ego", "sibling", "mom", "mat_gm", "unrelated"),
    stringsAsFactors = FALSE
  )

  ped$momID_chain <- list(
    c("mom", "mat_gm"),
    c("mom", "mat_gm"),
    c("mat_gm"),
    character(0),
    character(0)
  )

  result <- addMaternalLineFlag(
    ped = ped,
    anchor_id = "mat_gm",
    flag_col = "descends_from_mat_gm"
  )

  expect_true("descends_from_mat_gm" %in% names(result))

  expect_identical(
    result$descends_from_mat_gm,
    c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
})


test_that("addParentalLineFlag works with explicit paternal and maternal chain columns", {

  ped <- data.frame(
    personID = c("ego", "dad", "mom", "pat_gf", "mat_gm", "unrelated"),
    stringsAsFactors = FALSE
  )

  ped$dadID_chain <- list(
    c("dad", "pat_gf"),
    c("pat_gf"),
    character(0),
    character(0),
    character(0),
    character(0)
  )

  ped$momID_chain <- list(
    c("mom", "mat_gm"),
    character(0),
    c("mat_gm"),
    character(0),
    character(0),
    character(0)
  )

  paternal_result <- addParentalLineFlag(
    ped = ped,
    anchor_id = "pat_gf",
    flag_col = "paternal_flag",
    chain_col = "dadID_chain",
    component = "dadID"
  )

  maternal_result <- addParentalLineFlag(
    ped = ped,
    anchor_id = "mat_gm",
    flag_col = "maternal_flag",
    chain_col = "momID_chain",
    component = "momID"
  )

  expect_identical(
    paternal_result$paternal_flag,
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )

  expect_identical(
    maternal_result$maternal_flag,
    c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
  )
})


test_that("addParentalLineFlag coerces numeric anchor IDs to character", {

  ped <- data.frame(
    personID = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  ped$dadID_chain <- list(
    c("2", "3"),
    c("3"),
    character(0),
    character(0)
  )

  result <- addPaternalLineFlag(
    ped = ped,
    anchor_id = 3,
    flag_col = "has_ancestor_3"
  )

  expect_identical(
    result$has_ancestor_3,
    c(TRUE, TRUE, FALSE, FALSE)
  )
})


test_that("addParentalLineFlag errors for invalid component", {

  ped <- data.frame(
    personID = c("ego"),
    stringsAsFactors = FALSE
  )

  ped$dadID_chain <- list(character(0))

  expect_error(
    addParentalLineFlag(
      ped = ped,
      anchor_id = "x",
      flag_col = "flag",
      chain_col = "dadID_chain",
      component = "invalid"
    )
  )
})
