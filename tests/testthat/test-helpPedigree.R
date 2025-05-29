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
