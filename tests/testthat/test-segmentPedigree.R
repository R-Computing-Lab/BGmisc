test_that("ped2fam is smart about string ids", {
  data(hazard)
  ds_num <- ped2fam(hazard, famID = "newFamID")
  expect_true(is.numeric(ds_num$ID))
  expect_true(is.numeric(ds_num$newFamID))
  hazard$ID_og <- hazard$ID
  hazard$ID <- paste0("ID", hazard$ID)
  hazard$dadID <- paste0("ID", hazard$dadID)
  hazard$dadID[hazard$dadID == "IDNA"] <- NA
  hazard$momID <- paste0("ID", hazard$momID)
  hazard$momID[hazard$momID == "IDNA"] <- NA
  expect_warning(ped2fam(hazard, famID = "newFamID"))
  ds <- suppressWarnings(ped2fam(hazard, famID = "newFamID"))
  tab <- table(ds$famID, ds$newFamID)
  expect_true(all(grepl("^ID", ds$ID)))
  ds_num_s <- ds_num[order(ds_num$ID), ]
  hazard_s <- hazard[order(hazard$ID_og), ]
  ds_s <- ds[order(ds$ID_og), ]
  expect_equal(ds_num_s$ID, hazard_s$ID_og)
  expect_equal(ds_num_s$newFamID, hazard_s$famID)
  expect_equal(ds_s$ID, hazard_s$ID)
  expect_equal(ds_num_s$newFamID, hazard_s$famID)
  expect_equal(ds_num_s$newFamID, ds_s$newFamID)
  expect_equal(ds_s$famID, ds_s$newFamID)
})


test_that("ped2fam gets the right families for hazard data", {
  data(hazard)
  ds <- ped2fam(hazard, famID = "newFamID")
  tab <- table(ds$famID, ds$newFamID)
  expect_equal(ds$famID, ds$newFamID)
})


test_that("ped2fam gets the right families for inbreeding data", {
  data(inbreeding)
  ds <- ped2fam(inbreeding, famID = "newFamID")
  tab <- table(ds$famID, ds$newFamID)
  expect_equal(ds$famID, ds$newFamID)
})

test_that("ped2graph produces a graph for hazard data with mothers", {
  expect_silent(data(hazard))
  g <- ped2graph(hazard, adjacent = "mothers")
  expect_true(inherits(g, "igraph"))
})

test_that("ped2graph produces a graph for hazard data with fathers", {
  expect_silent(data(hazard))
  g <- ped2graph(hazard, adjacent = "fathers")
  expect_true(inherits(g, "igraph"))
})

test_that("ped2graph produces a graph for inbreeding data", {
  expect_silent(data(inbreeding))
  g <- ped2graph(inbreeding)
  expect_true(inherits(g, "igraph"))
})
