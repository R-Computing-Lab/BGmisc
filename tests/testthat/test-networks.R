test_that("hazard data loads", {
  expect_silent(data(hazard))
})

test_that("inbreeding data loads", {
  expect_silent(data(inbreeding))
})

test_that("ped2fam gets the right families for hazard data", {
  data(hazard)
  ds <- ped2fam(hazard, famID = "newFamID")
  tab <- table(ds$FamID, ds$newFamID)
  expect_equal(ds$FamID, ds$newFamID)
})

test_that("ped2fam gets the right families for inbreeding data", {
  data(inbreeding)
  ds <- ped2fam(inbreeding, famID = "newFamID")
  tab <- table(ds$FamID, ds$newFamID)
  expect_equal(ds$FamID, ds$newFamID)
})

test_that("ped2graph produces a graph for hazard data", {
  expect_silent(data(hazard))
  g <- ped2graph(hazard)
  expect_true(inherits(g, "igraph"))
})

test_that("ped2graph produces a graph for inbreeding data", {
  expect_silent(data(inbreeding))
  g <- ped2graph(inbreeding)
  expect_true(inherits(g, "igraph"))
})


