
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
