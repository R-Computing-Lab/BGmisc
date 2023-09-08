test_that("hazard data loads", {
  expect_silent(data(hazard))
})

test_that("inbreeding data loads", {
  expect_silent(data(inbreeding))
})

test_that("ped2fam gets the right families for hazard data", {
  data(hazard)
  ds <- ped2fam(hazard, famID='newFamID')
  tab <- table(ds$FamID, ds$newFamID)
  expect_equal(ds$FamID, ds$newFamID)
})

test_that("ped2graph produces a graph", {
  expect_silent(data(inbreeding))
  g <- ped2graph(inbreeding)
  expect_true(inherits(g, 'igraph'))
})

test_that("ped2add produces correct matrix dims, values, and dimnames", {
  data(hazard)
  add <- ped2add(hazard)
  # Check dimension
  expect_equal(dim(add), c(43, 43))
  # Check several values
  expect_true(all(diag(add) == 1))
  expect_equal(add, t(add))
  expect_equal(add[2, 1], 0)
  expect_equal(add[10, 1], .25)
  expect_equal(add[9, 1], 0)
  expect_equal(add['5', '6'], .5)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(hazard$ID))
})

#test_that("ped2mit produces correct matrix dims, values, and dimnames", {
#  # Check dimension
#  # Check several values
#  # Check that dimnames are correct
#  expect_silent(data(inbreeding))
#})

#test_that("ped2cn produces correct matrix dims, values, and dimnames", {
#  # Check dimension
#  # Check several values
#  # Check that dimnames are correct
#  expect_silent(data(inbreeding))
#})

#test_that("ped2ce produces correct matrix dims, values, and dimnames", {
#  # Check dimension
#  # Check several values
#  # Check that dimnames are correct
#  expect_silent(data(inbreeding))
#})
