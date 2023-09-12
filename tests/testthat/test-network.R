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



test_that("ped2add produces correct matrix dims, values, and dimnames for hazard", {
  data(hazard)
  add <- ped2add(hazard)
  # Check dimension
  expect_equal(dim(add), c(nrow(hazard), nrow(hazard)))
  # Check several values
  expect_true(all(diag(add) == 1))
  expect_equal(add, t(add))
  expect_equal(add[2, 1], 0)
  expect_equal(add[10, 1], .25)
  expect_equal(add[9, 1], 0)
  expect_equal(add["5", "6"], .5)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(hazard$ID))
})

test_that("ped2add produces correct matrix dims, values, and dimnames for inbreeding data", {
  data(inbreeding)
  add <- ped2add(inbreeding)
  # Check dimension
  expect_equal(dim(add), c(nrow(inbreeding), nrow(inbreeding)))
  # Check several values
  expect_true(all(diag(add) >= 1))
  expect_equal(add, t(add))
  expect_equal(add[2, 1], 0)
  expect_equal(add[6, 1], .5)
  expect_equal(add[113, 113], 1.1250)
  expect_equal(add["113", "112"], 0.62500)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})
test_that("ped2add flattens diagonal for inbreeding data", {
  data(inbreeding)
  add <- ped2add(inbreeding, flatten.diag = TRUE)
  # Check dimension
  expect_equal(dim(add), c(nrow(inbreeding), nrow(inbreeding)))
  # Check several values
  expect_true(all(diag(add) == 1))
  expect_equal(add, t(add))
  expect_equal(add[2, 1], 0)
  expect_equal(add[6, 1], .5)
  expect_equal(add[113, 113], 1)
  expect_equal(add["113", "112"], 0.62500)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})
test_that("ped2mit produces correct matrix dims, values, and dimnames for inbreeding", {
  # Check dimension
  data(inbreeding)
  mit <- ped2mit(inbreeding)
  # Check dimension
  expect_equal(dim(mit), c(nrow(inbreeding), nrow(inbreeding)))
  # Check several values
  expect_true(all(diag(mit) == 1))
  expect_equal(mit, t(mit))
  expect_equal(mit[2, 1], 0)
  expect_equal(mit[6, 1], 1)
  expect_equal(mit[113, 113], 1)
  expect_equal(mit["113", "112"], 1)
  # Check that dimnames are correct
  dn <- dimnames(mit)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})

test_that("ped2mit produces correct matrix dims, values, and dimnames for inbreeding", {
  # Check dimension
  data(inbreeding)
  mit <- ped2mit(inbreeding)
  # Check dimension
  expect_equal(dim(mit), c(nrow(inbreeding), nrow(inbreeding)))
  # Check several values
  expect_true(all(diag(mit) == 1))
  expect_equal(mit, t(mit))
  expect_equal(mit[2, 1], 0)
  expect_equal(mit[6, 1], 1)
  expect_equal(mit[113, 113], 1)
  expect_equal(mit["113", "112"], 1)
  # Check that dimnames are correct
  dn <- dimnames(mit)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})

test_that("ped2cn produces correct matrix dims, values, and dimnames", {
  #  # Check dimension
  data(inbreeding)
  cn <- ped2cn(inbreeding)
  expect_equal(dim(cn), c(nrow(inbreeding), nrow(inbreeding)))
  # Check several values
  expect_true(all(diag(cn) == 1))
  expect_equal(cn, t(cn))
  expect_equal(cn[2, 1], 0)
  expect_equal(cn[6, 1], 0)
  expect_equal(cn[113, 113], 1)
  expect_equal(cn["113", "112"], 1)
  # Check that dimnames are correct
  dn <- dimnames(cn)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
  #  expect_silent(data(inbreeding))
})

test_that("ped2ce produces correct matrix dims, values, and dimnames", {
  data(inbreeding)
  ce <- ped2ce(inbreeding)
  expect_equal(dim(ce), c(nrow(inbreeding), nrow(inbreeding)))
  # Check several values
  expect_true(all(diag(ce) == 1))
  expect_equal(ce, t(ce))
  expect_equal(ce[2, 1], 1)
  expect_equal(ce[6, 1], 1)
  expect_equal(ce[113, 113], 1)
  expect_equal(ce["113", "112"], 1)
  # Check that dimnames are correct
  dn <- dimnames(ce)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
  #  expect_silent(data(inbreeding))
})
