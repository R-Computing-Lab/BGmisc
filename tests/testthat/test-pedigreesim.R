test_that("simulated pedigree generates expected data structure", {
  set.seed(5)
  Ngen <- 4
  kpc  <-  4
  sexR  <-  .50
  marR  <-  .7

  results <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  # Check that dimnames are correct
  expect_equal(length(results$ID), 57, tolerance = 1e-8)
  expect_equal(length(results), 7, tolerance = 1e-8)

  # check number of generations
  expect_equal(max(results$gen), Ngen, tolerance = 1e-8)

  # check number of sex ratio
  expect_equal(mean(results$sex == "M"), sexR, tolerance = .05)
})
