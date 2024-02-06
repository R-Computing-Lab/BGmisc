test_that("simulated pedigree generates expected data structure", {
  set.seed(5)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7

  results <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  # Check that dimnames are correct
  expect_equal(length(results$ID), 57, tolerance = 1e-8)
  expect_equal(length(results), 7, tolerance = 1e-8)

  # check number of generations
  expect_equal(max(results$gen), Ngen, tolerance = 1e-8)

  # check number of sex ratio
  expect_equal(mean(results$sex == "M"), sexR, tolerance = .05)
})

test_that("simulated pedigree plots correctly", {
  set.seed(5)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7

  results <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  expect_no_error(plotPedigree(results, verbose = FALSE))

  kpc <- 2
  results2 <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  results2$fam <- paste0("fam 2")
  results <- rbind(results, results2)
  expect_output(plotPedigree(results, verbose = TRUE))
})

test_that("simulatePedigree verbose prints updates", {
  set.seed(5)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7

  expect_output(simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, verbose = TRUE), regexp = "Let's build the connection within each generation first")
})
