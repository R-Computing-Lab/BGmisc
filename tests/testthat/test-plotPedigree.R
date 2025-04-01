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


test_that("pedigree plots correctly with affected variables", {
  set.seed(5)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7

  results <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  results$affected <- rbinom(n = nrow(results), size = 1, prob = .1)
  expect_output(plotPedigree(results, verbose = TRUE, affected = "affected"))
  expect_output(plotPedigree(results, verbose = TRUE, affected = results$affected))

  # file.remove("Rplots.pdf")
})
# file.remove("Rplots.pdf")
