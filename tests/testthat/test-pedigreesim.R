
test_that("simulated pedigree generates expected data structure", {

  set.seed(5)
  results <- SimPed(kpc = 4, Ngen = 4, sexR = .5, marR = .7)

expect_equal(length(results$ID), 57)
expect_equal(length(results), 7)
})
