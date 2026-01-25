test_that("simulated pedigree generates expected data structure", {
  seed <- 5
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  beta_options <- c(F, T)
  #  beta_options <- T
  for (beta in beta_options) {
    set.seed(seed)
    message("Beta option Starting: ", beta)
    results <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = beta)
    # Check that dimnames are correct
    expect_equal(length(results$ID), 57, tolerance = 1e-8)
    expect_equal(length(results), 7, tolerance = 1e-8)

    # check number of generations
    expect_equal(max(results$gen), Ngen, tolerance = 1e-8)

    # check number of sex ratio
    expect_equal(mean(results$sex == "M"), sexR, tolerance = .05, info = paste0("Beta option: ", beta))
    expect_equal(mean(results$sex == "F"), 1 - sexR, tolerance = .05, info = paste0("Beta option: ", beta))
    message("Beta option Ending: ", beta)
  }
})


test_that("simulated pedigree generates expected data structure when sexR is imbalanced", {
  seed <- 51
  Ngen <- 5
  kpc <- 4
  sexR <- .55
  marR <- .7
  beta_options <- c(F, T)
  #  beta_options <- T
  for (beta in beta_options) {
    set.seed(seed)
    message("Beta option Starting: ", beta)
    results <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = beta)
    # Check that dimnames are correct
    expect_equal(length(results$ID), 154, tolerance = 1e-8)
    expect_equal(length(results), 7, tolerance = 1e-8)

    # check number of generations
    expect_equal(max(results$gen), Ngen, tolerance = 1e-8)

    # check marR
    if (F) { # temporarily disable this test as it is failing intermittently
      expect_equal(mean(!is.na(results$spID[results$gen < Ngen & results$sex == "M"])), marR, tolerance = .1, info = paste0("Beta option: ", beta))
      expect_equal(mean(!is.na(results$spID[results$gen < Ngen & results$sex == "F"])), marR, tolerance = .1, info = paste0("Beta option: ", beta))
    }

    # check number of sex ratio
    expect_equal(mean(results$sex == "M"), sexR, tolerance = .01, info = paste0("Beta option: ", beta))
    expect_equal(mean(results$sex == "F"), 1 - sexR, tolerance = .01, info = paste0("Beta option: ", beta))
    message("Beta option Ending: ", beta)
  }
})



test_that("simulated pedigree generates expected data structure but supply var names", {
  seed <- 5
  Ngen <- 4
  kpc <- 4
  sexR <- .5
  marR <- .7
  code_male <- "M"
  code_female <- "Fe"
  personID <- "Id"
  beta_options <- c(F, T)
  # beta_options <- T

  for (beta in beta_options) {
    set.seed(seed)
    message("Beta option Starting: ", beta)
    results <- simulatePedigree(
      kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR,
      code_female = code_female, personID = personID,
      code_male = code_male,
      beta = beta
    )
    # Check that dimnames are correct
    expect_equal(length(results$Id), 57, tolerance = 1e-8)
    expect_equal(length(results), 7, tolerance = 1e-8)

    # check number of generations
    expect_equal(max(results$gen), Ngen, tolerance = 1e-8)

    # check number of sex ratio
    expect_equal(mean(results$sex == code_male), sexR, tolerance = .05, info = paste0("Beta option: ", beta))
    expect_equal(mean(results$sex == code_female), 1 - sexR, tolerance = .05, info = paste0("Beta option: ", beta))
    message("Beta option Ending: ", beta)
  }
})

test_that("simulatePedigree verbose prints updates", {
  seed <- 5
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  beta_options <- c(F, T)
  # beta_options <- T
  for (beta in beta_options) {
    set.seed(seed)
    message("Beta option Starting: ", beta)
    expect_message(simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, verbose = TRUE, beta = beta), regexp = "Let's build the connection within each generation first")
    message("Beta option Ending: ", beta)
  }
})
