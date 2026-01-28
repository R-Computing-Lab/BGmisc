test_that("simulated pedigree generates expected data structure", {
  seed <- 5
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  beta_options <- c(F, T)
  strict_tolerance <- 1e-8
  sex_tolerance <- .035
  #  beta_options <- T
  for (beta in beta_options) {
    set.seed(seed)
    message("Beta option Starting: ", beta)
    results <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = beta)
    # Check that dimnames are correct
    expect_equal(length(results$ID), 57, tolerance = strict_tolerance)
    expect_equal(length(results), 7, tolerance = strict_tolerance)

    # check number of generations
    expect_equal(max(results$gen), Ngen, tolerance = strict_tolerance)

    # check
    # check number of sex ratio
    sex_mean_male <- mean(results$sex == "M")
    sex_mean_female <- mean(results$sex == "F")

    expect_equal(sex_mean_male, sex_mean_female, tolerance = sex_tolerance, info = paste0("Beta option: ", beta))
    # check number of sex ratio
    expect_equal(mean(results$sex == "M"), sexR, tolerance = sex_tolerance, info = paste0("Beta option: ", beta))
    expect_equal(mean(results$sex == "F"), 1 - sexR, tolerance = sex_tolerance, info = paste0("Beta option: ", beta))
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
  strict_tolerance <- 1e-8
  sex_tolerance <- .03
  #  beta_options <- T
  for (beta in beta_options) {
    set.seed(seed)
    message("Beta option Starting: ", beta)
    results <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = beta)
    # Check that dimnames are correct
    expect_equal(length(results$ID), 154, tolerance = strict_tolerance)
    expect_equal(length(results), 7, tolerance = strict_tolerance)

    # check number of generations
    expect_equal(max(results$gen), Ngen, tolerance = strict_tolerance)

    # check marR


    # check number of sex ratio
    sex_mean_male <- mean(results$sex == "M")
    sex_mean_female <- mean(results$sex == "F")

    expect_lt(sex_mean_female, sex_mean_male)

    expect_equal(sex_mean_male, sexR, tolerance = sex_tolerance, info = paste0("Beta option: ", beta))
    expect_equal(sex_mean_female, 1 - sexR, tolerance = sex_tolerance, info = paste0("Beta option: ", beta))

    message("Beta option Ending: ", beta)
  }
})

test_that("simulated pedigree generates expected data structure when sexR is imbalanced in opposite", {
  seed <- 51
  Ngen <- 6
  kpc <- 4
  sexR <- .45
  marR <- .7
  beta_options <- c(F, T)
  strict_tolerance <- 1e-8
  sex_tolerance <- .03

  #  beta_options <- T
  for (beta in beta_options) {
    set.seed(seed)
    message("Beta option Starting: ", beta)
    results <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = beta)
    # Check that dimnames are correct
    expect_equal(length(results$ID), 424, tolerance = strict_tolerance)
    expect_equal(length(results), 7, tolerance = strict_tolerance)

    # check number of generations
    expect_equal(max(results$gen), Ngen, tolerance = strict_tolerance)


    # check number of sex ratio
    sex_mean_male <- mean(results$sex == "M")
    sex_mean_female <- mean(results$sex == "F")

    expect_lt(sex_mean_male, sex_mean_female)

    expect_equal(sex_mean_male, sexR, tolerance = sex_tolerance, info = paste0("Beta option: ", beta))
    expect_equal(sex_mean_female, 1 - sexR, tolerance = sex_tolerance, info = paste0("Beta option: ", beta))
    message("Beta option Ending: ", beta)
  }
})

test_that("simulated pedigree generates expected data structure but supply var names", {
  seed <- 5
  Ngen <- 4
  kpc <- 4
  sexR <- .45
  marR <- .7
  code_male <- "M"
  code_female <- "Fe"
  personID <- "Id"
  beta_options <- c(F, T)
  strict_tolerance <- 1e-8
  sex_tolerance <- .03
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
    expect_equal(length(results$Id), 57, tolerance = strict_tolerance)
    expect_equal(length(results), 7, tolerance = strict_tolerance)

    # check number of generations
    expect_equal(max(results$gen), Ngen, tolerance = strict_tolerance)

    # check number of sex ratio

    # check number of sex ratio
    sex_mean_male <- mean(results$sex == code_male)
    sex_mean_female <- mean(results$sex == code_female)

    expect_lt(sex_mean_male, sex_mean_female)


    expect_equal(sex_mean_male, sexR, tolerance = sex_tolerance, info = paste0("Beta option: ", beta))
    expect_equal(sex_mean_female, 1 - sexR, tolerance = sex_tolerance, info = paste0("Beta option: ", beta))
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

test_that("simulatePedigree accepts string aliases for beta parameter", {
  seed <- 5
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  strict_tolerance <- 1e-8
  
  # Test that "optimized" string alias works
  set.seed(seed)
  result_true <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = TRUE)
  
  set.seed(seed)
  result_optimized <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = "optimized")
  
  # Results should be identical when using TRUE vs "optimized"
  expect_equal(nrow(result_true), nrow(result_optimized))
  expect_equal(ncol(result_true), ncol(result_optimized))
  expect_equal(result_true$ID, result_optimized$ID)
  
  # Test that "base" string alias works
  set.seed(seed)
  result_false <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = FALSE)
  
  set.seed(seed)
  result_base <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = "base")
  
  # Results should be identical when using FALSE vs "base"
  expect_equal(nrow(result_false), nrow(result_base))
  expect_equal(ncol(result_false), ncol(result_base))
  expect_equal(result_false$ID, result_base$ID)
  
  # Test that invalid beta values throw errors
  expect_error(
    simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = "invalid"),
    "Invalid value for beta parameter"
  )
  
  # Test that "indexed" throws appropriate error
  expect_error(
    simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = "indexed"),
    "not yet implemented"
  )
})
