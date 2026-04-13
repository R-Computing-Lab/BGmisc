test_that("simulated pedigree generates expected data structure", {
  seed <- 5
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  beta_options <- c(FALSE, TRUE)
  strict_tolerance <- 1e-8
  sex_tolerance <- .035
  base_length <- 57
  base_length_tol <- 0.2 * base_length
  beta_match_base <- FALSE
  #  beta_options <- T
  for (beta in beta_options) {
    set.seed(seed)
    message("Beta option Starting: ", beta)
    results <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = beta)
    # Check that dimnames are correct
    # Base version: exact count. Optimized version: within 20% range
    if (isFALSE(beta) || (isTRUE(beta) && beta_match_base)) {
      expect_equal(length(results$ID), base_length, tolerance = strict_tolerance)
    } else {
      expect_true(length(results$ID) >= base_length - base_length_tol && length(results$ID) <= base_length + base_length_tol,
        info = paste0("Beta=TRUE: Expected 45-70 individuals, got ", length(results$ID))
      )
    }
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
  beta_options <- c(FALSE, TRUE)
  strict_tolerance <- 1e-8
  sex_tolerance <- .03
  base_length <- 154
  base_length_tol <- 0.2 * base_length
  beta_match_base <- FALSE
  #  beta_options <- T
  for (beta in beta_options) {
    set.seed(seed)
    message("Beta option Starting: ", beta)
    results <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = beta)
    # Check that dimnames are correct
    # Base version: exact count. Optimized version: within 20% range
    if (isFALSE(beta) || (isTRUE(beta) && beta_match_base)) {
      expect_equal(length(results$ID), base_length, tolerance = strict_tolerance)
    } else {
      expect_true(length(results$ID) >= base_length - base_length_tol && length(results$ID) <= base_length + base_length_tol,
        info = paste0("Beta=TRUE: Expected 123-185 individuals, got ", length(results$ID))
      )
    }
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
  beta_options <- c(FALSE, TRUE)
  strict_tolerance <- 1e-8
  sex_tolerance <- .03
  # Optimized version needs wider tolerance for sex ratios on large pedigrees
  sex_tolerance_opt <- .07

  base_length <- 424
  base_length_tol <- 0.2 * base_length
  beta_match_base <- FALSE

  #  beta_options <- T
  for (beta in beta_options) {
    set.seed(seed)
    message("Beta option Starting: ", beta)
    results <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = beta)
    # Check that dimnames are correct
    # Base version: exact count. Optimized version: within 20% range
    if (isFALSE(beta) || (isTRUE(beta) && beta_match_base)) {
      expect_equal(length(results$ID), base_length, tolerance = strict_tolerance)
    } else {
      expect_true(length(results$ID) >= base_length - base_length_tol && length(results$ID) <= base_length + base_length_tol,
        info = paste0("Beta=TRUE: Expected 340-510 individuals, got ", length(results$ID))
      )
    }
    expect_equal(length(results), 7, tolerance = strict_tolerance)

    # check number of generations
    expect_equal(max(results$gen), Ngen, tolerance = strict_tolerance)

    # expect there to be parents in each for all generations except the first one
    filter_parents <- results %>%
      group_by(gen) %>%
      summarize(num_parents = sum(!is.na(dadID), na.rm = TRUE) + sum(!is.na(momID), na.rm = TRUE))

    expect_true(all(filter_parents$num_parents[filter_parents$gen > 1] > 0), info = paste0("Beta option: ", beta))
    expect_true(all(filter_parents$num_parents[filter_parents$gen == 1] == 0), info = paste0("Beta option: ", beta))


    # check number of sex ratio
    sex_mean_male <- mean(results$sex == "M")
    sex_mean_female <- mean(results$sex == "F")

    expect_lt(sex_mean_male, sex_mean_female)

    # Use wider tolerance for optimized version
    tol <- if (isFALSE(beta)) sex_tolerance else sex_tolerance_opt

    expect_equal(sex_mean_male, sexR, tolerance = tol, info = paste0("Beta option: ", beta))
    expect_equal(sex_mean_female, 1 - sexR, tolerance = tol, info = paste0("Beta option: ", beta))
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
  beta_options <- c(FALSE, TRUE)
  strict_tolerance <- 1e-8
  sex_tolerance <- .03
  sex_tolerance_opt <- .07
  # beta_options <- TRUE
  base_length <- 57
  base_length_tol <- 0.2 * base_length
  beta_match_base <- FALSE

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
    # Base version: exact count. Optimized version: within 20% range
    if (isFALSE(beta) || (isTRUE(beta) && beta_match_base)) {
      expect_equal(length(results$Id), base_length, tolerance = strict_tolerance)
    } else {
      expect_true(length(results$Id) >= base_length - base_length_tol && length(results$Id) <= base_length + base_length_tol,
        info = paste0("Beta=TRUE: Expected 45-70 individuals, got ", length(results$Id))
      )
    }
    expect_equal(length(results), 7, tolerance = strict_tolerance)

    # check number of generations
    expect_equal(max(results$gen), Ngen, tolerance = strict_tolerance)

    # check number of sex ratio

    # check number of sex ratio
    sex_mean_male <- mean(results$sex == code_male)
    sex_mean_female <- mean(results$sex == code_female)

    expect_lt(sex_mean_male, sex_mean_female)

    # expect there to be parents in each for all generations except the first one
    filter_parents <- results %>%
      group_by(gen) %>%
      summarize(num_parents = sum(!is.na(dadID), na.rm = TRUE) + sum(!is.na(momID), na.rm = TRUE))

    expect_true(all(filter_parents$num_parents[filter_parents$gen > 1] > 0), info = paste0("Beta option: ", beta))
    expect_true(all(filter_parents$num_parents[filter_parents$gen == 1] == 0), info = paste0("Beta option: ", beta))


    # Use wider tolerance for optimized version
    tol <- if (isFALSE(beta)) sex_tolerance else sex_tolerance_opt
    expect_equal(sex_mean_male, sexR, tolerance = tol, info = paste0("Beta option: ", beta))
    expect_equal(sex_mean_female, 1 - sexR, tolerance = tol, info = paste0("Beta option: ", beta))
    message("Beta option Ending: ", beta)
  }
})

test_that("simulatePedigree verbose prints updates", {
  seed <- 5
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  beta_options <- c(FALSE, TRUE)

  # beta_options <- TRUE
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

  # Test that "original" string alias works
  set.seed(seed)
  result_original <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = "original")

  # Results should be identical when using FALSE vs "original"
  expect_equal(nrow(result_false), nrow(result_original))
  expect_equal(ncol(result_false), ncol(result_original))
  expect_equal(result_false$ID, result_original$ID)

  # Test that invalid beta values throw errors
  expect_error(
    simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = "invalid"),
    "Invalid value for parameter"
  )

  # Test that "index" and "indexed" both throw appropriate error
  expect_error(
    simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = "index"),
    "not yet implemented"
  )

  expect_error(
    simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = "indexed"),
    "not yet implemented"
  )
})

test_that("simulatePedigrees returns combined data frame for multiple families", {
  set.seed(5)
  n_fam <- 3
  results <- simulatePedigrees(n_fam = n_fam, kpc = 3, Ngen = 4, marR = 0.6)

  # Should return a data frame
  expect_s3_class(results, "data.frame")

  # Should have exactly n_fam unique family IDs
  fam_ids <- unique(results$fam)
  expect_setequal(fam_ids, paste0("fam", seq_len(n_fam)))

  # All person IDs should be unique across families
  expect_equal(length(unique(results$ID)), nrow(results))

  # Should have standard pedigree columns
  expect_true(all(c("fam", "ID", "gen", "dadID", "momID", "spouseID", "sex") %in% colnames(results)))
})

test_that("simulatePedigrees with n_fam = 1 matches simulatePedigree structure", {
  set.seed(42)
  result_multi <- simulatePedigrees(n_fam = 1, kpc = 3, Ngen = 4, marR = 0.6)

  set.seed(42)
  result_single <- simulatePedigree(kpc = 3, Ngen = 4, marR = 0.6, fam_shift = 1L)

  # Both should have the same number of rows and columns
  expect_equal(nrow(result_multi), nrow(result_single))
  expect_equal(ncol(result_multi), ncol(result_single))
})

test_that("simulatePedigrees returns sequential IDs starting at 1", {
  set.seed(5)
  results <- simulatePedigrees(n_fam = 3, kpc = 3, Ngen = 4, marR = 0.6)

  # Person IDs should be exactly 1:nrow
  expect_equal(results$ID, seq_len(nrow(results)))

  # All parent/spouse references should be within the ID range (or NA)
  valid_ids <- seq_len(nrow(results))
  expect_true(all(is.na(results$momID) | results$momID %in% valid_ids))
  expect_true(all(is.na(results$dadID) | results$dadID %in% valid_ids))
  expect_true(all(is.na(results$spouseID) | results$spouseID %in% valid_ids))
})

test_that("simulatePedigrees works with beta = TRUE", {
  set.seed(5)
  n_fam <- 2
  results <- simulatePedigrees(n_fam = n_fam, kpc = 3, Ngen = 4, marR = 0.6, beta = TRUE)

  expect_s3_class(results, "data.frame")
  expect_equal(length(unique(results$fam)), n_fam)
  expect_equal(length(unique(results$ID)), nrow(results))
})

test_that("simulatePedigrees validates n_fam input", {
  expect_error(simulatePedigrees(n_fam = 0), "'n_fam' must be a positive integer")
  expect_error(simulatePedigrees(n_fam = -1), "'n_fam' must be a positive integer")
  expect_error(simulatePedigrees(n_fam = NA), "'n_fam' must be a positive integer")
})
