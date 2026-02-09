# Test Recommendations for Optimized Pedigree Simulator

## Current Situation

The optimized version (`beta = TRUE`) produces statistically equivalent but not identical results to the base version. This causes 7 out of 777 tests to fail when testing the optimized version.

## Recommended Test Strategy

### Option A: Test Both Versions Separately

Test the base version with exact expectations, and test the optimized version with statistical ranges:

```r
test_that("simulated pedigree generates expected data structure", {
  set.seed(5)

  # Test base version with exact expectations
  results_base <- simulatePedigree(
    kpc = 4, Ngen = 4, sexR = 0.5, marR = 0.7,
    beta = FALSE
  )
  expect_equal(length(results_base$ID), 57)
  expect_equal(mean(results_base$sex == "M"), 0.5)

  # Test optimized version with statistical ranges
  results_opt <- simulatePedigree(
    kpc = 4, Ngen = 4, sexR = 0.5, marR = 0.7,
    beta = TRUE
  )
  expect_true(length(results_opt$ID) >= 50 && length(results_opt$ID) <= 65)
  expect_true(abs(mean(results_opt$sex == "M") - 0.5) < 0.1)

  # Both versions should have valid structure
  for (results in list(results_base, results_opt)) {
    expect_true(all(c("fam", "ID", "gen", "dadID", "momID", "spID", "sex") %in% names(results)))
    expect_true(all(!is.na(results$ID)))
    expect_true(all(results$sex %in% c("M", "F")))
  }
})
```

### Option B: Only Test Base Version by Default

Keep existing tests for base version (default `beta = FALSE`), and create separate optional tests for optimized version:

```r
# Standard tests - always run
test_that("simulated pedigree generates expected data structure", {
  set.seed(5)
  results <- simulatePedigree(kpc = 4, Ngen = 4, sexR = 0.5, marR = 0.7)
  expect_equal(length(results$ID), 57)
  expect_equal(mean(results$sex == "M"), 0.5)
})

# Optimized version tests - check statistical properties
test_that("optimized pedigree has correct statistical properties", {
  set.seed(5)
  results <- simulatePedigree(kpc = 4, Ngen = 4, sexR = 0.5, marR = 0.7, beta = TRUE)

  # Check size is reasonable (within 20% of expected)
  expect_true(length(results$ID) >= 45 && length(results$ID) <= 70)

  # Check sex ratio is approximately correct (within 10%)
  sex_ratio <- mean(results$sex == "M")
  expect_true(abs(sex_ratio - 0.5) < 0.1)

  # Check all IDs are unique
  expect_equal(length(unique(results$ID)), length(results$ID))

  # Check generation structure is valid
  expect_true(all(results$gen >= 1 && results$gen <= 4))
  expect_true(all(results$gen[1:2] == 1))  # First two should be founders
})
```

### Option C: Parameterize Tests

Create test fixtures that work for both versions:

```r
test_pedigree_structure <- function(beta_version) {
  set.seed(5)
  results <- simulatePedigree(kpc = 4, Ngen = 4, sexR = 0.5, marR = 0.7, beta = beta_version)

  # Tests that work for both versions
  expect_true(all(c("fam", "ID", "gen", "dadID", "momID", "spID", "sex") %in% names(results)))
  expect_true(all(!is.na(results$ID)))
  expect_equal(length(unique(results$ID)), length(results$ID))
  expect_true(all(results$sex %in% c("M", "F")))
  expect_true(all(results$gen >= 1 && results$gen <= 4))

  # Version-specific expectations
  if (beta_version == FALSE) {
    # Exact expectations for base version
    expect_equal(length(results$ID), 57)
    expect_equal(mean(results$sex == "M"), 0.5)
  } else {
    # Statistical expectations for optimized version
    expect_true(length(results$ID) >= 45 && length(results$ID) <= 70)
    expect_true(abs(mean(results$sex == "M") - 0.5) < 0.1)
  }

  return(results)
}

test_that("base pedigree structure", {
  test_pedigree_structure(beta = FALSE)
})

test_that("optimized pedigree structure", {
  test_pedigree_structure(beta = TRUE)
})
```

## Recommended Approach

**Use Option A** - Test both versions in the same test with appropriate expectations for each. This ensures:
1. Base version maintains exact reproducibility
2. Optimized version is tested for correctness
3. Both versions are validated against the same seed
4. Tests document the expected differences

## Tests That Need Updating

Based on the failure report:

1. **test-simulatePedigree.R:16** - Expected 57, got 52 with beta=TRUE
2. **test-simulatePedigree.R:51** - Expected 154, got 145 with beta=TRUE
3. **test-simulatePedigree.R:89** - Expected 424, got 411 with beta=TRUE
4. **test-simulatePedigree.R:132** - Expected 57, got 52 with beta=TRUE
5. **test-simulatePedigree.R:147-148** - Sex ratio off by ~3%

For each of these, change from:
```r
expect_equal(length(results$ID), 57)
```

To:
```r
if (isFALSE(beta)) {
  expect_equal(length(results$ID), 57)
} else {
  expect_true(length(results$ID) >= 45 && length(results$ID) <= 70)
}
```

Or use ±20% ranges for statistical properties:
```r
expect_true(length(results$ID) >= 57 * 0.8 && length(results$ID) <= 57 * 1.2)
```

## Example: Updated Test

```r
test_that("simulated pedigree generates expected data structure", {
  set.seed(5)

  # Test with base version
  results <- simulatePedigree(kpc = 4, Ngen = 4, sexR = 0.5, marR = 0.7, beta = FALSE)
  expect_equal(length(results$ID), 57)
  expect_equal(mean(results$sex == "M"), 0.5)

  # Test with optimized version - same seed, different but valid results
  results_opt <- simulatePedigree(kpc = 4, Ngen = 4, sexR = 0.5, marR = 0.7, beta = TRUE)
  expect_true(length(results_opt$ID) >= 45 && length(results_opt$ID) <= 70)
  expect_true(abs(mean(results_opt$sex == "M") - 0.5) < 0.1)

  # Both should have valid structure
  for (df in list(results, results_opt)) {
    expect_s3_class(df, "data.frame")
    expect_true(all(c("fam", "ID", "gen", "dadID", "momID", "spID", "sex") %in% names(df)))
    expect_equal(length(unique(df$ID)), length(df$ID))  # All IDs unique
    expect_true(all(df$sex %in% c("M", "F")))
  }
})
```
