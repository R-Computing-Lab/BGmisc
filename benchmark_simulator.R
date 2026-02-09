#!/usr/bin/env Rscript
# Benchmark script for pedigree simulator optimizations

library(BGmisc)

set.seed(42)

# Test parameters
test_configs <- list(
  small = list(kpc = 3, Ngen = 4, sexR = 0.5, marR = 0.7),
  medium = list(kpc = 4, Ngen = 5, sexR = 0.5, marR = 0.7),
  large = list(kpc = 5, Ngen = 6, sexR = 0.5, marR = 0.7)
)

cat("=== Pedigree Simulator Performance Benchmark ===\n\n")

for (config_name in names(test_configs)) {
  config <- test_configs[[config_name]]
  cat(sprintf("Testing %s configuration (kpc=%d, Ngen=%d)...\n",
              config_name, config$kpc, config$Ngen))

  # Test base version
  set.seed(42)
  time_base <- system.time({
    ped_base <- simulatePedigree(
      kpc = config$kpc,
      Ngen = config$Ngen,
      sexR = config$sexR,
      marR = config$marR,
      beta = FALSE
    )
  })

  # Test optimized version
  set.seed(42)
  time_opt <- system.time({
    ped_opt <- simulatePedigree(
      kpc = config$kpc,
      Ngen = config$Ngen,
      sexR = config$sexR,
      marR = config$marR,
      beta = TRUE
    )
  })

  # Calculate speedup
  speedup <- time_base["elapsed"] / time_opt["elapsed"]

  cat(sprintf("  Base version:      %.3f seconds\n", time_base["elapsed"]))
  cat(sprintf("  Optimized version: %.3f seconds\n", time_opt["elapsed"]))
  cat(sprintf("  Speedup:           %.2fx\n", speedup))
  cat(sprintf("  Rows generated:    %d (base) vs %d (optimized)\n\n",
              nrow(ped_base), nrow(ped_opt)))
}

cat("=== Testing Correctness ===\n")
# Verify both versions produce valid pedigrees with same seed
set.seed(123)
ped1 <- simulatePedigree(kpc = 3, Ngen = 4, beta = FALSE)
set.seed(123)
ped2 <- simulatePedigree(kpc = 3, Ngen = 4, beta = TRUE)

cat(sprintf("Base version: %d individuals\n", nrow(ped1)))
cat(sprintf("Optimized version: %d individuals\n", nrow(ped2)))
cat(sprintf("Column names match: %s\n",
            identical(colnames(ped1), colnames(ped2))))
cat(sprintf("Structure identical: %s\n",
            identical(class(ped1), class(ped2))))

cat("\nBenchmark complete!\n")
