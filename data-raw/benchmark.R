library(microbenchmark)
library(Matrix)
# library(BGmisc)
# data("hazard")


# make big data
set.seed(15)
Ngen <- 5
kpc <- 5
sexR <- .50
marR <- .7
ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)

# Define parameters
component <- "additive" # Change this to test different components
saveable <- FALSE # Disable saving to avoid disk I/O slowing down benchmarking
resume <- FALSE # Disable resume to ensure full fresh runs
save_path <- "checkpoint/"
verbose <- FALSE # Turn off verbose for cleaner output
update_rate <- 100
save_rate_parlist <- 1000

# Run benchmarking for "loop" and "indexed" methods in ped2com()
benchmark_results <- microbenchmark(
  loop = {
    ped2com(
      ped = ped,
      component = component,
      adjacency_method = "loop", # Test "loop" method
      saveable = saveable,
      resume = resume,
      save_path = save_path,
      verbose = verbose,
      update_rate = update_rate,
      save_rate_parlist = save_rate_parlist
    )
  },
  indexed = {
    ped2com(
      ped = ped,
      component = component,
      adjacency_method = "indexed", # Test "indexed" method
      saveable = saveable,
      resume = resume,
      save_path = save_path,
      verbose = verbose,
      update_rate = update_rate,
      save_rate_parlist = save_rate_parlist
    )
  },
  times = 100 # Run each method 100 times
)

# Print benchmark


# Print benchmark results
print(benchmark_results)

# Optional: Save results to CSV for later analysis
write.csv(summary(benchmark_results), "benchmark_results.csv", row.names = FALSE)
