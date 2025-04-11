library(microbenchmark)
library(Matrix)
# library(BGmisc)
# data("hazard")
library(tidyverse)

# make big data
set.seed(15)
Ngen <- 5
kpc <- 5
sexR <- .50
marR <- .7
ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR) %>%
  mutate(
    fam = "fam 1"
  )
set.seed(151)
Ngen <- 5
marR <- .8
ped2 <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR) %>%
  mutate(
    fam = "fam 2",
    ID = ID + 10000,
    momID = momID + 10000,
    dadID = dadID + 10000,
    spID = spID + 10000
  )
set.seed(1151)
kpc <- 8
ped3 <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR) %>%
  mutate(
    fam = "fam 3",
    ID = ID + 20000,
    momID = momID + 20000,
    dadID = dadID + 20000,
    spID = spID + 20000
  )
ped <- rbind(ped, ped2)
ped <- rbind(ped, ped3)
if(FALSE){
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

summary(benchmark_results)

lm(benchmark_results$time ~ benchmark_results$expr) %>%
  summary()
# Print benchmark results
print(benchmark_results)

# Optional: Save results to CSV for later analysis
write.csv(summary(benchmark_results),
          "benchmark_results.csv",
          row.names = FALSE
)
# Print benchmark
}
verbose=FALSE
ad_ped_matrix <- ped2com(ped, component = "additive", adjacency_method = "direct", sparse = TRUE)
mit_ped_matrix <- ped2com(ped, component = "mitochondrial", adjacency_method = "direct", sparse = TRUE)
cn_ped_matrix <- ped2com(ped, component = "common nuclear", adjacency_method = "indexed", sparse = TRUE)
benchmark_results <- microbenchmark(
  beta = {
    com2links.beta(
  ad_ped_matrix = ad_ped_matrix,
  mit_ped_matrix = mit_ped_matrix,
  cn_ped_matrix = cn_ped_matrix,
  writetodisk = TRUE,
  verbose = verbose
); file.remove("dataRelatedPairs.csv")
  },  regular = {
    com2links(
      ad_ped_matrix = ad_ped_matrix,
      mit_ped_matrix = mit_ped_matrix,
      cn_ped_matrix = cn_ped_matrix,
      writetodisk = TRUE,
      verbose = verbose
    ); file.remove("dataRelatedPairs.csv")
  },   legacy = {
    com2links(
      ad_ped_matrix = ad_ped_matrix,
      mit_ped_matrix = mit_ped_matrix,
      cn_ped_matrix = cn_ped_matrix,
      verbose = verbose,
      legacy = TRUE
    ); file.remove("dataRelatedPairs.csv")
  },

  times = 100 # Run each method 100 times
)

summary(benchmark_results)

lm(benchmark_results$time ~ benchmark_results$expr) %>%
  summary()
# Print benchmark results
print(benchmark_results)

# Optional: Save results to CSV for later analysis
write.csv(summary(benchmark_results),
  "benchmark_results.csv",
  row.names = FALSE
)
