library(microbenchmark)
library(Matrix)
# library(BGmisc)
# data("hazard")
library(tidyverse)



# Run benchmarking for "loop" and "indexed" methods in ped2com()
benchmark_results <- microbenchmark(
  reg = {
    readGedcom("data-raw/royal92.ged")
  },
  alpha = {
    readGedcom.alpha("data-raw/royal92.ged")
  },
  times = 5 # Run each method 100 times
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
