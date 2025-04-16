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
Ngen <- 10
ped3 <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR) %>%
  mutate(
    fam = "fam 3",
    ID = ID + 20000,
    momID = momID + 20000,
    dadID = dadID + 20000,
    spID = spID + 20000
  )
ped3 <- ped3 %>%
  mutate(
    fam = "fam 4",
    ID = ID + 10000,
    momID = momID + 10000,
    dadID = dadID + 10000,
    spID = spID + 10000
  ) %>% rbind(ped3)

set.seed(1151)
kpc <- 2
Ngen <- 10
ped4 <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR) %>%
  mutate(
    fam = "fam 5",
    ID = ID + 40000,
    momID = momID + 40000,
    dadID = dadID + 40000,
    spID = spID + 40000
  )


ped <- rbind(ped, ped2)
ped <- rbind(ped, ped3)
ped <- rbind(ped, ped4)

if(TRUE){
# Define parameters
component <- "common nuclear"#"additive" # Change this to test different components
saveable <- FALSE # Disable saving to avoid disk I/O slowing down benchmarking
resume <- FALSE # Disable resume to ensure full fresh runs
save_path <- "checkpoint/"
verbose <- FALSE # Turn off verbose for cleaner output
update_rate <- 100
save_rate_parlist <- 1000
#method_approach <- 1
# Run benchmarking for "loop" and "indexed" methods in ped2com()
benchmark_results <- microbenchmark(
#  loop_big = {
#    ped2com(
#      ped = ped,
#      component = component,
#      adjacency_method = "loop", # Test "loop" method
#      saveable = saveable,
#      resume = resume,
#      save_path = save_path,
#      verbose = verbose,
#      update_rate = update_rate,
#      save_rate_parlist = save_rate_parlist
#    )
#  },
  indexed_big = {
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
  direct4_big = {
    ped2com(
      ped = ped,
      component = component,
      adjacency_method = "direct", # Test "indexed" method
      saveable = saveable,
      resume = resume,
      save_path = save_path,
      verbose = verbose,
      update_rate = update_rate,
      method_approach = 4,
      save_rate_parlist = save_rate_parlist
    )
  },
  direct2_big = {
    ped2com(
      ped = ped,
      component = component,
      adjacency_method = "direct", # Test "indexed" method
      saveable = saveable,
      resume = resume,
      save_path = save_path,
      verbose = verbose,
      update_rate = update_rate,
      method_approach = 2,
      save_rate_parlist = save_rate_parlist
    )
  },
  direct5_big = {
    ped2com(
      ped = ped,
      component = component,
      adjacency_method = "direct", # Test "indexed" method
      saveable = saveable,
      resume = resume,
      save_path = save_path,
      verbose = verbose,
      update_rate = update_rate,
      method_approach = 5,
      save_rate_parlist = save_rate_parlist
    )
  },
#  loop = {
#    ped2com(
#      ped = ped2,
#      component = component,
#      adjacency_method = "loop", # Test "loop" method
#      saveable = saveable,
#      resume = resume,
#      save_path = save_path,
#      verbose = verbose,
#      update_rate = update_rate,
#      save_rate_parlist = save_rate_parlist
#
#    )
#  },
  indexed = {
    ped2com(
      ped = ped2,
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
  direct4 = {
    ped2com(
      ped = ped2,
      component = component,
      adjacency_method = "direct", # Test "indexed" method
      saveable = saveable,
      resume = resume,
      save_path = save_path,
      verbose = verbose,
      update_rate = update_rate,
      method_approach = 4,
      save_rate_parlist = save_rate_parlist
    )
  },
  direct2 = {
    ped2com(
      ped = ped2,
      component = component,
      adjacency_method = "direct", # Test "indexed" method
      saveable = saveable,
      resume = resume,
      save_path = save_path,
      verbose = verbose,
      update_rate = update_rate,
      method_approach = 2,
      save_rate_parlist = save_rate_parlist
    )
  },
  direct5 = {
    ped2com(
      ped = ped2,
      component = component,
    adjacency_method = "direct", # Test "indexed" method
     saveable = saveable,
      resume = resume,
      save_path = save_path,
      verbose = verbose,
      update_rate = update_rate,
      method_approach = 5,
      save_rate_parlist = save_rate_parlist
   )
  },
  times = 100 # Run each method 100 times
)


summary(benchmark_results)

df_plot <- benchmark_results %>% mutate(size = case_when(expr %in% c("loop", "indexed", "direct4", "direct2", "direct5") ~ "small",
                                             expr %in% c("loop_big", "indexed_big", "direct4_big", "direct2_big", "direct5_big") ~ "big"),
                             method = case_when(expr %in% c("loop", "loop_big") ~ "loop",
                                         expr %in% c("indexed", "indexed_big") ~ "indexed",
                                         expr %in% c("direct4", "direct4_big") ~ "direct4",
                                         expr %in% c("direct2", "direct2_big") ~ "direct2",
                                         expr %in% c("direct5", "direct5_big") ~ "direct5"))# %>%

# set indexed as reference level
df_plot$method <- factor(df_plot$method, levels = c("indexed", "loop","direct2", "direct4",  "direct5"))
df_plot$size <- factor(df_plot$size, levels = c("small", "big"))


lm(time ~ method*size,data=df_plot) %>%
  summary() %>% print()


p<-ggplot(df_plot, aes(x = method, y = time)) +
 geom_boxplot(aes(fill = size), alpha = 0.5) +
  labs(title = "Benchmarking Results",
       x = "Method",
       y = "Time (seconds)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Print benchmark results

p
print(benchmark_results)

# Optional: Save results to CSV for later analysis
write.csv(summary(benchmark_results),
          "benchmark_results.csv",
          row.names = FALSE
)
# Print benchmark
}

if(FALSE){
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
}
