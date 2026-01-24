library(profvis)
library(microbenchmark)
library(tidyverse)
set.seed(16)
Ngen <- 3
kpc <- 3
sexR <- .50
marR <- .7
reps <- 20
if (FALSE) {
  profvis({
    simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = FALSE)
  })

  profvis({
    simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = TRUE)
  })
}


benchmark_results <- microbenchmark(
  beta_false_1gen = {
    simulatePedigree(kpc = kpc, Ngen = 1, sexR = sexR, marR = marR, beta = FALSE)
  },
  beta_true_1gen = {
    simulatePedigree(kpc = kpc, Ngen = 1, sexR = sexR, marR = marR, beta = TRUE)
  },

  beta_indexed_1gen = {
    simulatePedigree(kpc = kpc, Ngen = 1, sexR = sexR, marR = marR, beta = "indexed")
  },

  beta_false_lowgen = {
    simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = FALSE)
  },
  beta_true_lowgen = {
    simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = TRUE)
  },
  beta_indexed_lowgen = {
    simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = "indexed")
  },

  beta_false_midgen = {
    simulatePedigree(kpc = kpc, Ngen = Ngen * 2, sexR = sexR, marR = marR, beta = FALSE)
  },
  beta_true_midgen = {
    simulatePedigree(kpc = kpc, Ngen = Ngen * 2, sexR = sexR, marR = marR, beta = TRUE)
  },
  beta_indexed_midgen = {
    simulatePedigree(kpc = kpc, Ngen = Ngen * 2, sexR = sexR, marR = marR, beta = "indexed")
  },

  beta_false_highgen = {
    simulatePedigree(kpc = kpc, Ngen = Ngen * 3, sexR = sexR, marR = marR, beta = FALSE)
  },
  beta_true_highgen = {
    simulatePedigree(kpc = kpc, Ngen = Ngen * 3, sexR = sexR, marR = marR, beta = TRUE)
  },
  beta_indexed_highgen = {
    simulatePedigree(kpc = kpc, Ngen = Ngen * 3, sexR = sexR, marR = marR, beta = "indexed")
  },
  times = reps # Run each method 10 times
)

benchmark_results <- benchmark_results %>%
  mutate(
    beta_factor = factor(case_when(grepl("beta_true", expr) ~ "TRUE",
      grepl("beta_false", expr) ~ "FALSE",
      grepl("beta_indexed", expr) ~ "indexed")),
    beta = ifelse(grepl("beta_false", expr), FALSE, TRUE),
    gen_num = case_when(
      grepl("1gen", expr) ~ 1,
      grepl("lowgen", expr) ~ Ngen,
      grepl("midgen", expr) ~ Ngen * 2,
      grepl("highgen", expr) ~ Ngen * 3
    ),
    gen_factor = factor(gen_num, levels = c(1, Ngen, Ngen * 2, Ngen * 3)
    )
  )

summary(benchmark_results)
lm(benchmark_results$time ~ benchmark_results$beta * benchmark_results$gen_num) %>%
  summary()


# log transform time for better visualization

ggplot(benchmark_results, aes(x = gen, y = time / 1e6, color = beta)) +
  geom_boxplot() +
  labs(
    title = "Benchmarking simulatePedigree() with and without beta parameter",
    x = "Generation Size",
    y = "Execution Time (ms)",
    color = "Beta Parameter"
  ) +
  theme_minimal() +
  scale_y_log10()
