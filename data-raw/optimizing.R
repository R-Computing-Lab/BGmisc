library(profvis)
library(microbenchmark)
library(tidyverse)
set.seed(116427)
Ngen <- 3
kpc <- 6
sexR <- .50 # sometimes fails above .5
marR <- .9
reps <- 15
if (FALSE) {
  profvis({
    simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = beta_F)
  })

  profvis({
    simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = beta_T)
  })
}
# mz_method_opts <- c("addtwins", "merging")
beta_method_opts <- c(TRUE, FALSE)
beta_F <- T
beta_T <- T
gen_twin <- Ngen - 1


df_gen1 <- simulatePedigree(
  kpc = kpc, Ngen = 1, sexR = sexR, marR = marR,
  beta = TRUE
) %>%
  makeTwins(gen_twin = 1)

df_lowgen <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = TRUE) %>%
  makeTwins(gen_twin = gen_twin)

df_midgen <- simulatePedigree(kpc = kpc, Ngen = Ngen * 2, sexR = sexR, marR = marR, beta = TRUE) %>%
  makeTwins(gen_twin = gen_twin)

df_highgen <- simulatePedigree(kpc = kpc, Ngen = Ngen * 2 + 1, sexR = sexR, marR = marR, beta = TRUE) %>%
  makeTwins(gen_twin = gen_twin)

r_mz1 <- df_midgen %>%
  ped2add(mz_method = "merging", mz_twins = TRUE)
r_mz2 <- df_midgen %>%
  ped2add(mz_method = "addtwins", mz_twins = TRUE)
# expect_equal(length(r_mz1@i), length(r_mz2@i))
# expect_equal(length(r_mz1@x), length(r_mz2@x))
# expect_equal(length(r_mz1@p), length(r_mz2@p))

benchmark_results <- microbenchmark(
  beta_null_1gen = {
    df_gen1 %>%
      ped2add(mz_twins = F)
  },
  beta_false_1gen = {
    df_gen1 %>%
      ped2add(mz_method = "addtwins", mz_twins = TRUE)
  },
  beta_true_1gen = {
    df_gen1 %>%
      ped2add(mz_method = "merging", mz_twins = TRUE)
  },
  beta_null_lowgen = {
    df_lowgen %>%
      ped2add(mz_twins = F)
  },
  beta_false_lowgen = {
    df_lowgen %>%
      ped2add(mz_method = "addtwins", mz_twins = TRUE)
  },
  beta_true_lowgen = {
    df_lowgen %>%
      ped2add(mz_method = "merging", mz_twins = TRUE)
  },
  beta_null_midgen = {
    df_midgen %>%
      ped2add(mz_twins = F)
  },
  beta_false_midgen = {
    df_midgen %>%
      ped2add(mz_method = "addtwins", mz_twins = TRUE)
  },
  beta_true_midgen = {
    df_midgen %>%
      ped2add(mz_method = "merging", mz_twins = TRUE)
  },
  beta_null_highgen = {
    df_highgen %>%
      ped2add(mz_twins = F)
  },
  beta_false_highgen = {
    df_highgen %>%
      ped2add(mz_method = "addtwins", mz_twins = TRUE)
  },
  beta_true_highgen = {
    df_highgen %>%
      ped2add(mz_method = "merging", mz_twins = TRUE)
  },
  times = reps # Run each method 10 times
)




benchmark_results <- benchmark_results %>%
  mutate(
    beta_factor = factor(case_when(
      grepl("beta_true", expr) ~ "TRUE",
      grepl("beta_false", expr) ~ "FALSE",
      grepl("beta_null", expr) ~ "NULL",
      grepl("beta_indexed", expr) ~ "indexed"
    )),
    beta = ifelse(grepl("beta_false", expr), FALSE, TRUE),
    gen_num = case_when(
      grepl("1gen", expr) ~ 1,
      grepl("lowgen", expr) ~ Ngen,
      grepl("midgen", expr) ~ Ngen * 2,
      grepl("highgen", expr) ~ Ngen * 2 + 1
    ),
    gen_factor = factor(gen_num, levels = c(1, Ngen, Ngen * 2, Ngen * 2 + 1))
  )

summary(benchmark_results)
lm(benchmark_results$time ~ benchmark_results$beta_factor * benchmark_results$gen_num) %>%
  summary()

lm(benchmark_results$time ~ benchmark_results$beta_factor) %>%
  summary()
# log transform time for better visualization

ggplot(benchmark_results, aes(x = gen_factor, y = time / 1e6, color = beta_factor)) +
  geom_boxplot() +
  labs(
    title = "Benchmarking simulatePedigree() with and without beta parameter",
    x = "Generation Size",
    y = "Execution Time (ms)",
    color = "Beta Parameter"
  ) +
  theme_minimal() +
  scale_y_log10()



library(profvis)
library(microbenchmark)
library(tidyverse)
set.seed(1667)
Ngen <- 3
kpc <- 4
sexR <- .50 # sometimes fails above .5
marR <- .7
reps <- 10
if (FALSE) {
  profvis({
    simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = FALSE)
  })

  profvis({
    simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = TRUE)
  })
}
if (FALSE) {
  benchmark_results <- microbenchmark(
    beta_false_1gen = {
      simulatePedigree(kpc = kpc, Ngen = 1, sexR = sexR, marR = marR, beta = FALSE)
    },
    beta_true_1gen = {
      simulatePedigree(kpc = kpc, Ngen = 1, sexR = sexR, marR = marR, beta = TRUE)
    },
    beta_false_lowgen = {
      simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = FALSE)
    },
    beta_true_lowgen = {
      simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = TRUE)
    },
    beta_false_midgen = {
      simulatePedigree(kpc = kpc, Ngen = Ngen * 2, sexR = sexR, marR = marR, beta = FALSE)
    },
    beta_true_midgen = {
      simulatePedigree(kpc = kpc, Ngen = Ngen * 2, sexR = sexR, marR = marR, beta = TRUE)
    },
    beta_false_highgen = {
      simulatePedigree(kpc = kpc, Ngen = Ngen * 3, sexR = sexR, marR = marR, beta = FALSE)
    },
    beta_true_highgen = {
      simulatePedigree(kpc = kpc, Ngen = Ngen * 3, sexR = sexR, marR = marR, beta = TRUE)
    },
    times = reps # Run each method 10 times
  )

  benchmark_results <- benchmark_results %>%
    mutate(
      beta_factor = factor(case_when(
        grepl("beta_true", expr) ~ "TRUE",
        grepl("beta_false", expr) ~ "FALSE",
        grepl("beta_indexed", expr) ~ "indexed"
      )),
      beta = ifelse(grepl("beta_false", expr), FALSE, TRUE),
      gen_num = case_when(
        grepl("1gen", expr) ~ 1,
        grepl("lowgen", expr) ~ Ngen,
        grepl("midgen", expr) ~ Ngen * 2,
        grepl("highgen", expr) ~ Ngen * 3
      ),
      gen_factor = factor(gen_num, levels = c(1, Ngen, Ngen * 2, Ngen * 3))
    )

  summary(benchmark_results)
  lm(benchmark_results$time ~ benchmark_results$beta * benchmark_results$gen_num) %>%
    summary()

  lm(benchmark_results$time ~ benchmark_results$beta) %>%
    summary()
  # log transform time for better visualization

  ggplot(benchmark_results, aes(x = gen_factor, y = time / 1e6, color = beta_factor)) +
    geom_boxplot() +
    labs(
      title = "Benchmarking simulatePedigree() with and without beta parameter",
      x = "Generation Size",
      y = "Execution Time (ms)",
      color = "Beta Parameter"
    ) +
    theme_minimal() +
    scale_y_log10()
}
