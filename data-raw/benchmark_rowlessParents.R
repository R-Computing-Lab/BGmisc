library(microbenchmark)
library(Matrix)
library(BGmisc)
library(tidyverse)

# Build a pedigree, then strip out some founder rows so their IDs become
# "rowless parents" -- referenced in momID/dadID but with no row of their
# own -- to benchmark rowless_parents_method = "rows" vs "schur" against
# each other, the same way benchmark.R compares adjacency_method values.
makeRowlessPed <- function(kpc, Ngen, sexR = .5, marR = .7, seed = 1, drop_frac = 0.2, full = FALSE) {
  set.seed(seed)
  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)

  if (full) {
    return(ped)
  }
  founder_ids <- ped$ID[is.na(ped$momID) & is.na(ped$dadID)]
  n_drop <- floor(drop_frac * length(founder_ids))
  drop_ids <- sample(founder_ids, n_drop)

  ped[!ped$ID %in% drop_ids, ]
}
ped_small_complete <- makeRowlessPed(kpc = 3, Ngen = 5, seed = 15, full = TRUE)
ped_big_complete <- makeRowlessPed(kpc = 9, Ngen = 5, seed = 1151, full = TRUE)
ped_small <- makeRowlessPed(kpc = 3, Ngen = 5, seed = 15)
ped_big <- makeRowlessPed(kpc = 9, Ngen = 5, seed = 1151)


cat("small: n =", nrow(ped_small), ", rowless parents =", length(.findRowlessParents(standardizeColnames(ped_small))), "\n")
cat("big:   n =", nrow(ped_big), ", rowless parents =", length(.findRowlessParents(standardizeColnames(ped_big))), "\n")


# check if methods return the same result
check_small <- ped2com(
  ped = ped_small, component = "additive",
  repair_rowless_parents = TRUE, rowless_parents_method = "rows",
  saveable = FALSE, resume = FALSE, verbose = FALSE, sparse = FALSE
) %>%
  all.equal(
    ped2com(
      ped = ped_small, component = "additive",
      repair_rowless_parents = TRUE, rowless_parents_method = "schur",
      saveable = FALSE, resume = FALSE, verbose = FALSE, sparse = FALSE
    )
  )

if (!check_small) {
  stop("ped2com() results differ for rowless_parents_method = 'rows' vs 'schur' on small pedigree")
}
# check if methods return the same result
check_big <- ped2com(
  ped = ped_big, component = "additive",
  repair_rowless_parents = TRUE, rowless_parents_method = "rows",
  saveable = FALSE, resume = FALSE, verbose = FALSE, sparse = FALSE
) %>%
  all.equal(
    ped2com(
      ped = ped_big, component = "additive",
      repair_rowless_parents = TRUE, rowless_parents_method = "schur",
      saveable = FALSE, resume = FALSE, verbose = FALSE, sparse = FALSE
    )
  )

if (!check_big) {
  stop("ped2com() results differ for rowless_parents_method = 'rows' vs 'schur' on big pedigree")
}

component <- "additive"
verbose <- FALSE
saveable <- FALSE
resume <- FALSE

benchmark_results <- microbenchmark(
  rows_small = {
    ped2com(
      ped = ped_small, component = component,
      repair_rowless_parents = TRUE, rowless_parents_method = "rows",
      saveable = saveable, resume = resume, verbose = verbose, sparse = FALSE
    )
  },
  base_small = {
    ped2com(
      ped = ped_small_complete, component = component,
      repair_rowless_parents = F,
      saveable = saveable, resume = resume, verbose = verbose, sparse = FALSE
    )
  },
  schur_small = {
    ped2com(
      ped = ped_small, component = component,
      repair_rowless_parents = TRUE, rowless_parents_method = "schur",
      saveable = saveable, resume = resume, verbose = verbose, sparse = FALSE
    )
  },
  rows_big = {
    ped2com(
      ped = ped_big, component = component,
      repair_rowless_parents = TRUE, rowless_parents_method = "rows",
      saveable = saveable, resume = resume, verbose = verbose, sparse = FALSE
    )
  },
  base_big = {
    ped2com(
      ped = ped_big_complete, component = component,
      repair_rowless_parents = F,
      saveable = saveable, resume = resume, verbose = verbose, sparse = FALSE
    )
  },
  schur_big = {
    ped2com(
      ped = ped_big, component = component,
      repair_rowless_parents = TRUE, rowless_parents_method = "schur",
      saveable = saveable, resume = resume, verbose = verbose, sparse = FALSE
    )
  },
  times = 50
)

summary(benchmark_results)

df_plot <- benchmark_results %>% mutate(
  size = case_when(
    expr %in% c(
      "rows_small", "schur_small",
      "base_small"
    ) ~ "small",
    expr %in% c(
      "rows_big", "schur_big",
      "base_big"
    ) ~ "big"
  ),
  method = case_when(
       expr %in% c("base_small", "base_big") ~ "base",
    expr %in% c("rows_small", "rows_big") ~ "rows",
    expr %in% c("schur_small", "schur_big") ~ "schur"
  ) # make base the reference level for the linear model, so that the intercept is the mean of the base method
)

df_plot$method <- factor(df_plot$method, levels = c("base", "rows", "schur"))
df_plot$size <- factor(df_plot$size, levels = c("small", "big"))

lm(time ~ method * size, data = df_plot) %>%
  summary() %>%
  print()

p <- ggplot(df_plot, aes(x = method, y = time)) +
  geom_boxplot(aes(fill = size), alpha = 0.5) +
  labs(
    title = "Rowless-Parent Correction: rows vs schur",
    x = "Method",
    y = "Time (nanoseconds)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p
print(benchmark_results)

write.csv(summary(benchmark_results),
  "benchmark_rowlessParents_results.csv",
  row.names = FALSE
)
