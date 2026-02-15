library(profvis)
library(microbenchmark)
library(tidyverse)
devtools::load_all(".")


# ---------------------------
# 0) Config
# ---------------------------
cfg <- list(
  seed = 1164127,
  Ngen_base = 3,
  reps = 10,
  all_scenarios = FALSE, # set to TRUE to run all scenarios defined below
  include_highgen = TRUE
)
cfg$gen_twin <- ceiling(cfg$Ngen_base - 1)

set.seed(cfg$seed)

# ---------------------------
# 1) Levels (edit here to extend)
# ---------------------------
levels <- list(
  ped = tibble(
    ped_label  = c("1gen", "lowgen", "midgen", if (cfg$include_highgen) "highgen"),
    Ngen_total = c(1, cfg$Ngen_base, cfg$Ngen_base * 2, if (cfg$include_highgen) cfg$Ngen_base * 3),
    gen_twin   = c(1, cfg$gen_twin, cfg$gen_twin, if (cfg$include_highgen) cfg$gen_twin)
    # Add highgen row whenever you want
  ),

  # Simulation-side factors (simulatePedigree)
  kpc = 3, # set to c(2, 3, 4) to vary
  sexR = 0.50, # sometimes fails above .5
  marR = c(0.8), # set to c(0.6, 0.8, 0.9) to vary
  sim_beta = TRUE, # set to c(TRUE, FALSE) if you ever want to vary

  # Conversion-side factors (ped2com)
  component = c("additive"),
  twin_method = c("NULL", "addtwins", "merging"),
  beta = c(FALSE, TRUE),
  sparse_matrix = c(FALSE, TRUE) # user-facing name, translated to ped2com's `sparse`
)

# Which columns define a unique simulation vs conversion condition
# If you add a new factor, put its name in the right scope here.

scopes <- list(
  sim  = c("ped_label", "Ngen_total", "gen_twin", "kpc", "sexR", "marR", "sim_beta"),
  conv = c("component", "twin_method", "beta", "sparse_matrix")
)

# ---------------------------
# 2) Scenarios (edit here to control crossing)
# ---------------------------
# Each scenario says what to vary and what to fix. No other code changes.
scenarios <- list(
  full = list(
    vary  = c("ped", "marR", "twin_method", "beta", "sparse_matrix"),
    fixed = list()
  ),
  quick = list(
    vary  = c("ped", "twin_method"),
    fixed = list(marR = 0.8, beta = FALSE, sparse_matrix = TRUE, component = "additive")
  )

  # Add more scenarios whenever you want:
  # e.g., "marR_only" = list(vary=c("marR"), fixed=list(ped = levels$ped[levels$ped$ped_label=="midgen",], ...))
)

if (!cfg$all_scenarios) {
  scenarios <- scenarios[c("full")] # order control; also, comment out any you don't want to run
}
# ---------------------------
# 3) Generic design builder (do not edit to add factors)
# ---------------------------
`%||%` <- function(x, y) if (!is.null(x)) x else y

level_tbl <- function(name, x) {
  if (inherits(x, "data.frame")) {
    return(x)
  }
  tibble(!!name := x)
}

default_value <- function(x) {
  if (inherits(x, "data.frame")) {
    return(x[1, , drop = FALSE])
  }
  x[[1]]
}

expand_scenario <- function(levels, vary, fixed = list(), scenario = "scenario") {
  df <- tibble(.dummy = 1)

  # expand varied factors
  for (nm in vary) {
    df <- tidyr::crossing(df, level_tbl(nm, levels[[nm]]))
  }

  # add fixed/default factors not varied
  not_varied <- setdiff(names(levels), vary)
  for (nm in not_varied) {
    lv <- levels[[nm]]

    if (inherits(lv, "data.frame")) {
      const <- fixed[[nm]] %||% default_value(lv)
      if (!inherits(const, "data.frame") || nrow(const) != 1) const <- default_value(lv)
      df <- bind_cols(df, const[rep(1, nrow(df)), , drop = FALSE])
    } else {
      df[[nm]] <- fixed[[nm]] %||% default_value(lv)
    }
  }

  df %>%
    select(-.dummy) %>%
    mutate(scenario = scenario, .before = 1)
}

build_design <- function(levels, scenarios) {
  purrr::imap_dfr(
    scenarios,
    ~ expand_scenario(levels, vary = .x$vary, fixed = .x$fixed, scenario = .y)
  )
}

design <- build_design(levels, scenarios)

# Create a stable, joinable label (also becomes the microbenchmark expr name)
design <- design %>%
  mutate(
    label = paste0(
      scenario,
      "|ped=", ped_label,
      "|marR=", marR,
      "|twin=", twin_method,
      "|beta=", beta,
      "|sparse=", sparse_matrix,
      "|comp=", component
    )
  )

# ---------------------------
# 4) Simulation cache (simulate once per unique sim condition)
# ---------------------------
simulate_one <- function(Ngen_total, gen_twin, kpc, sexR, marR, sim_beta) {
  simulatePedigree(
    kpc = kpc, Ngen = Ngen_total, sexR = sexR, marR = marR,
    beta = sim_beta
  ) %>%
    makeTwins(gen_twin = gen_twin)
}

sim_tbl <- design %>%
  distinct(across(all_of(scopes$sim))) %>%
  mutate(
    sim_id = row_number(),
    ped = pmap(
      list(Ngen_total, gen_twin, kpc, sexR, marR, sim_beta, sim_id),
      function(Ngen_total, gen_twin, kpc, sexR, marR, sim_beta, sim_id) {
        set.seed(cfg$seed + sim_id)
        simulate_one(Ngen_total, gen_twin, kpc, sexR, marR, sim_beta)
      }
    )
  ) %>%
  select(-sim_id)

design <- design %>% left_join(sim_tbl, by = scopes$sim)

# Put peds and args in keyed lists so benchmark expressions stay tiny
peds_by_label <- setNames(design$ped, design$label)

# ---------------------------
# 5) Conversion arg translation (edit only if you add non-1:1 args)
# ---------------------------
# Columns that map directly to ped2com arg names go through automatically.
# Anything else gets a translation rule here.
special_to_args <- list(
  twin_method = function(v) {
    if (is.null(v) || length(v) == 0 || is.na(v) || v == "NULL") {
      list(mz_twins = FALSE)
    } else {
      list(mz_twins = TRUE, mz_method = v)
    }
  },
  beta = function(v) list(beta = TRUE),
  sparse_matrix = function(v) list(sparse = v)
)

make_conv_args <- function(row, conv_cols) {
  # row is already a named list of scalar values when called correctly
  direct_cols <- setdiff(conv_cols, names(special_to_args))
  args <- row[direct_cols]

  for (nm in intersect(names(special_to_args), conv_cols)) {
    args <- c(args, special_to_args[[nm]](row[[nm]]))
  }

  args
}

make_conv_args_row <- function(...) {
  row <- list(...)
  make_conv_args(row, scopes$conv)
}

# Correct per-row args creation
args_by_label <- design %>%
  mutate(
    conv_args = pmap(select(., all_of(scopes$conv)), make_conv_args_row)
  ) %>%
  select(label, conv_args) %>%
  deframe()

# ---------------------------
# 6) One microbenchmark call with all expressions (correct behavior)
# ---------------------------
bench_exprs <- lapply(names(peds_by_label), function(lbl) {
  bquote(
    do.call(ped2com, c(list(ped = peds_by_label[[.(lbl)]]), args_by_label[[.(lbl)]]))
  )
})
names(bench_exprs) <- names(peds_by_label)

write_csv(design, "ped2com_benchmark_design.csv")
# write start time
write.table(Sys.time(), "ped2com_benchmark_start_time.txt", row.names = FALSE, col.names = FALSE)


benchmark_results <- do.call(
  microbenchmark::microbenchmark,
  c(bench_exprs, list(times = cfg$reps))
)

write.table(Sys.time(), "ped2com_benchmark_end_time.txt", row.names = FALSE, col.names = FALSE)

results <- as_tibble(benchmark_results) %>%
  mutate(label = as.character(expr)) %>%
  left_join(
    design %>% select(-ped),
    by = "label"
  )

# ---------------------------
# 7) Analysis/plot
# ---------------------------
results <- results %>%
  mutate(
    twin_method = factor(twin_method, levels = c("NULL", "addtwins", "merging")),
    ped_label = factor(ped_label, levels = levels$ped$ped_label),
    gen_factor = factor(ped_label, levels = levels$ped$ped_label, labels = paste0(levels$ped$Ngen_total, " gen"))
  )
write_csv(results, "ped2com_benchmark_results.csv")

summary(results)

if (cfg$reps > 8) {
  notch <- TRUE
} else {
  notch <- FALSE
}

results %>%
  mutate(
    beta_sparse = paste0("beta=", beta, ", sparse=", sparse_matrix),
    beta_sparse = factor(beta_sparse, levels = c(
      "beta=FALSE, sparse=FALSE",
      "beta=FALSE, sparse=TRUE",
      "beta=TRUE, sparse=FALSE",
      "beta=TRUE, sparse=TRUE"
    ))
  ) %>%
  ggplot(
    aes(
      x = ped_label,
      y = time / 1e6,
      color = twin_method,
      fill = beta_sparse
    )
  ) +
  geom_boxplot(notch = notch, position = position_dodge(width = 0.8)) +
  scale_y_log10() +
  facet_grid(~scenario) +
  labs(
    title = "Benchmarking ped2com() by twin handling and beta option",
    x = "Pedigree", y = "Execution time (ms)", color = "Twin method", fill = "Beta"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c(
    "beta=FALSE, sparse=FALSE" = "lightgray",
    "beta=FALSE, sparse=TRUE" = "gray8",
    "beta=TRUE, sparse=FALSE" = "lightcoral",
    "beta=TRUE, sparse=TRUE" = "red2"
  )) +
  scale_color_manual(values = c("NULL" = "gray", "addtwins" = "skyblue3", "merging" = "tomato2"))
