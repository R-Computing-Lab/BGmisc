# temporal_BGmisc_AE_parameter_recovery.R
#
# Monte Carlo parameter-recovery study for a temporally moderated
# BGmisc/OpenMx pedigree model.
#
# This script extends the original smoke test by:
#   1. Simulating a fresh set of pedigrees in each replication.
#   2. Constructing BGmisc relatedness matrices.
#   3. Simulating phenotype data from a temporal A + E model.
#   4. Building OpenMx family-group models using a BGmisc-style architecture.
#   5. Fitting the true linear-time + historical-moderator AE model.
#   6. Saving convergence diagnostics and parameter estimates.
#   7. Summarizing bias, RMSE, Monte Carlo standard errors, and empirical
#      Monte Carlo 95% intervals.
#
# The defaults reproduce the proposed pilot design of 1,410 pedigrees and
# 500 Monte Carlo replications. For a quick test, set n_replications and
# n_families to smaller values before running the full study.

# -----------------------------------------------------------------------------
# Package setup
# -----------------------------------------------------------------------------

required_packages <- c("BGmisc", "OpenMx", "mvtnorm", "dplyr", "tidyr", "purrr", "readr", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall them before running this script."
  )
}

library(BGmisc)
library(OpenMx)
library(mvtnorm)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)

source(file.path("data-raw", "smoketest_helpers.R"))

# -----------------------------------------------------------------------------
# Monte Carlo settings
# -----------------------------------------------------------------------------

master_seed <- 202601
n_replications <- 3
n_families <- 150
threshold_year <- 1776
optimizer_tries <- 30
save_rate <- 10 # every 10 reps

# Save a checkpoint after every completed replication. This is slower than
# saving only at the end, but protects a long simulation from data loss.
save_checkpoints <- TRUE
output_directory <- file.path("results", "temporal_AE_parameter_recovery")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

replication_file <- file.path(output_directory, "replication_results.csv")
recovery_file <- file.path(output_directory, "parameter_recovery_summary.csv")
convergence_file <- file.path(output_directory, "convergence_summary.csv")
plot_file <- file.path(output_directory, "parameter_recovery_plot.png")

sim_components <- c(
  "a",
  # "cn", "ce",
  # "mt",
  "e"
)

fit_components <- c(
  "a",
  # "cn", "ce",
  # "mt",
  "e"
)

# Data-generating parameters.
# These are linear-loading parameters used for simulation. The fitted model
# uses exp(loadings). The target transformation below follows the same mapping
# used in the original smoke-test script.
true_beta <- list(
  a  = c(5, 0.5, 0.00, 0.00),
  cn = c(0.00, 0.00, 0.00, 0.00),
  ce = c(0.00, 0.00, 0.00, 0.00),
  mt = c(0.00, 0.00, 0.00, 0.00),
  e  = c(5, -0.20, 0.00, 0.00)
)

true_gamma <- list(
  a  = +0.5,
  cn = 0.00,
  ce = 0.00,
  mt = 0.00,
  e  = -0.50
)

# The parameters estimated in the true fitted model.
target <- c(
  b_a_0 = if (true_beta$a[1] != 0) log(true_beta$a[1]) else 0,
  b_a_1 = if (true_beta$a[1] != 0) true_beta$a[2] / true_beta$a[1] else 0,
  g_a_1 = if (true_beta$a[1] != 0) true_gamma$a[1] / true_beta$a[1] else 0,
  b_e_0 = if (true_beta$e[1] != 0) log(true_beta$e[1]) else 0,
  b_e_1 = if (true_beta$e[1] != 0) true_beta$e[2] / true_beta$e[1] else 0,
  g_e_1 = if (true_beta$e[1] != 0) true_gamma$e[1] / true_beta$e[1] else 0
)

labels_to_free <- c(names(target), "mean_y")

# -----------------------------------------------------------------------------
# Helper functions for one Monte Carlo replication
# -----------------------------------------------------------------------------

simulate_one_dataset <- function(replication, replication_seed) {
  set.seed(replication_seed)

  families <- vector("list", n_families)

  for (i in seq_len(n_families)) {
    families[[i]] <- simulate_temporal_family(
      kpc = 4,
      Ngen = 5,
      marR = 0.8,
      threshold_year = threshold_year,
      true_beta = true_beta,
      true_gamma = true_gamma,
      components = sim_components,
      family_id = i
    )
  }

  family_sizes <- vapply(families, function(x) length(x$y), integer(1))

  list(
    replication = replication,
    seed = replication_seed,
    families = families,
    n_families = length(families),
    total_n = sum(family_sizes),
    mean_family_size = mean(family_sizes),
    min_family_size = min(family_sizes),
    max_family_size = max(family_sizes)
  )
}

build_true_model <- function(families, replication) {
  group_models <- vector("list", length(families))

  for (i in seq_along(families)) {
    fam <- families[[i]]

    group_models[[i]] <- buildOneTemporalFamilyGroup(
      group_name = paste0("rep", replication, "_family", i),
      Addmat = fam$A,
      Nucmat = NULL,
      Extmat = NULL,
      Mtdmat = NULL,
      Dmgmat = NULL,
      full_df_row = fam$y,
      obs_ids = fam$obs_ids,
      birth_year = fam$birth_year_scaled,
      H = fam$H,
      use_exp_loadings = TRUE
    )
  }

  temporal_model_ae <- buildTemporalPedigreeMx(
    model_name = paste0("TemporalPedigreeRecovery_AE_rep", replication),
    group_models = group_models,
    p_hist = 1,
    components = fit_components,
    ci = FALSE
  )

  # Fit the true model: AE with linear birth-cohort moderation plus one
  # historical moderator.
  free_only(
    temporal_model_ae,
    labels_to_free = labels_to_free
  )
}

fit_one_dataset <- function(model) {
  fit <- run_and_report(
    model,
    "AE linear time + historical moderator",
    tries = optimizer_tries
  )

  status_code <- fit$output$status$code
  status_message <- fit$output$status$status
  estimates <- omxGetParameters(fit)[names(target)]

  list(
    fit = fit,
    status_code = status_code,
    status_message = status_message,
    converged = isTRUE(status_code %in% c(0L, 1L)) && all(is.finite(estimates)),
    minus2ll = as.numeric(fit$output$Minus2LogLikelihood),
    estimates = estimates
  )
}

failed_replication_row <- function(
    replication,
    replication_seed,
    elapsed_seconds,
    error_message,
    n_families_value = n_families,
    total_n = NA_real_,
    mean_family_size = NA_real_,
    min_family_size = NA_real_,
    max_family_size = NA_real_) {
  estimate_values <- stats::setNames(rep(NA_real_, length(target)), names(target))

  tibble::as_tibble_row(c(
    list(
      replication = replication,
      seed = replication_seed,
      converged = FALSE,
      status_code = NA_integer_,
      status_message = NA_character_,
      error_message = error_message,
      n_families = n_families_value,
      total_n = total_n,
      mean_family_size = mean_family_size,
      min_family_size = min_family_size,
      max_family_size = max_family_size,
      minus2ll = NA_real_,
      elapsed_seconds = elapsed_seconds
    ),
    as.list(estimate_values)
  ))
}

run_one_replication <- function(replication) {
  replication_seed <- master_seed + replication
  start_time <- proc.time()[["elapsed"]]

  simulated <- NULL

  result <- tryCatch(
    {
      simulated <- simulate_one_dataset(
        replication = replication,
        replication_seed = replication_seed
      )

      model <- build_true_model(
        families = simulated$families,
        replication = replication
      )

      fitted <- fit_one_dataset(model)
      elapsed_seconds <- proc.time()[["elapsed"]] - start_time

      estimate_values <- stats::setNames(
        as.numeric(fitted$estimates),
        names(target)
      )

      tibble::as_tibble_row(c(
        list(
          replication = replication,
          seed = replication_seed,
          converged = fitted$converged,
          status_code = fitted$status_code,
          status_message = fitted$status_message,
          error_message = NA_character_,
          n_families = simulated$n_families,
          total_n = simulated$total_n,
          mean_family_size = simulated$mean_family_size,
          min_family_size = simulated$min_family_size,
          max_family_size = simulated$max_family_size,
          minus2ll = fitted$minus2ll,
          elapsed_seconds = elapsed_seconds
        ),
        as.list(estimate_values)
      ))
    },
    error = function(e) {
      elapsed_seconds <- proc.time()[["elapsed"]] - start_time

      failed_replication_row(
        replication = replication,
        replication_seed = replication_seed,
        elapsed_seconds = elapsed_seconds,
        error_message = conditionMessage(e),
        n_families_value = if (is.null(simulated)) n_families else simulated$n_families,
        total_n = if (is.null(simulated)) NA_real_ else simulated$total_n,
        mean_family_size = if (is.null(simulated)) NA_real_ else simulated$mean_family_size,
        min_family_size = if (is.null(simulated)) NA_real_ else simulated$min_family_size,
        max_family_size = if (is.null(simulated)) NA_real_ else simulated$max_family_size
      )
    }
  )

  rm(simulated)
  invisible(gc())

  result
}

# -----------------------------------------------------------------------------
# Run the Monte Carlo study
# -----------------------------------------------------------------------------

cat(
  "Starting temporal AE parameter-recovery simulation\n",
  "Replications: ", n_replications, "\n",
  "Pedigrees per replication: ", n_families, "\n",
  "Master seed: ", master_seed, "\n\n",
  sep = ""
)

replication_results <- vector("list", n_replications)

for (replication in seq_len(n_replications)) {
  cat(
    sprintf(
      "Replication %d of %d, seed %d\n",
      replication,
      n_replications,
      master_seed + replication
    )
  )

  replication_results[[replication]] <- run_one_replication(replication)

    should_save_checkpoint <- save_checkpoints &&
    (replication %% save_rate == 0L || replication == n_replications)

  if (should_save_checkpoint) {
    checkpoint_results <- dplyr::bind_rows(replication_results[seq_len(replication)])
    readr::write_csv(checkpoint_results, replication_file, na = "")
  }
}

replication_results <- dplyr::bind_rows(replication_results)
readr::write_csv(replication_results, replication_file, na = "")

cat("\nMonte Carlo fitting completed.\n")

# -----------------------------------------------------------------------------
# Parameter-recovery summaries
# -----------------------------------------------------------------------------

parameter_targets <- tibble::enframe(
  target,
  name = "parameter",
  value = "true_value"
)

long_results <- replication_results %>%
  dplyr::select(replication, converged, dplyr::all_of(names(target))) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(names(target)),
    names_to = "parameter",
    values_to = "estimate"
  ) %>%
  dplyr::left_join(parameter_targets, by = "parameter") %>%
  dplyr::mutate(
    usable = converged & is.finite(estimate),
    error = estimate - true_value,
    squared_error = error^2
  )

recovery_summary <- long_results %>%
  dplyr::filter(usable) %>%
  dplyr::group_by(parameter, true_value) %>%
  dplyr::summarise(
    n_requested = n_replications,
    n_success = dplyr::n(),
    success_rate = n_success / n_requested,
    mean_estimate = mean(estimate),
    median_estimate = stats::median(estimate),
    empirical_sd = stats::sd(estimate),
    mcse_mean = empirical_sd / sqrt(n_success),
    bias = mean(error),
    relative_bias = dplyr::if_else(
      true_value == 0,
      NA_real_,
      bias / true_value
    ),
    rmse = sqrt(mean(squared_error)),
    mc_lower = as.numeric(stats::quantile(estimate, 0.025, names = FALSE)),
    mc_upper = as.numeric(stats::quantile(estimate, 0.975, names = FALSE)),
    empirical_interval_excludes_zero = mc_lower > 0 | mc_upper < 0,
    .groups = "drop"
  ) %>%
  dplyr::arrange(match(parameter, names(target)))

# Include parameters with zero successful fits so failures remain visible.
recovery_summary <- parameter_targets %>%
  dplyr::left_join(recovery_summary, by = c("parameter", "true_value")) %>%
  dplyr::mutate(
    n_requested = dplyr::coalesce(n_requested, n_replications),
    n_success = dplyr::coalesce(n_success, 0L),
    success_rate = dplyr::coalesce(success_rate, 0)
  )

convergence_summary <- replication_results %>%
  dplyr::summarise(
    n_requested = n_replications,
    n_converged = sum(converged, na.rm = TRUE),
    convergence_rate = mean(converged, na.rm = TRUE),
    n_with_errors = sum(!is.na(error_message)),
    mean_total_n = mean(total_n, na.rm = TRUE),
    sd_total_n = stats::sd(total_n, na.rm = TRUE),
    min_total_n = min(total_n, na.rm = TRUE),
    max_total_n = max(total_n, na.rm = TRUE),
    mean_family_size = mean(mean_family_size, na.rm = TRUE),
    mean_elapsed_seconds = mean(elapsed_seconds, na.rm = TRUE),
    total_elapsed_hours = sum(elapsed_seconds, na.rm = TRUE) / 3600
  )

readr::write_csv(recovery_summary, recovery_file, na = "")
readr::write_csv(convergence_summary, convergence_file, na = "")

print(convergence_summary)
print(recovery_summary)

# -----------------------------------------------------------------------------
# Parameter-recovery plot
# -----------------------------------------------------------------------------

recovery_plot <- ggplot2::ggplot(
  recovery_summary,
  ggplot2::aes(
    x = stats::reorder(parameter, true_value),
    y = mean_estimate
  )
) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = mc_lower, ymax = mc_upper),
    width = 0.15
  ) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_point(
    ggplot2::aes(y = true_value),
    shape = 4,
    size = 3,
    stroke = 1
  ) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Temporal AE Monte Carlo Parameter Recovery",
    subtitle = "Points are Monte Carlo means; crosses are generating values",
    x = "Parameter",
    y = "Estimate"
  )

ggplot2::ggsave(
  filename = plot_file,
  plot = recovery_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# -----------------------------------------------------------------------------
# Compact text summary for reporting
# -----------------------------------------------------------------------------

cat("\nCompact recovery report:\n")
cat(
  sprintf(
    paste0(
      "%d replications were requested; %d converged without non-finite ",
      "target estimates (%.1f%%). The mean realized sample size was %.1f ",
      "individuals across %d pedigrees, with a mean pedigree size of %.2f.\n"
    ),
    convergence_summary$n_requested,
    convergence_summary$n_converged,
    100 * convergence_summary$convergence_rate,
    convergence_summary$mean_total_n,
    n_families,
    convergence_summary$mean_family_size
  )
)

report_table <- recovery_summary %>%
  dplyr::transmute(
    parameter,
    true_value = round(true_value, 4),
    mean_estimate = round(mean_estimate, 4),
    bias = round(bias, 4),
    rmse = round(rmse, 4),
    mc_95_interval = paste0(
      "[",
      round(mc_lower, 4),
      ", ",
      round(mc_upper, 4),
      "]"
    )
  )

print(report_table)

cat(
  "\nFiles written to: ", normalizePath(output_directory, mustWork = FALSE), "\n",
  sep = ""
)
