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

master_seed <- 1202601
n_replications <- 500
n_families <- 150
threshold_year <- 1776
prop_historical <- 0.5
# Standard deviation of the birth-year range, widened here for broader time
# coverage. It is not linked to parental age, so at this width about 8% of
# parent-child pairs end up with the child born before the parent.
birth_year_sd <- 12
birth_year_base <- 1700
gen_gap <- 30

historical_threshold_centered <- (threshold_year - birth_year_base)

optimizer_tries <- 5
save_rate <- 10 # every 10 reps
kpc <- 4
Ngen <- 4
marR <- 0.8
use_exp_loadings <- TRUE
core_folder <- "temporal_ACE_parameter_recovery_500"
loading_link <- if (use_exp_loadings) {
  "exp"
} else {
  "identity"
}

# Save a checkpoint after every completed replication. This is slower than
# saving only at the end, but protects a long simulation from data loss.
save_checkpoints <- TRUE
output_directory <- file.path("results", core_folder)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

replication_file <- file.path(output_directory, "replication_results.csv")
recovery_file <- file.path(output_directory, "parameter_recovery_summary.csv")
convergence_file <- file.path(output_directory, "convergence_summary.csv")
plot_file <- file.path(output_directory, "parameter_recovery_plot.png")

replication_file_ace <- file.path(output_directory, "replication_results_ace.csv")
recovery_file_ace <- file.path(output_directory, "parameter_recovery_summary_ace.csv")
convergence_file_ace <- file.path(output_directory, "convergence_summary_ace.csv")
plot_file_ace <- file.path(output_directory, "parameter_recovery_plot_ace.png")


sim_components <- c(
  "a",
  "cn", # "ce",
  #  "mt",
  "e"
)

fit_components <- c(
  "a",
  "cn", # "ce",
  # "mt",
  "e"
)

# Data-generating parameters, expressed on the exponential-loading scale.
# Element j of true_beta[[k]] is the coefficient on time^(j-1), so
#   lambda_k = exp(beta_k0 + beta_k1 * t + beta_k2 * t^2 + beta_k3 * t^3 + gamma_k * H)
# and component k contributes lambda_k^2 to the phenotypic variance.
true_beta <- list(
  a  = c(log(2), 0.1, -0.1, 0.00),
  cn = c(log(1.5), 0.00, 0.00, 0.00),
  ce = c(0.00, 0.00, 0.00, 0.00),
  mt = c(0.00, 0.00, 0.00, 0.00),
  e  = c(log(2.0), 0.0, 0.00, 0.00)
)

true_gamma <- list(
  a  = 0.2,
  cn = 0.00,
  ce = 0.00,
  mt = 0.00,
  e  = 0.0
)


# The parameters estimated in the true fitted model. The simulator
# (simulate_temporal_family -> make_lambda, loading_link = "exp") and the fitted
# model (buildOneFamilyGroup -> L_k = exp(Tpoly %*% B_k + H %*% G_k)) share the
# same parameterization, so each generating coefficient is its own target and no
# transformation is applied. A log()/ratio mapping would only be appropriate if
# the simulator used linear (identity) loadings.
stopifnot(use_exp_loadings, loading_link == "exp")

target <- c(
  b_a_0 = true_beta$a[1],
  b_a_1 = true_beta$a[2],
  b_a_2 = true_beta$a[3],
  g_a_1 = true_gamma$a[1],
  b_cn_0 = true_beta$cn[1],
  b_cn_1 = true_beta$cn[2],
  b_cn_2 = true_beta$cn[3],
  g_cn_1 = true_gamma$cn[1],
  b_e_0 = true_beta$e[1],
  b_e_1 = true_beta$e[2],
  b_e_2 = true_beta$e[3],
  g_e_1 = true_gamma$e[1]
)

labels_to_free <- c(names(target), "mean_y")


# -----------------------------------------------------------------------------
# Helper functions for one Monte Carlo replication
# -----------------------------------------------------------------------------

# threshold_year, birth_year_sd, birth_year_base, gen_gap and loading_link are
# read from the Monte Carlo settings above, as kpc, Ngen and marR already are.
simulate_one_dataset <- function(
  replication, replication_seed, poly = 3,
  rescale = TRUE,
  # Map the designed birth-year span onto [-3, 3] with design constants instead
  # of a per-family z-score, so t genuinely covers the plotted time_grid.
  time_scale = "fixed",
  time_half_range = 3,
  prop_historical = 1
) {
  set.seed(replication_seed)

  families <- vector("list", n_families)

  for (i in seq_len(n_families)) {
    families[[i]] <- simulate_temporal_family(
      kpc = kpc,
      Ngen = Ngen,
      marR = marR,
      threshold_year = threshold_year,
      true_beta = true_beta,
      true_gamma = true_gamma,
      components = sim_components,
      gen_gap = gen_gap,
      birth_year_sd = birth_year_sd,
      birth_year_base = birth_year_base,
      family_id = i,
      poly = poly,
      rescale = TRUE,
      loading_link = loading_link,
      time_scale = time_scale,
      time_half_range = time_half_range,
      prop_historical = prop_historical
    )
  }

  family_sizes <- vapply(families, function(x) length(x$y), integer(1))
  all_H <- unlist(lapply(families, function(x) x$H), use.names = FALSE)
  all_birth_year_scaled <- unlist(
    lapply(families, function(x) x$birth_year_scaled),
    use.names = FALSE
  )
  historical_birth_years_scaled <- all_birth_year_scaled[all_H == 1L]
  list(
    replication = replication,
    seed = replication_seed,
    families = families,
    n_families = length(families),
    total_n = sum(family_sizes),
    mean_family_size = mean(family_sizes),
    min_family_size = min(family_sizes),
    max_family_size = max(family_sizes),
    mean_H = mean(all_H, na.rm = TRUE),
    z_year = if (length(historical_birth_years_scaled) > 0L) {
      min(historical_birth_years_scaled, na.rm = TRUE)
    } else {
      NA_real_
    }
  )
}

build_true_model <- function(families, replication) {
  group_models <- vector("list", length(families))

  for (i in seq_along(families)) {
    fam <- families[[i]]

    group_models[[i]] <- buildOneTemporalFamilyGroup(
      group_name = paste0("rep", replication, "_family", i),
      Addmat = fam$A,
      Nucmat = fam$Cn,
      Extmat = NULL,
      Mtdmat = NULL,
      Dmgmat = NULL,
      full_df_row = fam$y,
      obs_ids = fam$obs_ids,
      birth_year = fam$birth_year_scaled,
      H = fam$H,
      use_exp_loadings = use_exp_loadings
    )
  }

  temporal_model_ace <- buildTemporalPedigreeMx(
    model_name = paste0("TemporalPedigreeRecovery_ACE_rep", replication),
    group_models = group_models,
    p_hist = 1,
    components = fit_components,
    ci = FALSE
  )

  # Fit the true model: AE with linear birth-cohort moderation plus one
  # historical moderator.
  free_only(
    temporal_model_ace,
    labels_to_free = labels_to_free
  )
}

# OpenMx reports parameters in several shapes: omxGetParameters(fetch = "all")
# returns a data.frame whose labels are rownames and whose estimates are the
# "values" column, while output$standardErrors is a one-column matrix whose
# labels are also rownames. In both cases names() returns something other than
# the parameter labels. Collapse either shape to a named numeric vector.
as_named_parameter_vector <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NULL)
  }

  labels <- if (!is.null(rownames(x))) rownames(x) else names(x)

  values <- if (is.data.frame(x)) x[["values"]] else as.numeric(x)

  if (is.null(labels) || is.null(values) || length(labels) != length(values)) {
    return(NULL)
  }

  stats::setNames(as.numeric(values), labels)
}

# Look parameters up by label, leaving NA where the fit does not supply one.
# as.numeric() on the right-hand side is load-bearing: assigning a list or
# data.frame into a numeric vector coerces the vector to a list, even when the
# index is empty, which turns a label mismatch into a downstream is.finite()
# failure instead of a missing value.
pick_by_label <- function(source, wanted) {
  out <- stats::setNames(rep(NA_real_, length(wanted)), wanted)

  if (is.null(source)) {
    return(out)
  }

  available <- intersect(wanted, names(source))

  if (length(available) > 0L) {
    out[available] <- as.numeric(source[available])
  }

  out
}

fit_one_dataset <- function(model) {
  fit <- run_and_report(
    model,
    "ACE linear time + historical moderator",
    tries = optimizer_tries
  )

  status_code <- as.integer(fit$output$status$code)
  status_message <- fit$output$status$status

  all_estimates <- as_named_parameter_vector(
    omxGetParameters(fit, fetch = "all")
  )

  estimates <- pick_by_label(all_estimates, names(target))

  standard_errors <- pick_by_label(
    as_named_parameter_vector(fit$output$standardErrors),
    names(target)
  )

  list(
    fit = fit,
    status_code = status_code,
    status_message = status_message,
    converged = isTRUE(status_code %in% c(0L, 1L)) &&
      all(is.finite(estimates)),
    minus2ll = if (is.null(fit$output$Minus2LogLikelihood)) {
      NA_real_
    } else {
      as.numeric(fit$output$Minus2LogLikelihood)
    },
    iterations = if (is.null(fit$output$iterations)) {
      NA_real_
    } else {
      as.numeric(fit$output$iterations)
    },
    estimates = estimates,
    standard_errors = standard_errors,
    elapsed_seconds = if (is.null(fit$output$wallTime)) {
      NA_real_
    } else {
      as.numeric(fit$output$wallTime)
    }
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
  max_family_size = NA_real_,
  mean_H = NA_real_,
  z_year = NA_real_
) {
  estimate_values <- stats::setNames(
    rep(NA_real_, length(target)),
    names(target)
  )

  standard_error_values <- stats::setNames(
    rep(NA_real_, length(target)),
    paste0("se_", names(target))
  )
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
      iterations = NA_real_,
      elapsed_seconds = elapsed_seconds,
      mean_H = mean_H,
      z_year = z_year
    ),
    as.list(estimate_values),
    as.list(standard_error_values)
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
        replication_seed = replication_seed,
        prop_historical = prop_historical
      )

      model <- build_true_model(
        families = simulated$families,
        replication = replication
      )

      fitted <- fit_one_dataset(model)


      estimate_values <- stats::setNames(
        as.numeric(fitted$estimates),
        names(target)
      )

      standard_error_values <- stats::setNames(
        as.numeric(fitted$standard_errors),
        paste0("se_", names(target))
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
          iterations = fitted$iterations,
          elapsed_seconds = fitted$elapsed_seconds,
          mean_H = simulated$mean_H,
          z_year = simulated$z_year
        ),
        as.list(estimate_values),
        as.list(standard_error_values)
      ))
    },
    error = function(e) {
      elapsed_seconds <- proc.time()[["elapsed"]] - start_time

      failed_replication_row(
        replication = replication,
        replication_seed = replication_seed,
        elapsed_seconds = elapsed_seconds,
        error_message = conditionMessage(e),
        n_families_value = if (is.null(simulated)) {
          n_families
        } else {
          simulated$n_families
        },
        total_n = if (is.null(simulated)) {
          NA_real_
        } else {
          simulated$total_n
        },
        mean_family_size = if (is.null(simulated)) {
          NA_real_
        } else {
          simulated$mean_family_size
        },
        min_family_size = if (is.null(simulated)) {
          NA_real_
        } else {
          simulated$min_family_size
        },
        max_family_size = if (is.null(simulated)) {
          NA_real_
        } else {
          simulated$max_family_size
        },
        mean_H = if (is.null(simulated)) {
          NA_real_
        } else {
          simulated$mean_H
        },
        z_year = if (is.null(simulated)) {
          NA_real_
        } else {
          simulated$z_year
        }
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
    relative_bias = mean(bias / true_value, na.rm = TRUE),
    rmse = sqrt(mean(squared_error)),
    mc_lower = as.numeric(stats::quantile(estimate, 0.025, names = FALSE)),
    mc_upper = as.numeric(stats::quantile(estimate, 0.975, names = FALSE)),
    mc_q25 = as.numeric(
      stats::quantile(estimate, 0.25, names = FALSE)
    ),
    mc_q75 = as.numeric(
      stats::quantile(estimate, 0.75, names = FALSE)
    ),
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
parameter_order <- c(
  "b_a_0",  "b_a_1",  "b_a_2",  "g_a_1",
  "b_cn_0", "b_cn_1", "b_cn_2", "g_cn_1",
  "b_e_0",  "b_e_1",  "b_e_2",  "g_e_1"
)
parameter_labels <- c(
  b_a_0  = "A: intercept",
  b_a_1  = "A: linear time",
  b_a_2  = "A: quadratic time",
  g_a_1  = "A: historical event",
  b_cn_0 = "C: intercept",
  b_cn_1 = "C: linear time",
  b_cn_2 = "C: quadratic time",
  g_cn_1 = "C: historical event",
  b_e_0  = "E: intercept",
  b_e_1  = "E: linear time",
  b_e_2  = "E: quadratic time",
  g_e_1  = "E: historical event"
)


recovery_plot <- recovery_summary %>%
  mutate(
    parameter = factor(
      parameter,
      levels = rev(parameter_order)
    )
  ) %>%
  ggplot(
    aes(
      x = parameter, # stats::reorder(parameter, true_value),
      y = mean_estimate
    )
  ) +
  geom_tile(
    aes(
      y = (mc_lower + mc_upper) / 2,
      height = mc_upper - mc_lower
    ),
    width = 0.55,
    alpha = 0.20
  ) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_errorbar(
    aes(ymin = mc_q25, ymax = mc_q75),
    width = 0.15
  ) +
  geom_point(size = 2) +
  geom_point(
    aes(y = true_value),
    shape = 4,
    size = 3,
    stroke = 1
  ) +
  coord_flip() +
  theme_bw() +
  labs(
    title = "Temporal ACE Monte Carlo Parameter Recovery",
    subtitle = paste0(
      "Bands show empirical 95% intervals; thick bars show the middle 50%; ",
      "points are Monte Carlo means; crosses are generating values"
    ),
    x = "Parameter",
    y = "Estimate"
  ) +
  scale_x_discrete(labels = parameter_labels)

ggsave(
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

FIGURE <- TRUE
if (FIGURE) {
  # temporal_AE_panelB_variance_recovery.R
  #
  # Panel B for the temporal AE Monte Carlo recovery figure.
  #
  # This script maps the variance components implied by the recovered parameter
  # estimates against the true population variance components used to generate
  # the simulated data.
  #
  # For each successful Monte Carlo replication, the script:
  #   1. Transforms the recovered regression parameters into A, E, and total
  #      variance trajectories across time and historical condition.
  #   2. Computes the empirical Monte Carlo mean and 2.5th/97.5th percentiles of
  #      the recovered variance trajectories at every point on the time grid.
  #   3. Computes the corresponding true population trajectories from the known
  #      generating parameters.
  #   4. Plots the recovered trajectories and empirical Monte Carlo ranges
  #      directly against the population trajectories.
  #
  # Required inputs from temporal_BGmisc_AE_parameter_recovery.R:
  #   results/temporal_ACE_parameter_recovery/replication_results.csv
  #   results/temporal_ACE_parameter_recovery/parameter_recovery_summary.csv

  # -----------------------------------------------------------------------------
  # Package setup
  # -----------------------------------------------------------------------------

  required_packages <- c("dplyr", "tidyr", "readr", "ggplot2")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      "Missing required package(s): ", paste(missing_packages, collapse = ", "),
      "\nInstall them before running this script."
    )
  }

  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)

  # -----------------------------------------------------------------------------
  # File locations and plotting settings
  # -----------------------------------------------------------------------------

  results_directory <- file.path("results", core_folder)
  replication_file <- file.path(results_directory, "replication_results.csv")
  recovery_file <- file.path(results_directory, "parameter_recovery_summary.csv")

  output_data_file <- file.path(
    results_directory,
    "variance_component_recovery_over_time.csv"
  )
  output_png <- file.path(
    results_directory,
    "panel_B_variance_component_recovery.png"
  )
  output_pdf <- file.path(
    results_directory,
    "panel_B_variance_component_recovery.pdf"
  )

  output_data_file_ace <- file.path(
    results_directory,
    "variance_component_recovery_over_timeA.csv"
  )
  output_pnga <- file.path(
    results_directory,
    "panel_A_variance_component_recovery.png"
  )
  output_pdfa <- file.path(
    results_directory,
    "panel_A_variance_component_recovery.pdf"
  )

  # The model uses scaled birth year. Modify this range if needed to match the
  # range represented in the simulated or empirical data.
  time_grid <- seq(-3, 3, length.out = 301)

  # Use the Monte Carlo mean as the recovered trajectory. Set this to "median"
  # if the pointwise median is preferred as the central recovered trajectory.
  central_summary <- "mean"

  # -----------------------------------------------------------------------------
  # Read and validate Monte Carlo output
  # -----------------------------------------------------------------------------

  if (!file.exists(replication_file)) {
    stop(
      "Could not find: ", replication_file,
      "\nRun temporal_BGmisc_ACE_parameter_recovery.R first."
    )
  }

  if (!file.exists(recovery_file)) {
    stop(
      "Could not find: ", recovery_file,
      "\nRun temporal_BGmisc_ACE_parameter_recovery.R first."
    )
  }

  replication_results <- readr::read_csv(
    replication_file,
    show_col_types = FALSE
  )

  recovery_summary <- readr::read_csv(
    recovery_file,
    show_col_types = FALSE
  )

  parameter_names <- c(
    "b_a_0", "b_a_1", "b_a_2", "g_a_1",
    "b_cn_0", "b_cn_1", "b_cn_2", "g_cn_1",
    "b_e_0", "b_e_1", "b_e_2", "g_e_1"
  )

  required_replication_columns <- c(
    "replication", "converged", "z_year", parameter_names
  )

  missing_replication_columns <- setdiff(
    required_replication_columns,
    names(replication_results)
  )

  if (length(missing_replication_columns) > 0) {
    stop(
      "replication_results.csv is missing required column(s): ",
      paste(missing_replication_columns, collapse = ", ")
    )
  }

  required_recovery_columns <- c("parameter", "true_value")
  missing_recovery_columns <- setdiff(
    required_recovery_columns,
    names(recovery_summary)
  )

  if (length(missing_recovery_columns) > 0) {
    stop(
      "parameter_recovery_summary.csv is missing required column(s): ",
      paste(missing_recovery_columns, collapse = ", ")
    )
  }

  # Retain only converged replications with finite estimates for every parameter
  # needed to calculate the variance trajectories.
  usable_results <- replication_results %>%
    filter(converged) %>%
    filter(if_all(all_of(parameter_names), is.finite))

  if (nrow(usable_results) == 0L) {
    stop("No converged replications with finite temporal ACE parameters were found.")
  }


  historical_threshold <- stats::median(
    usable_results$z_year[is.finite(usable_results$z_year)],
    na.rm = TRUE
  )
  if (!is.finite(historical_threshold)) {
    stop("Could not identify a finite scaled historical-event threshold.")
  }
  # Read the true population parameters directly from the Monte Carlo summary so
  # this plotting script remains synchronized with the generating model.
  true_parameters <- recovery_summary %>%
    filter(parameter %in% parameter_names) %>%
    select(parameter, true_value) %>%
    tidyr::pivot_wider(
      names_from = parameter,
      values_from = true_value
    )

  if (nrow(true_parameters) != 1L ||
    any(!is.finite(as.numeric(true_parameters[1, parameter_names])))) {
    stop("Could not recover one finite true value for every temporal ACE parameter.")
  }

  # -----------------------------------------------------------------------------
  # Convert each recovered parameter vector into variance-component trajectories
  # -----------------------------------------------------------------------------

  # One row is created for every replication, time value, and historical state.
  # These are model-implied variance components, not variances calculated from
  # bins of observed phenotypes.
  recovered_curves <- usable_results %>%
    select(replication, all_of(parameter_names)) %>%
    tidyr::crossing(
      time = time_grid # ,
      #   historical = c(0L, 1L)
    ) %>%
    mutate(
      historical = as.integer(time > historical_threshold),
      # Each component contributes lambda^2 = exp(2 * eta) to the variance.
      a_variance = exp(2 * (
        b_a_0 +
          b_a_1 * time +
          b_a_2 * time^2 +
          g_a_1 * historical
      )),
      cn_variance = exp(2 * (
        b_cn_0 +
          b_cn_1 * time +
          b_cn_2 * time^2 +
          g_cn_1 * historical
      )),
      e_variance = exp(2 * (
        b_e_0 +
          b_e_1 * time +
          b_e_2 * time^2 +
          g_e_1 * historical
      )),
      total_variance = a_variance +
        cn_variance +
        e_variance
    ) %>%
    select(
      replication,
      time,
      historical,
      a_variance,
      cn_variance,
      e_variance,
      total_variance
    ) %>%
    pivot_longer(
      cols = c(
        a_variance,
        cn_variance,
        e_variance,
        total_variance
      ),
      names_to = "component",
      values_to = "recovered_variance"
    )

  # Empirical Monte Carlo distribution of the recovered variance component at
  # each time point and historical condition.
  recovered_summary <- recovered_curves %>%
    group_by(time, historical, component) %>%
    summarise(
      n_replications = n(),
      recovered_mean = mean(recovered_variance),
      recovered_median = median(recovered_variance),
      recovered_lower = as.numeric(
        quantile(recovered_variance, probs = 0.025, names = FALSE)
      ),
      recovered_upper = as.numeric(
        quantile(recovered_variance, probs = 0.975, names = FALSE)
      ),
      .groups = "drop"
    )

  # -----------------------------------------------------------------------------
  # Calculate the true population variance-component trajectories
  # -----------------------------------------------------------------------------

  population_curves <- tidyr::expand_grid(
    time = time_grid,
    #  historical = c(0L, 1L),
  ) %>%
    mutate(
      historical = as.integer(time > historical_threshold),
      # Must mirror recovered_curves exactly, or the two lines are not comparable.
      a_variance = exp(2 * (
        true_parameters$b_a_0 +
          true_parameters$b_a_1 * time +
          true_parameters$b_a_2 * time^2 +
          true_parameters$g_a_1 * historical
      )),
      cn_variance = exp(2 * (
        true_parameters$b_cn_0 +
          true_parameters$b_cn_1 * time +
          true_parameters$b_cn_2 * time^2 +
          true_parameters$g_cn_1 * historical
      )),
      e_variance = exp(2 * (
        true_parameters$b_e_0 +
          true_parameters$b_e_1 * time +
          true_parameters$b_e_2 * time^2 +
          true_parameters$g_e_1 * historical
      )),
      total_variance = a_variance +
        cn_variance +
        e_variance
    ) %>%
    select(
      time,
      historical,
      a_variance,
      cn_variance,
      e_variance,
      total_variance
    ) %>%
    pivot_longer(
      cols = c(a_variance, cn_variance, e_variance, total_variance),
      names_to = "component",
      values_to = "population_variance"
    )

  # Join the Monte Carlo findings directly to the corresponding population values.
  plot_data <- recovered_summary %>%
    left_join(
      population_curves,
      by = c("time", "historical", "component")
    ) %>%
    mutate(
      recovered_central = if (central_summary == "median") {
        recovered_median
      } else {
        recovered_mean
      },
      #  historical = factor(
      #    historical,
      #    levels = c(0L, 1L),
      #    labels = c("Before historical event", "After historical event")
      #  ),
      component = factor(
        component,
        levels = c(
          "a_variance",
          "cn_variance",
          "e_variance", "total_variance"
        ),
        labels = c(
          "Additive genetic variance",
          "Common environmental variance",
          "Nonshared environmental variance",
          "Total phenotypic variance"
        )
      )
    )
  # hybrid that combines pre and post estimate. because the historical event only occurs when time > H


  readr::write_csv(plot_data, output_data_file, na = "")

  # -----------------------------------------------------------------------------
  # Create Panel B
  # -----------------------------------------------------------------------------

  panel_b <- ggplot(
    plot_data,
    aes(
      x = time, color = factor(component),
      fill = factor(component)
    )
  ) +
    annotate(
      "rect",
      xmin = historical_threshold,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      fill = "grey80",
      alpha = 0.20
    ) +
    geom_vline(
      xintercept = historical_threshold,
      linetype = "dotdash",
      linewidth = 0.6
    ) +
    # The ribbon is the empirical 95% range of the variance estimates obtained
    # from the successful Monte Carlo replications.
    geom_ribbon(
      aes(
        ymin = recovered_lower,
        ymax = recovered_upper,
      ),
      alpha = 0.20
    ) +
    # Solid line: central variance estimate implied by the recovered parameters.
    geom_line(
      aes(
        y = recovered_central,
        linetype = "Recovered Monte Carlo estimate"
      ),
      linewidth = 0.9
    ) +
    # Dashed line: known population variance used to generate the data.
    geom_line(
      aes(
        y = population_variance,
        linetype = "Population parameter"
      ),
      linewidth = 0.9
    ) +
    #  facet_grid(
    #  rows = vars(component),
    #    cols = vars(historical)#,
    #   scales = "free_y"
    #  ) +
    scale_linetype_manual(
      name = NULL,
      values = c(
        "Recovered Monte Carlo estimate" = "solid",
        "Population parameter" = "dashed"
      )
    ) +
    labs(
      title = "Recovery of time-varying variance components",
      subtitle = paste0(
        "Recovered ", central_summary,
        " and empirical 95% Monte Carlo range mapped against the population trajectory"
      ),
      x = "Scaled birth year",
      y = "Variance"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 9),
      strip.text = element_text(face = "bold", size = 9),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = element_text(size = 8)
    )

  print(panel_b)

  # -----------------------------------------------------------------------------
  # Save figure
  # -----------------------------------------------------------------------------

  ggsave(
    filename = output_png,
    plot = panel_b,
    width = 10.5,
    height = 8,
    dpi = 400,
    bg = "white"
  )

  ggsave(
    filename = output_pdf,
    plot = panel_b,
    width = 10.5,
    height = 8,
    device = cairo_pdf,
    bg = "white"
  )

  cat("Panel B data and figures written to:\n")
  cat("  ", normalizePath(output_data_file, mustWork = FALSE), "\n", sep = "")
  cat("  ", normalizePath(output_png, mustWork = FALSE), "\n", sep = "")
  cat("  ", normalizePath(output_pdf, mustWork = FALSE), "\n", sep = "")
}


# Figure A
# -----------------------------------------------------------------------------

# Calculate the true population variance-component trajectories

# -----------------------------------------------------------------------------
if (FIGURE) {
  recovered_curves_a <- usable_results %>%
    select(replication, all_of(parameter_names)) %>%
    tidyr::crossing(
      time = time_grid,
      historical = c(0L, 1L)
    ) %>%
    mutate(
      #  historical = as.integer(time > historical_threshold),
      # Each component contributes lambda^2 = exp(2 * eta) to the variance.
      a_variance = exp(2 * (
        b_a_0 + b_a_1 * time + b_a_2 * time^2 + g_a_1 * historical
      )),
      cn_variance = exp(2 * (
        b_cn_0 + b_cn_1 * time + b_cn_2 * time^2 + g_cn_1 * historical
      )),
      e_variance = exp(2 * (
        b_e_0 + b_e_1 * time + b_e_2 * time^2 + g_e_1 * historical
      )),
      total_variance = a_variance + cn_variance + e_variance
    ) %>%
    select(
      replication,
      time,
      historical,
      a_variance,
      cn_variance,
      e_variance,
      total_variance
    ) %>%
    pivot_longer(
      cols = c(a_variance, cn_variance, e_variance, total_variance),
      names_to = "component",
      values_to = "recovered_variance"
    )

  # Empirical Monte Carlo distribution of the recovered variance component at
  # each time point and historical condition.
  recovered_summary_a <- recovered_curves_a %>%
    group_by(time, historical, component) %>%
    summarise(
      n_replications = n(),
      recovered_mean = mean(recovered_variance),
      recovered_median = median(recovered_variance),
      recovered_lower = as.numeric(
        quantile(recovered_variance, probs = 0.025, names = FALSE)
      ),
      recovered_upper = as.numeric(
        quantile(recovered_variance, probs = 0.975, names = FALSE)
      ),
      .groups = "drop"
    )


  population_curves_a <- tidyr::expand_grid(
    time = time_grid,
    historical = c(0L, 1L),
  ) %>%
    mutate(
      # Must mirror recovered_curves_a exactly, or the two lines are not comparable.
      a_variance = exp(2 * (
        true_parameters$b_a_0 +
          true_parameters$b_a_1 * time +
          true_parameters$b_a_2 * time^2 +
          true_parameters$g_a_1 * historical
      )),
      cn_variance = exp(2 * (
        true_parameters$b_cn_0 +
          true_parameters$b_cn_1 * time +
          true_parameters$b_cn_2 * time^2 +
          true_parameters$g_cn_1 * historical
      )),
      e_variance = exp(2 * (
        true_parameters$b_e_0 +
          true_parameters$b_e_1 * time +
          true_parameters$b_e_2 * time^2 +
          true_parameters$g_e_1 * historical
      )),
      total_variance = a_variance + cn_variance + e_variance
    ) %>%
    select(
      time,
      historical,
      a_variance,
      cn_variance,
      e_variance,
      total_variance
    ) %>%
    pivot_longer(
      cols = c(a_variance, cn_variance, e_variance, total_variance),
      names_to = "component",
      values_to = "population_variance"
    )

  # Join the Monte Carlo findings directly to the corresponding population values.

  plot_data <- recovered_summary_a %>%
    left_join(
      population_curves_a,
      by = c("time", "historical", "component")
    ) %>%
    mutate(
      recovered_central = if (central_summary == "median") {
        recovered_median
      } else {
        recovered_mean
      },
      historical = factor(
        historical,
        levels = c(0L, 1L),
        labels = c("Before historical event", "After historical event")
      ),
      component = factor(
        component,
        levels = c(
          "a_variance",
          "cn_variance",
          "e_variance",
          "total_variance"
        ),
        labels = c(
          "Additive genetic variance",
          "Common environmental variance",
          "Nonshared environmental variance",
          "Total phenotypic variance"
        )
      )
    )

  # hybrid that combines pre and post estimate. because the historical event only occurs when time > H


  # -----------------------------------------------------------------------------

  # Create Panel a

  # -----------------------------------------------------------------------------

  panel_a <- ggplot(
    plot_data,
    aes(
      x = time, color = factor(component),
      fill = factor(component)
    )
  ) +

    # The ribbon is the empirical 95% range of the variance estimates obtained

    # from the successful Monte Carlo replications.

    geom_ribbon(
      aes(
        ymin = recovered_lower,
        ymax = recovered_upper
      ),
      alpha = 0.20
    ) +

    # Solid line: central variance estimate implied by the recovered parameters.

    geom_line(
      aes(
        y = recovered_central,
        linetype = "Recovered Monte Carlo estimate"
      ),
      linewidth = 0.9
    ) +

    # Dashed line: known population variance used to generate the data.

    geom_line(
      aes(
        y = population_variance,
        linetype = "Population parameter"
      ),
      linewidth = 0.9
    ) +
    facet_grid(
      # rows = vars(component),
      cols = vars(historical) # ,

      # scales = "free_y"
    ) +
    scale_linetype_manual(
      name = NULL,
      values = c(
        "Recovered Monte Carlo estimate" = "solid",
        "Population parameter" = "dashed"
      )
    ) +
    labs(
      title = "Recovery of time-varying variance components",
      subtitle = paste0(
        "Recovered ", central_summary,
        " and empirical 95% Monte Carlo range mapped against the population trajectory"
      ),
      x = "Scaled birth year",
      y = "Variance"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 9),
      strip.text = element_text(face = "bold", size = 9),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = element_text(size = 8)
    )

  print(panel_a)

  # -----------------------------------------------------------------------------

  # Save figure

  # -----------------------------------------------------------------------------


  ggsave(
    filename = output_pnga,
    plot = panel_a,
    width = 10.5,
    height = 8,
    dpi = 400,
    bg = "white"
  )

  ggsave(
    filename = output_pdfa,
    plot = panel_a,
    width = 10.5,
    height = 8,
    device = cairo_pdf,
    bg = "white"
  )

  cat("Panel A data and figures written to:\n")
  cat("  ", normalizePath(output_data_file_ace, mustWork = FALSE), "\n", sep = "")
  cat("  ", normalizePath(output_pnga, mustWork = FALSE), "\n", sep = "")
  cat("  ", normalizePath(output_pdfa, mustWork = FALSE), "\n", sep = "")
}
