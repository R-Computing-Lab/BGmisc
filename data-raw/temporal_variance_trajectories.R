# temporal_variance_trajectories.R
#
# Shared notation and model-implied variance math for the temporal BGmisc/OpenMx
# variance-components models. This file has no side effects: it reads nothing,
# writes nothing, and defines only constants and functions, so it can be sourced
# from any script that needs to turn temporal parameters into variances.
#
# Sourced by:
#   data-raw/temporal_basic_mc_plots.R           (parameter-recovery figures)
#   FamiliesofEngland/scripts/export_temporal_estimates.R
#   timinglongevity_graphs/scripts/ace_from_model.R
#
# The loading for component k is exp(eta_k) and enters the covariance as an
# outer product, so the variance contributed by component k is exp(2 * eta_k).
# That factor of 2 is why these functions exponentiate twice the linear
# predictor; anything that uses exp(eta) alone is on the standard-deviation
# scale, not the variance scale.

required_packages <- c("dplyr", "tidyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them before sourcing this file."
  )
}

library(dplyr)
library(tidyr)

# -----------------------------------------------------------------------------
# Notation
# -----------------------------------------------------------------------------

parameter_names <- c(
  "b_a_0", "b_a_1", "b_a_2", "g_a_1",
  "b_cn_0", "b_cn_1", "b_cn_2", "g_cn_1",
  "b_e_0", "b_e_1", "b_e_2", "g_e_1"
)

component_order <- c(
  "a_variance",
  "cn_variance",
  "e_variance",
  "total_variance"
)

component_labels <- c(
  a_variance = "Additive genetic variance",
  cn_variance = "Nuclear-family environmental variance",
  e_variance = "Nonshared environmental variance",
  total_variance = "Phenotypic variance"
)

historical_period_labels <- c(
  `0` = "Before the historical event",
  `1` = "After the historical event"
)

event_experience_labels <- c(
  `0` = "Event not experienced",
  `1` = "Event experienced"
)

# Prefix used by each component's parameters, keyed by the variance column it
# produces. Used to zero out components a model never estimated.
component_parameter_prefix <- c(
  a_variance = "a",
  cn_variance = "cn",
  e_variance = "e"
)

# -----------------------------------------------------------------------------
# Shared figure-data helpers
# -----------------------------------------------------------------------------

# Convert parameter rows into model-implied variance trajectories. The input
# parameter_data may contain one population row or many Monte Carlo rows.
# prediction_grid must contain time, historical_period, and event_experienced.
calculate_variance_trajectories <- function(
  parameter_data,
  prediction_grid,
  id_columns = character(),
  variance_column = "variance"
) {
  required_grid_columns <- c(
    "time",
    "historical_period",
    "event_experienced"
  )

  missing_grid_columns <- setdiff(
    required_grid_columns,
    names(prediction_grid)
  )

  if (length(missing_grid_columns) > 0L) {
    stop(
      "prediction_grid is missing required column(s): ",
      paste(missing_grid_columns, collapse = ", ")
    )
  }

  missing_parameter_columns <- setdiff(
    parameter_names,
    names(parameter_data)
  )

  if (length(missing_parameter_columns) > 0L) {
    stop(
      "parameter_data is missing required column(s): ",
      paste(missing_parameter_columns, collapse = ", ")
    )
  }

  trajectories <- tidyr::crossing(
    parameter_data,
    prediction_grid
  ) %>%
    dplyr::mutate(
      event_effect = historical_period * event_experienced,
      a_variance = exp(2 * (
        b_a_0 +
          b_a_1 * time +
          b_a_2 * time^2 +
          g_a_1 * event_effect
      )),
      cn_variance = exp(2 * (
        b_cn_0 +
          b_cn_1 * time +
          b_cn_2 * time^2 +
          g_cn_1 * event_effect
      )),
      e_variance = exp(2 * (
        b_e_0 +
          b_e_1 * time +
          b_e_2 * time^2 +
          g_e_1 * event_effect
      )),
      total_variance = a_variance + cn_variance + e_variance
    ) %>%
    dplyr::select(
      dplyr::all_of(id_columns),
      time,
      historical_period,
      event_experienced,
      event_effect,
      dplyr::all_of(component_order)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(component_order),
      names_to = "component",
      values_to = variance_column
    )

  trajectories
}

# -----------------------------------------------------------------------------
# Parameter tables
# -----------------------------------------------------------------------------

# Widen a named vector of estimates into the single row calculate_variance_
# trajectories() expects. Parameters a model never freed are absent from
# omxGetParameters(); they are filled with 0 here, which is their fixed value.
temporal_parameters_wide <- function(estimates, extra = NULL) {
  estimates <- unlist(estimates)

  unknown <- setdiff(names(estimates), c(parameter_names, "mean_y"))
  if (length(unknown) > 0L) {
    warning(
      "Ignoring parameter(s) with no place in the temporal notation: ",
      paste(unknown, collapse = ", ")
    )
  }

  values <- stats::setNames(rep(0, length(parameter_names)), parameter_names)
  shared <- intersect(names(estimates), parameter_names)
  values[shared] <- estimates[shared]

  row <- tibble::as_tibble(as.list(values))

  if (!is.null(extra)) {
    row <- dplyr::bind_cols(tibble::as_tibble(as.list(extra)), row)
  }

  row
}

# -----------------------------------------------------------------------------
# Calendar-year predictions
# -----------------------------------------------------------------------------

# Map calendar years onto the model's scaled time axis and set the two
# moderator indicators.
#
# historical_period is 1 from event_year onward, matching how the moderator is
# coded in the fitted models: a period contrast, not a transient shock.
# event_experienced selects whose trajectory to draw -- the cohort that lived
# through the event (1) or the counterfactual that did not (0). The moderator
# only bites where both are 1, so the two indicators together produce the
# before/after step in a single exposed trajectory.
calendar_prediction_grid <- function(
  years,
  year_mean,
  year_sd,
  event_year,
  event_experienced = 1
) {
  if (!is.numeric(year_sd) || length(year_sd) != 1L || year_sd <= 0) {
    stop("year_sd must be a single positive number.")
  }

  tidyr::crossing(
    year = sort(unique(as.numeric(years))),
    event_experienced = as.numeric(event_experienced)
  ) %>%
    dplyr::mutate(
      time = (year - year_mean) / year_sd,
      historical_period = as.numeric(year >= event_year)
    )
}

# Turn one row of temporal parameters into ACE proportions on a calendar-year
# grid, in the year/A_val/C_val/E_val shape the longevity panels consume.
#
# Va, Vc, Ve, and Vp are variance components on the phenotype's own scale.
# A_val, C_val, and E_val are their shares of Vp -- ratios, not variances. For
# the model's own phenotype A_val is a heritability; applied to some other
# quantity, such as a between-population spread, it is only a share and calling
# it a heritability would be wrong. Nothing here should be read as a diagnostic
# of how well the components were estimated: a share sitting near 0 or 1 means
# one component is much larger than another, which can be a perfectly well
# estimated state of affairs. Use check_temporal_identification() for that.
#
# components must list only the components the model actually estimated. A
# component whose parameters are all 0 still yields exp(2 * 0) = 1, so leaving
# "cn" in for an AE model would silently add a unit of shared-environment
# variance that was never estimated.
ace_proportions_from_parameters <- function(
  parameter_data,
  years,
  year_mean,
  year_sd,
  event_year,
  components = c("a", "cn", "e"),
  event_experienced = 1
) {
  unknown_components <- setdiff(components, component_parameter_prefix)
  if (length(unknown_components) > 0L) {
    stop(
      "Unsupported component(s): ",
      paste(unknown_components, collapse = ", "),
      ". Supported: ", paste(component_parameter_prefix, collapse = ", ")
    )
  }
  if (!"a" %in% components || !"e" %in% components) {
    stop("components must include at least \"a\" and \"e\".")
  }
  if (nrow(parameter_data) != 1L) {
    stop("parameter_data must be exactly one row of parameters.")
  }

  grid <- calendar_prediction_grid(
    years = years,
    year_mean = year_mean,
    year_sd = year_sd,
    event_year = event_year,
    event_experienced = event_experienced
  )

  estimated <- names(component_parameter_prefix)[
    component_parameter_prefix %in% components
  ]

  calculate_variance_trajectories(
    parameter_data = parameter_data,
    prediction_grid = grid,
    id_columns = "year"
  ) %>%
    dplyr::filter(component != "total_variance") %>%
    dplyr::mutate(
      variance = dplyr::if_else(component %in% estimated, variance, 0)
    ) %>%
    tidyr::pivot_wider(
      id_cols = c(year, historical_period, event_experienced, event_effect),
      names_from = component,
      values_from = variance
    ) %>%
    dplyr::mutate(
      Vp = a_variance + cn_variance + e_variance,
      A_val = a_variance / Vp,
      C_val = cn_variance / Vp,
      E_val = e_variance / Vp
    ) %>%
    dplyr::rename(
      Va = a_variance,
      Vc = cn_variance,
      Ve = e_variance
    ) %>%
    dplyr::arrange(year)
}

# Report whether a fit's parameters are actually pinned down, from the profile-
# likelihood intervals on the parameters themselves.
#
# This deliberately does not look at variance shares. A share near 0 or 1 says
# one component is much larger than another at that point on the time axis,
# which is a property of the estimates, not evidence that they are unreliable.
# What does indicate trouble is a confidence bound the optimiser could not
# find, a moderator whose interval covers zero, or an interval so wide on the
# log-loading scale that the component spans orders of magnitude.
#
# estimates_row must carry <parameter>_lbound and <parameter>_ubound columns
# alongside the point estimates, as written by export_temporal_estimates().
check_temporal_identification <- function(
  estimates_row,
  wide_interval = 2,
  warn = TRUE
) {
  stopifnot(nrow(estimates_row) == 1L)

  freed <- parameter_names[
    vapply(
      parameter_names,
      function(p) isTRUE(estimates_row[[p]] != 0),
      logical(1)
    )
  ]

  bound_column <- function(parameter, bound) {
    column <- paste0(parameter, "_", bound)
    if (column %in% names(estimates_row)) estimates_row[[column]] else NA_real_
  }

  diagnostics <- dplyr::bind_rows(lapply(freed, function(p) {
    lbound <- bound_column(p, "lbound")
    ubound <- bound_column(p, "ubound")

    tibble::tibble(
      parameter = p,
      estimate = estimates_row[[p]],
      lbound = lbound,
      ubound = ubound,
      bound_missing = is.na(lbound) || is.na(ubound),
      covers_zero = !is.na(lbound) && !is.na(ubound) &&
        lbound < 0 && ubound > 0,
      interval_width = ubound - lbound,
      interval_wide = !is.na(lbound) && !is.na(ubound) &&
        (ubound - lbound) > wide_interval
    )
  }))

  if (nrow(diagnostics) == 0L) {
    return(invisible(diagnostics))
  }

  suspect <- diagnostics %>%
    dplyr::filter(bound_missing | covers_zero | interval_wide)

  if (warn && nrow(suspect) > 0L) {
    warning(
      "Weakly identified parameter(s): ",
      paste(suspect$parameter, collapse = ", "),
      ". ",
      if (any(suspect$bound_missing)) {
        paste0(
          "No confidence bound was found for ",
          paste(suspect$parameter[suspect$bound_missing], collapse = ", "),
          ", which means the likelihood is flat in that direction. "
        )
      } else {
        ""
      },
      "Treat the corresponding variance component as unpinned."
    )
  }

  invisible(diagnostics)
}
