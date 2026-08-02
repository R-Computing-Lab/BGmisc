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
# Predictions on the scaled time axis
# -----------------------------------------------------------------------------

# The model's native axis. Everything is evaluated here first; putting it on a
# calendar is a rescaling applied afterwards, never a re-derivation of the
# scaling that produced the fitted parameters.
time_grid_default <- seq(-3, 3, length.out = 301)

# Build the prediction grid on the scaled axis.
#
# event_threshold is the scaled time at which the historical period begins. It
# must be supplied from whatever recorded the event, not recomputed from raw
# data.
#
# event_experienced is who, at each point on the axis, actually carries the
# event. In the fitted models the moderator is an individual attribute -- for
# the 1918 flu, alive in 1917 -- so it belongs to a set of birth cohorts, not to
# a span of calendar time. A population's exposed share therefore rises when the
# event happens and then decays as those cohorts die out and are replaced by
# people born too late to have been eligible. Pass a function of calendar year
# to describe that decay; pass a scalar only when the exposed share genuinely is
# constant, such as 1 for the exposed cohort's own trajectory or 0 for the
# never-exposed counterfactual.
#
# Since event_effect is historical_period * event_experienced and multiplies
# gamma on the log-loading scale, a fractional share interpolates the moderator
# there rather than mixing two variances. That is a modelling choice, not an
# identity: it says a half-exposed population sits halfway between the two
# loadings, which is not the same as being a 50/50 mixture of them.
scaled_prediction_grid <- function(
  event_threshold,
  year_mean,
  year_sd,
  time = time_grid_default,
  event_experienced = 1
) {
  if (!is.numeric(event_threshold) || length(event_threshold) != 1L) {
    stop("event_threshold must be a single number on the scaled time axis.")
  }

  # Include the threshold itself, from both sides, so the step is not smeared
  # by whatever resolution the grid happens to have.
  time <- sort(unique(c(
    time,
    event_threshold,
    event_threshold - .Machine$double.eps^0.5
  )))

  grid <- tibble::tibble(time = time) %>%
    dplyr::mutate(
      year = rescale_time_to_calendar(time, year_mean, year_sd),
      historical_period = as.numeric(time >= event_threshold)
    )

  grid$event_experienced <- if (is.function(event_experienced)) {
    share <- as.numeric(event_experienced(grid$year))

    if (length(share) != nrow(grid)) {
      stop("event_experienced() must return one value per year.")
    }
    if (any(is.na(share)) || any(share < 0) || any(share > 1)) {
      stop("event_experienced() must return shares between 0 and 1.")
    }

    share
  } else {
    rep_len(as.numeric(event_experienced), nrow(grid))
  }

  grid
}

# Put the scaled axis onto calendar years. year_mean and year_sd are the
# constants recorded by the run that fit the models.
rescale_time_to_calendar <- function(time, year_mean, year_sd) {
  if (!is.numeric(year_sd) || length(year_sd) != 1L || year_sd <= 0) {
    stop("year_sd must be a single positive number.")
  }

  time * year_sd + year_mean
}

rescale_calendar_to_time <- function(year, year_mean, year_sd) {
  if (!is.numeric(year_sd) || length(year_sd) != 1L || year_sd <= 0) {
    stop("year_sd must be a single positive number.")
  }

  (year - year_mean) / year_sd
}

# Turn one row of temporal parameters into a variance trajectory on the scaled
# time axis, then rescale that axis to calendar years.
#
# The order matters. The trajectory is evaluated on the model's own grid, the
# same seq(-3, 3) the parameter-recovery figures use. Only afterwards is the
# axis relabelled, using year_mean and year_sd as recorded by the fitting run
# and event_threshold as recorded for the event. Nothing here recomputes a
# scaling from raw data, so the calendar mapping is exactly as trustworthy as
# those recorded constants and no more.
#
# `years` requests calendar years for the panel. Shares are interpolated onto
# them within each historical period separately, so the step at the event
# survives instead of being averaged across.
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
  year_mean,
  year_sd,
  event_threshold,
  years = NULL,
  components = c("a", "cn", "e"),
  event_experienced = 1,
  time = time_grid_default
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

  grid <- scaled_prediction_grid(
    event_threshold = event_threshold,
    year_mean = year_mean,
    year_sd = year_sd,
    time = time,
    event_experienced = event_experienced
  )

  estimated <- names(component_parameter_prefix)[
    component_parameter_prefix %in% components
  ]

  trajectory <- calculate_variance_trajectories(
    parameter_data = parameter_data,
    prediction_grid = grid
  ) %>%
    dplyr::filter(component != "total_variance") %>%
    dplyr::mutate(
      variance = dplyr::if_else(component %in% estimated, variance, 0)
    ) %>%
    tidyr::pivot_wider(
      id_cols = c(time, historical_period, event_experienced, event_effect),
      names_from = component,
      values_from = variance
    ) %>%
    dplyr::mutate(
      Vp = a_variance + cn_variance + e_variance,
      A_val = a_variance / Vp,
      C_val = cn_variance / Vp,
      E_val = e_variance / Vp,
      # The axis becomes a calendar only at this point.
      year = rescale_time_to_calendar(time, year_mean, year_sd)
    ) %>%
    dplyr::rename(
      Va = a_variance,
      Vc = cn_variance,
      Ve = e_variance
    ) %>%
    dplyr::arrange(time)

  if (is.null(years)) {
    return(trajectory)
  }

  interpolate_on_calendar(
    trajectory = trajectory,
    years = years,
    year_mean = year_mean,
    year_sd = year_sd,
    event_threshold = event_threshold
  )
}

# Read the trajectory off at the panel's calendar years. Each historical period
# is interpolated on its own, so a year just before the event never borrows from
# one just after it.
interpolate_on_calendar <- function(
  trajectory,
  years,
  year_mean,
  year_sd,
  event_threshold
) {
  years <- sort(unique(as.numeric(years)))
  target_time <- rescale_calendar_to_time(years, year_mean, year_sd)

  if (min(target_time) < min(trajectory$time) ||
        max(target_time) > max(trajectory$time)) {
    warning(
      "Requested years fall outside the scaled time grid (",
      round(rescale_time_to_calendar(min(trajectory$time), year_mean, year_sd)),
      "-",
      round(rescale_time_to_calendar(max(trajectory$time), year_mean, year_sd)),
      "). Widen `time` rather than extrapolating."
    )
  }

  target <- tibble::tibble(
    year = years,
    time = target_time,
    historical_period = as.numeric(target_time >= event_threshold)
  )

  # event_experienced and event_effect are interpolated alongside the variances
  # because the exposed share moves over the grid whenever a decaying exposure
  # function was supplied.
  value_columns <- c(
    "Va", "Vc", "Ve", "Vp", "A_val", "C_val", "E_val",
    "event_experienced", "event_effect"
  )

  interpolated <- lapply(split(target, target$historical_period), function(part) {
    source_part <- trajectory %>%
      dplyr::filter(historical_period == part$historical_period[1])

    for (column in value_columns) {
      part[[column]] <- stats::approx(
        x = source_part$time,
        y = source_part[[column]],
        xout = part$time,
        rule = 2
      )$y
    }

    part
  })

  dplyr::bind_rows(interpolated) %>%
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

  # A fit run without intervals = TRUE has no bounds at all. That is an absence
  # of evidence, not evidence of a boundary fit, so say so instead of flagging
  # every parameter. A fit with some bounds and some missing is the real signal.
  diagnostics$intervals_requested <- !all(diagnostics$bound_missing)

  if (!diagnostics$intervals_requested[1]) {
    diagnostics$bound_missing <- FALSE

    if (warn) {
      warning(
        "This fit carries no confidence intervals, so identification was not ",
        "assessed. Re-fit with confidence_intervals = TRUE to check it."
      )
    }

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
