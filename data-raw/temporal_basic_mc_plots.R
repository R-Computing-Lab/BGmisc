# temporal_BGmisc_AE_parameter_recovery_figures.R
#
# Standalone figure-data preparation and plotting for the temporal BGmisc/OpenMx
# parameter-recovery simulation.
#
# This script does not run the simulation. It reads the files written by
# temporal_BGmisc_AE_parameter_recovery_simulation.R, prepares the recovered and
# population variance trajectories, and creates Panels A and B.

# -----------------------------------------------------------------------------
# Package setup
# -----------------------------------------------------------------------------

required_packages <- c("dplyr", "tidyr", "readr", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them before running this script."
  )
}

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# -----------------------------------------------------------------------------
# Input and output locations
# -----------------------------------------------------------------------------
# check if this already exists, so that I can source the file after the simulation
if(!exists("core_folder")){
core_folder <- "temporal_ACE_parameter_recovery_500"
}
results_directory <- file.path("results", core_folder)

replication_file <- file.path(
  results_directory,
  "replication_results.csv"
)

recovery_file <- file.path(
  results_directory,
  "parameter_recovery_summary.csv"
)

make_panel_files <- function(panel, directory = results_directory) {
  stem <- paste0(
    "panel_",
    tolower(panel),
    "_variance_component_recovery"
  )

  list(
    data = file.path(directory, paste0(stem, ".csv")),
    png = file.path(directory, paste0(stem, ".png")),
    pdf = file.path(directory, paste0(stem, ".pdf"))
  )
}

panel_a_files <- make_panel_files("A")
panel_b_files <- make_panel_files("B")

# -----------------------------------------------------------------------------
# Figure settings and notation
# -----------------------------------------------------------------------------

time_grid <- seq(-3, 3, length.out = 301)
central_summary <- "mean"

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

# -----------------------------------------------------------------------------
# Read and validate simulation output
# -----------------------------------------------------------------------------

if (!file.exists(replication_file)) {
  stop(
    "Could not find: ", replication_file,
    "\nRun the simulation script first."
  )
}

if (!file.exists(recovery_file)) {
  stop(
    "Could not find: ", recovery_file,
    "\nRun the simulation script first."
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

required_replication_columns <- c(
  "replication",
  "converged",
  "z_year",
  parameter_names
)

missing_replication_columns <- setdiff(
  required_replication_columns,
  names(replication_results)
)

if (length(missing_replication_columns) > 0L) {
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

if (length(missing_recovery_columns) > 0L) {
  stop(
    "parameter_recovery_summary.csv is missing required column(s): ",
    paste(missing_recovery_columns, collapse = ", ")
  )
}

usable_results <- replication_results %>%
  dplyr::filter(converged) %>%
  dplyr::filter(
    dplyr::if_all(
      dplyr::all_of(parameter_names),
      is.finite
    )
  )

if (nrow(usable_results) == 0L) {
  stop(
    "No converged replications with finite temporal ACE parameters were found."
  )
}

event_threshold <- stats::median(
  usable_results$z_year[is.finite(usable_results$z_year)],
  na.rm = TRUE
)

if (!is.finite(event_threshold)) {
  stop("Could not identify a finite scaled historical-event threshold.")
}

true_parameters <- recovery_summary %>%
  dplyr::filter(parameter %in% parameter_names) %>%
  dplyr::select(parameter, true_value) %>%
  tidyr::pivot_wider(
    names_from = parameter,
    values_from = true_value
  )

if (
  nrow(true_parameters) != 1L ||
    any(!is.finite(unlist(true_parameters[1, parameter_names], use.names = FALSE)))
) {
  stop(
    "Could not recover one finite true value for every temporal ACE parameter."
  )
}

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

# Summarize the pointwise Monte Carlo distribution while retaining every
# prediction condition used in the figure.
summarize_recovered_trajectories <- function(recovered_trajectories) {
  recovered_trajectories %>%
    dplyr::group_by(
      time,
      historical_period,
      event_experienced,
      event_effect,
      component
    ) %>%
    dplyr::summarise(
      n_replications = dplyr::n(),
      recovered_mean = mean(recovered_variance),
      recovered_median = stats::median(recovered_variance),
      recovered_lower = as.numeric(
        stats::quantile(
          recovered_variance,
          probs = 0.025,
          names = FALSE
        )
      ),
      recovered_upper = as.numeric(
        stats::quantile(
          recovered_variance,
          probs = 0.975,
          names = FALSE
        )
      ),
      .groups = "drop"
    )
}

# Join recovered summaries to the corresponding population predictions and
# apply figure-wide factor ordering and labels.
combine_recovered_and_population <- function(
  recovered_summary,
  population_trajectories,
  central = c("mean", "median")
) {
  central <- match.arg(central)

  recovered_summary %>%
    dplyr::left_join(
      population_trajectories,
      by = c(
        "time",
        "historical_period",
        "event_experienced",
        "event_effect",
        "component"
      )
    ) %>%
    dplyr::mutate(
      recovered_central = if (central == "median") {
        recovered_median
      } else {
        recovered_mean
      },
      component = factor(
        component,
        levels = component_order,
        labels = unname(component_labels[component_order])
      ),
      historical_period_label = factor(
        historical_period,
        levels = c(0L, 1L),
        labels = unname(historical_period_labels)
      ),
      event_experience_label = factor(
        event_experienced,
        levels = c(0L, 1L),
        labels = unname(event_experience_labels)
      )
    )
}

# -----------------------------------------------------------------------------
# Panel A data preparation
# -----------------------------------------------------------------------------

# Panel A shows conditional predictions for both historical periods and both
# event-experience states across the complete time grid. This makes the model
# contrast explicit: the moderator is active only when both indicators equal 1.
panel_a_grid <- tidyr::expand_grid(
  time = time_grid,
  historical_period = c(0L, 1L),
  event_experienced = c(0L, 1L)
)

panel_a_recovered_trajectories <- calculate_variance_trajectories(
  parameter_data = usable_results %>%
    dplyr::select(
      replication,
      dplyr::all_of(parameter_names)
    ),
  prediction_grid = panel_a_grid,
  id_columns = "replication",
  variance_column = "recovered_variance"
)

panel_a_recovered_summary <- summarize_recovered_trajectories(
  panel_a_recovered_trajectories
)

panel_a_population_trajectories <- calculate_variance_trajectories(
  parameter_data = true_parameters,
  prediction_grid = panel_a_grid,
  variance_column = "population_variance"
)

panel_a_data <- combine_recovered_and_population(
  recovered_summary = panel_a_recovered_summary,
  population_trajectories = panel_a_population_trajectories,
  central = central_summary
)

readr::write_csv(
  panel_a_data,
  panel_a_files$data,
  na = ""
)

# -----------------------------------------------------------------------------
# Panel A figure
# -----------------------------------------------------------------------------

panel_a <- ggplot2::ggplot(
  panel_a_data,
  ggplot2::aes(
    x = time,
    color = component,
    fill = component
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = recovered_lower,
      ymax = recovered_upper
    ),
    alpha = 0.20
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      y = recovered_central,
      linetype = "Recovered Monte Carlo estimate"
    ),
    linewidth = 0.9
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      y = population_variance,
      linetype = "Population parameter"
    ),
    linewidth = 0.9
  ) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(event_experience_label),
    cols = ggplot2::vars(historical_period_label)
  ) +
  ggplot2::scale_linetype_manual(
    name = NULL,
    values = c(
      "Recovered Monte Carlo estimate" = "solid",
      "Population parameter" = "dashed"
    )
  ) +
  ggplot2::labs(
    title = "Conditional recovery of time-varying variance components",
    subtitle = paste0(
      "Recovered ",
      central_summary,
      " and empirical 95% Monte Carlo range compared with the population trajectory"
    ),
    x = "Scaled birth year",
    y = "Variance",
    color = "Variance component",
    fill = "Variance component"
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 9),
    strip.text = ggplot2::element_text(face = "bold", size = 9),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = ggplot2::element_text(size = 8)
  )

print(panel_a)

ggsave(
  filename = panel_a_files$png,
  plot = panel_a,
  width = 10.5,
  height = 8,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = panel_a_files$pdf,
  plot = panel_a,
  width = 10.5,
  height = 8,
  device = cairo_pdf,
  bg = "white"
)

# -----------------------------------------------------------------------------
# Panel B data preparation
# -----------------------------------------------------------------------------

# Panel B uses the realized historical period implied by the event threshold.
# Before the event, both experience groups have the same prediction. After the
# event, only the experienced group receives the historical-event moderation.
panel_b_grid <- tidyr::expand_grid(
  time = time_grid,
  event_experienced = c(0L, 1L)
) %>%
  dplyr::mutate(
    historical_period = as.integer(time > event_threshold)
  )

panel_b_recovered_trajectories <- calculate_variance_trajectories(
  parameter_data = usable_results %>%
    dplyr::select(
      replication,
      dplyr::all_of(parameter_names)
    ),
  prediction_grid = panel_b_grid,
  id_columns = "replication",
  variance_column = "recovered_variance"
)

panel_b_recovered_summary <- summarize_recovered_trajectories(
  panel_b_recovered_trajectories
)

panel_b_population_trajectories <- calculate_variance_trajectories(
  parameter_data = true_parameters,
  prediction_grid = panel_b_grid,
  variance_column = "population_variance"
)

panel_b_data <- combine_recovered_and_population(
  recovered_summary = panel_b_recovered_summary,
  population_trajectories = panel_b_population_trajectories,
  central = central_summary
)

readr::write_csv(
  panel_b_data,
  panel_b_files$data,
  na = ""
)

# -----------------------------------------------------------------------------
# Panel B figure
# -----------------------------------------------------------------------------

panel_b <- ggplot2::ggplot(
  panel_b_data,
  ggplot2::aes(
    x = time,
    color = component,
    fill = component
  )
) +
  ggplot2::annotate(
    "rect",
    xmin = event_threshold,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey80",
    alpha = 0.20
  ) +
  ggplot2::geom_vline(
    xintercept = event_threshold,
    linetype = "dotdash",
    linewidth = 0.6
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = recovered_lower,
      ymax = recovered_upper
    ),
    alpha = 0.20
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      y = recovered_central,
      linetype = "Recovered Monte Carlo estimate"
    ),
    linewidth = 0.9
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      y = population_variance,
      linetype = "Population parameter"
    ),
    linewidth = 0.9
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(event_experience_label),
    ncol = 1,
    scales = "free_y"
  ) +
  ggplot2::scale_linetype_manual(
    name = NULL,
    values = c(
      "Recovered Monte Carlo estimate" = "solid",
      "Population parameter" = "dashed"
    )
  ) +
  ggplot2::labs(
    title = "Realized recovery of time-varying variance components",
    subtitle = paste0(
      "Recovered ",
      central_summary,
      " and empirical 95% Monte Carlo range compared with the population trajectory"
    ),
    x = "Scaled birth year",
    y = "Variance",
    color = "Variance component",
    fill = "Variance component"
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 9),
    strip.text = ggplot2::element_text(face = "bold", size = 9),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = ggplot2::element_text(size = 8)
  )

print(panel_b)

ggsave(
  filename = panel_b_files$png,
  plot = panel_b,
  width = 10.5,
  height = 8,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = panel_b_files$pdf,
  plot = panel_b,
  width = 10.5,
  height = 8,
  device = cairo_pdf,
  bg = "white"
)

cat("Figure data and files written to:\n")
cat(
  "  Panel A data: ",
  normalizePath(panel_a_files$data, mustWork = FALSE),
  "\n",
  sep = ""
)
cat(
  "  Panel A PNG:  ",
  normalizePath(panel_a_files$png, mustWork = FALSE),
  "\n",
  sep = ""
)
cat(
  "  Panel A PDF:  ",
  normalizePath(panel_a_files$pdf, mustWork = FALSE),
  "\n",
  sep = ""
)
cat(
  "  Panel B data: ",
  normalizePath(panel_b_files$data, mustWork = FALSE),
  "\n",
  sep = ""
)
cat(
  "  Panel B PNG:  ",
  normalizePath(panel_b_files$png, mustWork = FALSE),
  "\n",
  sep = ""
)
cat(
  "  Panel B PDF:  ",
  normalizePath(panel_b_files$pdf, mustWork = FALSE),
  "\n",
  sep = ""
)
