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
core_folder <- "temporal_ACE_parameter_recovery_500_p50"
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
panel_b1_files <- make_panel_files("B1")
panel_b2_files <- make_panel_files("B2")
# -----------------------------------------------------------------------------
# Figure settings and notation
# -----------------------------------------------------------------------------

time_grid <- seq(-3, 3, length.out = 301)
central_summary <- "mean"

# parameter_names, component_order, component_labels, historical_period_labels,
# event_experience_labels, and calculate_variance_trajectories() all live here
# so that the longevity-panel scripts can reuse the same variance math.
trajectory_helpers <- c(
  "data-raw/temporal_variance_trajectories.R",
  "temporal_variance_trajectories.R"
)
trajectory_helpers <- trajectory_helpers[file.exists(trajectory_helpers)]

if (length(trajectory_helpers) == 0L) {
  stop(
    "Could not find temporal_variance_trajectories.R. ",
    "Run this script from the package root or from data-raw/."
  )
}

source(trajectory_helpers[1])

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

# calculate_variance_trajectories() is defined in
# data-raw/temporal_variance_trajectories.R, sourced above.

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
panel_a <- panel_a_data %>%
 filter(event_experienced == TRUE) %>%
  ggplot(
  aes(
    x = time,
    color = component,
    fill = component
  )
) +
  geom_ribbon(
    aes(
      ymin = recovered_lower,
      ymax = recovered_upper
    ),
    alpha = 0.20
  ) +
  geom_line(
    aes(
      y = recovered_central,
      linetype = "Recovered Monte Carlo estimate"
    ),
    linewidth = 0.9
  ) +
  geom_line(
    aes(
      y = population_variance,
      linetype = "Population parameter"
    ),
    linewidth = 0.9
  ) +
  facet_grid(
    rows = vars(event_experience_label),
    cols = vars(historical_period_label)
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Recovered Monte Carlo estimate" = "solid",
      "Population parameter" = "dashed"
    )
  ) +
  labs(
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

ggsave(
  filename = panel_a_files$png,
  plot = panel_a,
  width = 10.5,
  height = 10.5,
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

panel_b_core <- ggplot(
  panel_b_data,
  aes(
    x = time,
    color = component,
    fill = component
  )
) +
  annotate(
    "rect",
    xmin = event_threshold,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey80",
    alpha = 0.20
  ) +
  geom_vline(
    xintercept = event_threshold,
    linetype = "dotdash",
    linewidth = 0.6
  )

panel_b_event_facet <- panel_b_core +
  geom_ribbon(
    aes(
      ymin = recovered_lower,
      ymax = recovered_upper,
      group=NULL
    ),
    alpha = 0.20
  ) +
  geom_line(
    aes(
      y = recovered_central,
      linetype = "Recovered Monte Carlo estimate"
    ),
    linewidth = 0.9
  ) +
  geom_line(
    aes(
      y = population_variance,
      linetype = "Population parameter",
      group=NULL
    ),
    linewidth = 0.9
  )   +
  facet_wrap(
    vars( event_experience_label),
    ncol = 1,
    scales = "free_y"
  )

panel_b_event_combined <- panel_b_core +
  geom_ribbon(data = panel_b_data %>% filter(event_experienced==0),
    #historical_period ==0&event_experienced==0|
    #historical_period ==1&event_experienced==0),
    aes(
      ymin = recovered_lower,
      ymax = recovered_upper,
      group=NULL
    ),
    alpha = 0.20
  ) +
    geom_ribbon(data = panel_b_data %>% filter(
    historical_period ==1&event_experienced==1|
      historical_period==0),
    aes(
      ymin = recovered_lower,
      ymax = recovered_upper,
      group=NULL
    ),
    alpha = 0.20
  ) +
  geom_line(data = panel_b_data %>% filter(event_experienced==0),
    aes(
      y = recovered_central,
      linetype = "Recovered Monte Carlo estimate"
    ),
    linewidth = 0.9
  ) +
  geom_line(data = panel_b_data %>% filter(historical_period ==1&event_experienced==1|
      historical_period==0),
    aes(
      y = recovered_central,
      linetype = "Recovered Monte Carlo estimate",
      group=NULL
    ),
    linewidth = 0.9
  )   +
  geom_line(data = panel_b_data %>% filter(event_experienced==0),
    aes(
      y = population_variance,
      linetype = "Population parameter",
      group=NULL
    ),
    linewidth = 0.9
  )    +
  geom_line(data = panel_b_data %>% filter(historical_period ==1&event_experienced==1|
      historical_period==0),
    aes(
      y = population_variance,
      linetype = "Population parameter",
      group=NULL
    ),
    linewidth = 0.9
  ) +
  facet_wrap(
    vars(component),
    #  event_experience_label),
    ncol = 1,
    scales = "free_y"
  )

# list prevents ggplot warning
ggstyling_panel_b <- list(
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Recovered Monte Carlo estimate" = "solid",
      "Population parameter" = "dashed"
    )
  ),
  labs(
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
  ),
  theme_bw(base_size = 11),
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9),
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 8)
  )
)

panel_b_v1 <- panel_b_event_combined + ggstyling_panel_b
panel_b_v2  <- panel_b_event_facet + ggstyling_panel_b

panel_b <- panel_b_v2
print(panel_b)

ggsave(
  filename = panel_b1_files$png,
  plot = panel_b_v1,
  width = 10.5,
  height = 8,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = panel_b1_files$pdf,
  plot = panel_b_v1,
  width = 10.5,
  height = 8,
  device = cairo_pdf,
  bg = "white"
)


ggsave(
  filename = panel_b2_files$png,
  plot = panel_b_v2,
  width = 10.5,
  height = 8,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = panel_b2_files$pdf,
  plot = panel_b_v2,
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
  normalizePath(panel_b1_files$png, mustWork = FALSE),
  "\n",
  sep = ""
)
cat(
  "  Panel B PDF:  ",
  normalizePath(panel_b1_files$pdf, mustWork = FALSE),
  "\n",
  sep = ""
)
cat(
  "  Panel B2 PNG:  ",
  normalizePath(panel_b2_files$png, mustWork = FALSE),
  "\n",
  sep = ""
)
cat(
  "  Panel B2 PDF:  ",
  normalizePath(panel_b2_files$pdf, mustWork = FALSE),
  "\n",
  sep = ""
)
