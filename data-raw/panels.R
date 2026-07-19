
# temporal_pair_panels.R
#
# Generate six separate figure panels that show how two individuals' temporal
# positions enter a temporally moderated additive-genetic covariance model.
#
# The script can be:
#   1. sourced after temporal_BGmisc_AE_parameter_recovery.R, or
#   2. run from the project root in a fresh R session.
#
# It simulates ONE illustrative pedigree only. It does not rerun the Monte Carlo
# study. For Panel 6, it uses compact_recovery_plot when that object already
# exists; otherwise it reads parameter_recovery_summary.csv when available.

# -----------------------------------------------------------------------------
# Package setup
# -----------------------------------------------------------------------------

required_packages <- c(
  "ggplot2", "dplyr", "tidyr", "tibble", "readr", "patchwork", "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall them before running this script."
  )
}

library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(patchwork)
library(scales)

# -----------------------------------------------------------------------------
# Settings and fallbacks
# -----------------------------------------------------------------------------

# These defaults match the parameter-recovery script. Existing objects in the
# current R session take precedence.
figure_seed <- get0("master_seed", ifnotfound = 11202601L) + 9000L
n_display <- 10L

kpc_figure <- get0("kpc", ifnotfound = 4L)
Ngen_figure <- get0("Ngen", ifnotfound = 4L)
marR_figure <- get0("marR", ifnotfound = 0.8)
threshold_year_figure <- get0("threshold_year", ifnotfound = 1750)
birth_year_sd_figure <- get0("birth_year_sd", ifnotfound = 12)
birth_year_base_figure <- get0("birth_year_base", ifnotfound = 1700)
gen_gap_figure <- get0("gen_gap", ifnotfound = 30)
prop_historical_figure <- get0("prop_historical", ifnotfound = 0.5)
loading_link_figure <- get0("loading_link", ifnotfound = "exp")
components_figure <- get0("sim_components", ifnotfound = c("a", "cn", "e"))

if (!exists("true_beta", inherits = TRUE)) {
  true_beta <- list(
    a  = c(log(2), 0.1, -0.1, 0.00),
    cn = c(log(1.5), 0.00, 0.00, 0.00),
    ce = c(0.00, 0.00, 0.00, 0.00),
    mt = c(0.00, 0.00, 0.00, 0.00),
    e  = c(log(2.0), 0.0, 0.00, 0.00)
  )
}

if (!exists("true_gamma", inherits = TRUE)) {
  true_gamma <- list(
    a  = 0.2,
    cn = 0.00,
    ce = 0.00,
    mt = 0.00,
    e  = 0.0
  )
}

# The simulator is defined in smoketest_helpers.R in the existing project.
if (!exists("simulate_temporal_family", mode = "function", inherits = TRUE)) {
  helper_file <- file.path("data-raw", "smoketest_helpers.R")

  if (!file.exists(helper_file)) {
    stop(
      "Could not find simulate_temporal_family().\n",
      "Run this script from the project root, or source the main Monte Carlo ",
      "script first. Expected helper file: ", helper_file
    )
  }

  source(helper_file)
}

base_output_directory <- get0(
  "output_directory",
  ifnotfound = file.path("results", "temporal_pair_figure")
)

panel_output_directory <- file.path(
  base_output_directory,
  "temporal_pair_panels"
)

dir.create(panel_output_directory, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# General helpers
# -----------------------------------------------------------------------------

first_existing_vector <- function(x, candidates, expected_length) {
  for (nm in candidates) {
    value <- x[[nm]]

    if (
      !is.null(value) &&
      is.atomic(value) &&
      length(value) == expected_length &&
      all(is.finite(as.numeric(value)))
    ) {
      return(as.numeric(value))
    }
  }

  NULL
}

matrix_to_long <- function(x) {
  stopifnot(is.matrix(x), nrow(x) == ncol(x))

  n <- nrow(x)

  tidyr::expand_grid(
    row = seq_len(n),
    column = seq_len(n)
  ) %>%
    mutate(value = x[cbind(row, column)])
}

calculate_lambda <- function(time, historical, beta, gamma, link = "exp") {
  beta <- as.numeric(beta)

  if (length(beta) < 4L) {
    beta <- c(beta, rep(0, 4L - length(beta)))
  }

  beta <- beta[seq_len(4L)]

  design <- cbind(
    1,
    time,
    time^2,
    time^3
  )

  eta <- as.vector(design %*% beta) + as.numeric(gamma)[1] * historical

  if (identical(link, "exp")) {
    exp(eta)
  } else if (identical(link, "identity")) {
    eta
  } else {
    stop("Unsupported loading link: ", link)
  }
}

find_temporal_contrast <- function(A, time, historical) {
  stopifnot(is.matrix(A), nrow(A) == length(time))

  pair_index <- which(upper.tri(A), arr.ind = TRUE)

  pair_data <- tibble(
    i = pair_index[, 1],
    j = pair_index[, 2],
    relatedness = A[pair_index],
    time_i = time[i],
    time_j = time[j],
    historical_i = historical[i],
    historical_j = historical[j]
  ) %>%
    mutate(
      time_gap = abs(time_j - time_i),
      relatedness_key = round(relatedness, 8),
      historical_contrast = historical_i != historical_j
    ) %>%
    filter(
      is.finite(relatedness),
      relatedness > 0,
      relatedness < 1
    )

  repeated_classes <- pair_data %>%
    group_by(relatedness_key) %>%
    summarise(
      n_pairs = n(),
      time_gap_range = max(time_gap) - min(time_gap),
      has_historical_contrast = any(historical_contrast),
      has_same_historical_status = any(!historical_contrast),
      .groups = "drop"
    ) %>%
    filter(n_pairs >= 2L) %>%
    arrange(
      desc(has_historical_contrast & has_same_historical_status),
      desc(time_gap_range),
      relatedness_key
    )

  if (nrow(repeated_classes) == 0L) {
    return(NULL)
  }

  chosen_key <- repeated_classes$relatedness_key[1]

  candidates <- pair_data %>%
    filter(relatedness_key == chosen_key)

  cross_pool <- candidates %>%
    filter(historical_contrast)

  if (nrow(cross_pool) == 0L) {
    cross_pool <- candidates
  }

  cross_pair <- cross_pool %>%
    arrange(desc(time_gap)) %>%
    slice(1)

  same_pool <- candidates %>%
    filter(!historical_contrast)

  if (nrow(same_pool) == 0L) {
    same_pool <- candidates
  }

  # Prefer a comparison pair that does not reuse either member of the first pair.
  disjoint_same_pool <- same_pool %>%
    filter(
      !(i %in% c(cross_pair$i, cross_pair$j)),
      !(j %in% c(cross_pair$i, cross_pair$j))
    )

  if (nrow(disjoint_same_pool) > 0L) {
    same_pool <- disjoint_same_pool
  }

  same_pair <- same_pool %>%
    arrange(time_gap) %>%
    slice(1)

  if (
    same_pair$i == cross_pair$i &&
    same_pair$j == cross_pair$j &&
    nrow(candidates) > 1L
  ) {
    same_pair <- candidates %>%
      filter(!(i == cross_pair$i & j == cross_pair$j)) %>%
      arrange(time_gap) %>%
      slice(1)
  }

  list(
    relatedness = chosen_key,
    cross_pair = cross_pair,
    same_pair = same_pair,
    all_candidates = candidates
  )
}

select_display_members <- function(n_total, required_members, n_keep) {
  n_keep <- min(as.integer(n_keep), n_total)
  required_members <- sort(unique(required_members))

  if (length(required_members) > n_keep) {
    stop("n_display is too small to retain both highlighted dyads.")
  }

  evenly_spaced <- unique(
    pmax(
      1L,
      pmin(
        n_total,
        as.integer(round(seq(1, n_total, length.out = n_keep)))
      )
    )
  )

  selected <- unique(c(required_members, evenly_spaced))

  if (length(selected) < n_keep) {
    selected <- c(
      selected,
      setdiff(seq_len(n_total), selected)[
        seq_len(n_keep - length(selected))
      ]
    )
  }

  if (length(selected) > n_keep) {
    selected <- c(
      required_members,
      setdiff(selected, required_members)[
        seq_len(n_keep - length(required_members))
      ]
    )
  }

  sort(unique(selected))
}

make_h_top_strip <- function(historical) {
  tibble(
    x = seq_along(historical),
    y = 1,
    historical = factor(historical, levels = c(0, 1))
  ) %>%
    ggplot(aes(x = x, y = y, fill = historical)) +
    geom_tile(colour = "grey40", linewidth = 0.25) +
    scale_fill_manual(values = c(`0` = "white", `1` = "#0B3B70")) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    coord_equal() +
    theme_void() +
    theme(legend.position = "none")
}

make_h_left_strip <- function(historical) {
  n <- length(historical)

  tibble(
    x = 1,
    y = seq_len(n),
    historical = factor(historical, levels = c(0, 1))
  ) %>%
    ggplot(aes(x = x, y = y, fill = historical)) +
    geom_tile(colour = "grey40", linewidth = 0.25) +
    scale_fill_manual(values = c(`0` = "white", `1` = "#0B3B70")) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_reverse(limits = c(n + 0.5, 0.5), expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    coord_equal() +
    theme_void() +
    theme(legend.position = "none")
}

add_historical_strips <- function(
  main_plot,
  historical,
  title,
  subtitle,
  caption,
  person_labels,
  strip_thickness = 0.40,
  strip_center = 0.25
) {
	n <- length(historical)
   strip_data <- tibble(
    position = seq_len(n),
    historical = as.integer(historical)
  )

  main_plot +

    # Top history strip: one tile aligned with each matrix column.
    geom_tile(
      data = strip_data %>% filter(historical == 0L),
      aes(x = position, y = strip_center),
      inherit.aes = FALSE,
      width = 1,
      height = strip_thickness,
      fill = "white",
      colour = "grey40",
      linewidth = 0.25
    ) +
    geom_tile(
      data = strip_data %>% filter(historical == 1L),
      aes(x = position, y = strip_center),
      inherit.aes = FALSE,
      width = 1,
      height = strip_thickness,
      fill = "#0B3B70",
      colour = "grey40",
      linewidth = 0.25
    ) +

    # Left history strip: one tile aligned with each matrix row.
    geom_tile(
      data = strip_data %>% filter(historical == 0L),
      aes(x = strip_center, y = position),
      inherit.aes = FALSE,
      width = strip_thickness,
      height = 1,
      fill = "white",
      colour = "grey40",
      linewidth = 0.25
    ) +
    geom_tile(
      data = strip_data %>% filter(historical == 1L),
      aes(x = strip_center, y = position),
      inherit.aes = FALSE,
      width = strip_thickness,
      height = 1,
      fill = "#0B3B70",
      colour = "grey40",
      linewidth = 0.25
    ) +

    # These scales include a half-cell margin for the history strips.
    scale_x_continuous(
      breaks = seq_len(n),
      labels = person_labels,
      limits = c(0, n + 0.5),
      expand = c(0, 0)
    ) +
    scale_y_reverse(
      breaks = seq_len(n),
      labels = person_labels,
      limits = c(n + 0.5, 0),
      expand = c(0, 0)
    ) +

    coord_equal(clip = "off") +

    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +

    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9),
      plot.caption = element_text(size = 8, hjust = 0)
    )  +

    # Label for both historical-status strips
    annotate(
      "text",
      x = strip_center,
      y = strip_center,
      label = "italic(H)",
      parse = TRUE,
      fontface = "bold",
      size = 4
    )
}

save_panel <- function(plot_object, filename, width, height) {
  ggplot2::ggsave(
    filename = file.path(panel_output_directory, filename),
    plot = plot_object,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

# -----------------------------------------------------------------------------
# Simulate one illustrative pedigree and choose two equal-relatedness dyads
# -----------------------------------------------------------------------------

simulate_figure_family <- function(seed) {
  set.seed(seed)

  simulate_temporal_family(
    kpc = kpc_figure,
    Ngen = Ngen_figure,
    marR = marR_figure,
    threshold_year = threshold_year_figure,
    true_beta = true_beta,
    true_gamma = true_gamma,
    components = components_figure,
    gen_gap = gen_gap_figure,
    birth_year_sd = birth_year_sd_figure,
    birth_year_base = birth_year_base_figure,
    family_id = seed,
    poly = 3,
    rescale = TRUE,
    loading_link = loading_link_figure,
    time_scale = "fixed",
    time_half_range = 3,
    prop_historical = prop_historical_figure
  )
}

figure_family <- NULL
contrast <- NULL
ordered_family <- NULL

for (attempt in 0:60) {
  candidate <- simulate_figure_family(figure_seed + attempt)

  time_candidate <- as.numeric(candidate$birth_year_scaled)
  historical_candidate <- as.integer(candidate$H)
  A_candidate <- as.matrix(candidate$A)

  order_candidate <- order(time_candidate, seq_along(time_candidate))

  A_ordered <- A_candidate[order_candidate, order_candidate, drop = FALSE]
  time_ordered <- time_candidate[order_candidate]
  historical_ordered <- historical_candidate[order_candidate]

  candidate_contrast <- find_temporal_contrast(
    A = A_ordered,
    time = time_ordered,
    historical = historical_ordered
  )

  if (
    !is.null(candidate_contrast) &&
    length(unique(historical_ordered)) >= 2L
  ) {
    figure_family <- candidate
    contrast <- candidate_contrast
    ordered_family <- list(
      order = order_candidate,
      A = A_ordered,
      time = time_ordered,
      historical = historical_ordered
    )
    break
  }
}

if (is.null(figure_family) || is.null(contrast)) {
  stop(
    "Could not find one simulated pedigree containing two equal-relatedness ",
    "dyads with sufficiently different temporal positions after 61 attempts."
  )
}

required_members <- unique(c(
  contrast$cross_pair$i,
  contrast$cross_pair$j,
  contrast$same_pair$i,
  contrast$same_pair$j
))

selected_members <- select_display_members(
  n_total = nrow(ordered_family$A),
  required_members = required_members,
  n_keep = n_display
)

A_display <- ordered_family$A[
  selected_members,
  selected_members,
  drop = FALSE
]

time_display <- ordered_family$time[selected_members]
historical_display <- ordered_family$historical[selected_members]

# Use actual birth years on the axes when the simulator supplies them.
raw_birth_year <- first_existing_vector(
  figure_family,
  candidates = c(
    "birth_year", "birth_year_raw", "birth_year_unscaled", "year_birth"
  ),
  expected_length = length(ordered_family$order)
)

if (!is.null(raw_birth_year)) {
  raw_birth_year <- raw_birth_year[ordered_family$order][selected_members]
  person_labels <- as.character(round(raw_birth_year))
  axis_description <- "Birth year"
} else {
  person_labels <- paste0("P", seq_along(selected_members))
  axis_description <- "Birth-order position"
}

member_map <- stats::setNames(
  seq_along(selected_members),
  as.character(selected_members)
)

map_pair_to_display <- function(pair_row) {
  c(
    i = unname(member_map[as.character(pair_row$i)]),
    j = unname(member_map[as.character(pair_row$j)])
  )
}

cross_position <- map_pair_to_display(contrast$cross_pair)
same_position <- map_pair_to_display(contrast$same_pair)

if (any(!is.finite(c(cross_position, same_position)))) {
  stop("Internal error: highlighted dyads were not retained in the display set.")
}

lambda_a <- calculate_lambda(
  time = time_display,
  historical = historical_display,
  beta = true_beta$a,
  gamma = true_gamma$a,
  link = loading_link_figure
)

T_a <- outer(lambda_a, lambda_a)
V_a <- A_display * T_a

n <- length(time_display)

highlight_upper <- bind_rows(
  tibble(
    row = cross_position["i"],
    column = cross_position["j"],
    dyad = "Cross-period relatives"
  ),
  tibble(
    row = same_position["i"],
    column = same_position["j"],
    dyad = "Same-period relatives"
  )
)

highlight_lower <- bind_rows(
  tibble(
    row = cross_position["j"],
    column = cross_position["i"],
    dyad = "Cross-period relatives"
  ),
  tibble(
    row = same_position["j"],
    column = same_position["i"],
    dyad = "Same-period relatives"
  )
)

highlight_mirrored <- bind_rows(
  highlight_upper,
  highlight_lower
)

#highlight_mirrored <- bind_rows(
#  highlight_upper,
#  highlight_upper %>% transmute(row = column, column = row, dyad = dyad)
#)

highlight_colours <- c(
  "Cross-period relatives" = "#C49A00",
  "Same-period relatives" = "#B2182B"
)


# -----
# Panel 0: Full model
# ------------------------------------------------------------------------

# The Hadamard (elementwise) ring is built from an ASCII-only source so the
# script stays portable across locales; the rendered figure shows the real
# symbol. These equations are the FULL temporally moderated model from the
# grant manuscript (Eq. 2): the six-component covariance decomposition, the
# pairwise construction of each component, and the individual loading function.
hadamard_ring <- intToUtf8(8728)
Encoding(hadamard_ring) <- "UTF-8"

# One moderated component term, e.g. (A o a^2(t, H)).
panel_0_term <- function(relatedness, component) {
  paste0("(", relatedness, " * '", hadamard_ring, "' * ", component, "^2*(list(t,H)))")
}

# Full covariance decomposition, wrapped across two lines (all six components).
panel_0_eq_cov_line1 <- paste0(
  "Cov(bold(p)) == ",
  panel_0_term("A", "a"), " + ",
  panel_0_term("D", "d"), " + ",
  panel_0_term("C[N]", "c[N]")
)
panel_0_eq_cov_line2 <- paste0(
  "phantom(Cov(bold(p)) == 0) + ",
  panel_0_term("C[E]", "c[E]"), " + ",
  panel_0_term("M", "mt"), " + ",
  panel_0_term("E", "e")
)


# Individual loading function (birth-cohort polynomial plus historical shifts).
panel_0_eq_loading <- paste0(
  "k[i](list(t[i],H[i])) == beta[k0] + beta[k1]*t[i] + beta[k2]*t[i]^2 + ",
  "beta[k3]*t[i]^3 + sum(gamma[km]*H[mi], m==1, p)"
)


panel_0_eq_loading2 <- paste0("lambda[Ai] == exp(beta[A0] + beta[A1]*t[i] + beta[A2]*t[i]^2 + ", "beta[A3]*t[i]^3 + gamma[A]*H[i])")

# Component matrix definition: k^2(t, H) = [k_ij^2(t, H)]_{i,j=1}^n
#k^2 (t,H)=[k_ij^2 (t,H)]_(i,j=1)^n
panel_0_eq_kij2 <-
  "bold(k)^2*(list(t,H)) == group('[', k[ij]^2*(list(t,H)), ']')[list(i,j)==1]^n"

# Scalar per-pair covariance (manuscript): expected covariance of individuals
# i and j, wrapped across two lines so the six terms fit the panel width.
# Cov⁡(p_i,p_j )=A_ij a_ij^2 (t,H)+D_ij d_ij^2 (t,H)+C_Nij c_Nij^2 (t,H)+C_Eij c_Eij^2 (t,H)+M_ij mt_ij^2 (t,H)+E_ij e_ij^2 (t,H)



panel_0_eq_pipj_line1 <- paste0(
  "Cov(list(p[i],p[j])) == A[ij]*a[ij]^2*(list(t,H)) + ",
  "D[ij]*d[ij]^2*(list(t,H)) + C[Nij]*c[Nij]^2*(list(t,H))"
)
panel_0_eq_pipj_line2 <- paste0(
  "phantom(Cov(list(p[i],p[j])) == 0) + C[Eij]*c[Eij]^2*(list(t,H)) + ",
  "M[ij]*mt[ij]^2*(list(t,H)) + E[ij]*e[ij]^2*(list(t,H))"
)

# Each component matrix is built pairwise from individual temporal loadings.
panel_0_eq_pairwise <-
  "k[ij]^2*(list(t,H)) == k[i](list(t[i],H[i])) %.% k[j](list(t[j],H[j]))"


panel_0_eq_covariance <- paste0("V[A] == A * '", hadamard_ring, "' * T[A]")
panel_0_eq_weight <- "T[A] == lambda[Ai] %*% lambda[Aj]"

# Draw one equation line, left-aligned, at height y.
panel_0_line <- function(y, label, size = 4.2) {
  annotate(
    "text", x = 0.02, y = y, label = label,
    parse = TRUE, hjust = 0, size = size, fontface = "bold"
  )
}

panel_0 <- ggplot() +
  # General model (manuscript order): full matrix decomposition, the component
  # matrix definition, the scalar per-pair covariance, the pairwise
  # construction, and the general individual loading function.
  panel_0_line(0.960, panel_0_eq_cov_line1, size = 4.4) +
  panel_0_line(0.905, panel_0_eq_cov_line2, size = 4.4) +
  panel_0_line(0.800, panel_0_eq_kij2) +
  panel_0_line(0.705, panel_0_eq_pipj_line1) +
  panel_0_line(0.650, panel_0_eq_pipj_line2) +
  panel_0_line(0.545, panel_0_eq_pairwise) +
  panel_0_line(0.450, panel_0_eq_loading) +
  # Additive specialization traced through panels 1-5 (exp link, as coded):
  # individual loading, pairwise temporal weight, moderated covariance.
  panel_0_line(0.320, panel_0_eq_loading2) +
  panel_0_line(0.225, panel_0_eq_weight) +
  panel_0_line(0.130, panel_0_eq_covariance) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.07, 1.0), expand = c(0, 0)) +
  labs(
    title = "0. Full temporally moderated biometric model"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    plot.margin = margin(10, 12, 10, 12)
  )


# -----------------------------------------------------------------------------
# Panel 1: additive relatedness matrix
# -----------------------------------------------------------------------------

panel_1_data <- matrix_to_long(A_display)

panel_1 <- ggplot(panel_1_data, aes(x = column, y = row, fill = value)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  geom_point(
    data = highlight_upper,
    aes(x = column, y = row, colour = dyad),
    inherit.aes = FALSE,
    shape = 22,
    fill = NA,
    size = 6,
    stroke = 1.3
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#0B3B70",
    name = expression(A[ij])
  ) +
  scale_colour_manual(values = highlight_colours, name = NULL) +
  scale_x_continuous(
    breaks = seq_len(n),
    labels = person_labels,
    expand = c(0, 0)
  ) +
  scale_y_reverse(
    breaks = seq_len(n),
    labels = person_labels,
    limits = c(n + 0.5, 0.5),
    expand = c(0, 0)
  ) +
  coord_equal() +
  labs(
    title = "1. Additive relatedness matrix A",
    subtitle = paste0(
      "Pedigree only; individuals are ordered from earlier to later ",
      tolower(axis_description), "."
    ),
    x = axis_description,
    y = axis_description,
    caption = paste0(
      "The highlighted dyads have the same expected additive relatedness: Aij = ",
      formatC(contrast$relatedness, format = "f", digits = 5), "."
    )
  ) +
  theme_bw(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9),
    plot.caption = element_text(size = 8, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# -----------------------------------------------------------------------------
# Panel 2: two-person temporal inputs above and below the diagonal
# -----------------------------------------------------------------------------

# Every row contains that row person's time value. Consequently, for a pair
# i < j, cell (i, j) above the diagonal contains t_i, whereas mirrored cell
# (j, i) below the diagonal contains t_j.
time_input_matrix <- outer(time_display, rep(1, n))
diag(time_input_matrix) <- NA_real_

panel_2_data <- matrix_to_long(time_input_matrix) %>%
  mutate(
    label = ifelse(is.finite(value), sprintf("%.1f", value), "")
  )

panel_2_main <- ggplot(
  panel_2_data,
  aes(x = column, y = row, fill = value)
) +
  geom_tile(colour = "white", linewidth = 0.35) +
  geom_text(aes(label = label), size = 2.7) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.45) +
  geom_point(
    data = highlight_mirrored,
    aes(x = column, y = row, colour = dyad),
    inherit.aes = FALSE,
    shape = 22,
    fill = NA,
    size = 5.4,
    stroke = 1.25
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    name = "Time t"
  ) +
  scale_colour_manual(values = highlight_colours, guide = "none") +
  scale_x_continuous(
    breaks = seq_len(n),
    labels = person_labels,
    expand = c(0, 0)
  ) +
  scale_y_reverse(
    breaks = seq_len(n),
    labels = person_labels,
    limits = c(n + 0.5, 0.5),
    expand = c(0, 0)
  ) +
  coord_equal() +
  labs(x = axis_description, y = axis_description) +
  theme_bw(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

panel_2 <- add_historical_strips(
  main_plot = panel_2_main,
  historical = historical_display,
  title = "2. Two-person temporal inputs",
  subtitle = paste0(
    "Above the diagonal: the earlier member's t. Below the diagonal: ",
    "the later member's t."
  ),
  caption = paste0(
    "Each dyad occupies two mirrored cells, so the two people retain separate ",
    "temporal values.\\\n Margin strips show H (white = 0; navy = 1)."
  ),
  person_labels = person_labels
)

# -----------------------------------------------------------------------------
# Panel 3: pairwise temporal weight T_A = lambda_Ai * lambda_Aj
# -----------------------------------------------------------------------------

panel_3_data <- matrix_to_long(T_a)

panel_3_labels <- highlight_upper %>%
  mutate(
    value = T_a[cbind(row, column)],
    label = sprintf("%.2f", value)
  )

panel_3_main <- ggplot(
  panel_3_data,
  aes(x = column, y = row, fill = value)
) +
  geom_tile(colour = "white", linewidth = 0.35) +
  geom_point(
    data = highlight_upper,
    aes(x = column, y = row, colour = dyad),
    inherit.aes = FALSE,
    shape = 22,
    fill = NA,
    size = 6,
    stroke = 1.3
  ) +
  geom_text(
    data = panel_3_labels,
    aes(x = column, y = row, label = label),
    inherit.aes = FALSE,
    size = 3,
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#D7301F",
    name = expression(T[A * "," * ij])
  ) +
  scale_colour_manual(values = highlight_colours, guide = "none") +
  scale_x_continuous(
    breaks = seq_len(n),
    labels = person_labels,
    expand = c(0, 0)
  ) +
  scale_y_reverse(
    breaks = seq_len(n),
    labels = person_labels,
    limits = c(n + 0.5, 0.5),
    expand = c(0, 0)
  ) +
  coord_equal() +
  labs(x = axis_description, y = axis_description) +
  theme_bw(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

panel_3 <- add_historical_strips(
  main_plot = panel_3_main,
  historical = historical_display,
  title = "3. Pairwise temporal weight T_A = lambda_Ai x lambda_Aj",
  subtitle = paste0(
    "lambda_Ai = exp(b_A0 + b_A1 t_i + b_A2 t_i^2 + ",
    "b_A3 t_i^3 + g_A H_i)"
  ),
  caption = paste0(
    "The two individual loadings are calculated separately, then multiplied ",
    "to form one symmetric pair-specific temporal weight."
  ),
  person_labels = person_labels
)

# -----------------------------------------------------------------------------
# Panel 4: worked comparison of the two highlighted dyads
# -----------------------------------------------------------------------------

summarise_dyad <- function(position, label) {
  i <- unname(position["i"])
  j <- unname(position["j"])

  tibble(
    dyad = label,
    member_i = person_labels[i],
    member_j = person_labels[j],
    t_i = time_display[i],
    t_j = time_display[j],
    H_i = historical_display[i],
    H_j = historical_display[j],
    lambda_i = lambda_a[i],
    lambda_j = lambda_a[j],
    A_ij = A_display[i, j],
    T_ij = T_a[i, j],
    V_ij = V_a[i, j]
  )
}

dyad_comparison <- bind_rows(
  summarise_dyad(cross_position, "Cross-period relatives"),
  summarise_dyad(same_position, "Same-period relatives")
)

readr::write_csv(
  dyad_comparison,
  file.path(panel_output_directory, "highlighted_dyad_values.csv")
)

comparison_rows <- c(
  "Pair members",
  "Time values (t_i, t_j)",
  "Historical exposure (H_i, H_j)",
  "Indiv. loadings (lambda_i, lambda_j)",
  "Additive relatedness A_ij",
  "Temporal weight T_A,ij",
  "Moderated covariance V_A,ij"
)

format_dyad_column <- function(x) {
  c(
    paste0(x$member_i, " and ", x$member_j),
    sprintf("%.2f, %.2f", x$t_i, x$t_j),
    sprintf("%d, %d", x$H_i, x$H_j),
    sprintf("%.3f, %.3f", x$lambda_i, x$lambda_j),
    sprintf("%.5f", x$A_ij),
    sprintf("%.3f", x$T_ij),
    sprintf("%.3f", x$V_ij)
  )
}

cross_values <- format_dyad_column(dyad_comparison[1, ])
same_values <- format_dyad_column(dyad_comparison[2, ])

panel_4_table <- bind_rows(
  tibble(
    x = 1,
    y = seq_along(comparison_rows),
    text = comparison_rows,
    cell_type = "Row label"
  ),
  tibble(
    x = 2,
    y = seq_along(comparison_rows),
    text = cross_values,
    cell_type = "Cross-period relatives"
  ),
  tibble(
    x = 3,
    y = seq_along(comparison_rows),
    text = same_values,
    cell_type = "Same-period relatives"
  )
)

panel_4_headers <- tibble(
  x = 1:3,
  y = 0,
  text = c(
    "Quantity",
    "Cross-period relatives",
    "Same-period relatives"
  ),
  cell_type = c(
    "Header",
    "Cross header",
    "Same header"
  )
)

panel_4 <- ggplot() +
  geom_tile(
    data = panel_4_table,
    aes(x = x, y = y, fill = cell_type),
    colour = "grey55",
    linewidth = 0.4,
    width = 1,
    height = 1
  ) +
  geom_tile(
    data = panel_4_headers,
    aes(x = x, y = y, fill = cell_type),
    colour = "grey40",
    linewidth = 0.5,
    width = 1,
    height = 1
  ) +
  geom_text(
    data = panel_4_table,
    aes(x = x, y = y, label = text),
    size = 3,
    lineheight = 0.9
  ) +
  geom_text(
    data = panel_4_headers,
    aes(x = x, y = y, label = text),
    size = 3.2,
    fontface = "bold",
    lineheight = 0.9
  ) +
  annotate(
    "text",
    x = 2,
    y = length(comparison_rows) + 1.05,
    label = paste0(
      "Same A_ij, but different individual temporal inputs produce ",
      "different covariances."
    ),
    size = 3.5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Row label" = "#F2F2F2",
      "Cross-period relatives" = "#FDE0DD",
      "Same-period relatives" = "#FFF2CC",
      "Header" = "#D9E2F3",
      "Cross header" = "#F4B6AE",
      "Same header" = "#F4DE88"
    ),
    guide = "none"
  ) +
  scale_x_continuous(limits = c(0.5, 3.5), expand = c(0, 0)) +
  scale_y_reverse(
    limits = c(length(comparison_rows) + 1.5, -0.5),
    expand = c(0, 0)
  ) +
  labs(
    title = "4. Worked comparison of two equal-relatedness dyads"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.margin = margin(8, 8, 8, 8)
  )

# -----------------------------------------------------------------------------
# Panel 5: temporally moderated additive covariance V_A = A o T_A
# -----------------------------------------------------------------------------

panel_5_data <- matrix_to_long(V_a)

panel_5_labels <- highlight_upper %>%
  mutate(
    value = V_a[cbind(row, column)],
    label = sprintf("%.3f", value)
  )

panel_5_main <- ggplot(
  panel_5_data,
  aes(x = column, y = row, fill = value)
) +
  geom_tile(colour = "white", linewidth = 0.35) +
  geom_point(
    data = highlight_upper,
    aes(x = column, y = row, colour = dyad),
    inherit.aes = FALSE,
    shape = 22,
    fill = NA,
    size = 6,
    stroke = 1.3
  ) +
  geom_text(
    data = panel_5_labels,
    aes(x = column, y = row, label = label),
    inherit.aes = FALSE,
    size = 3,
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#542788",
    trans = "sqrt",
    name = expression(V[A * "," * ij])
  ) +
  scale_colour_manual(values = highlight_colours, guide = "none") +
  scale_x_continuous(
    breaks = seq_len(n),
    labels = person_labels,
    expand = c(0, 0)
  ) +
  scale_y_reverse(
    breaks = seq_len(n),
    labels = person_labels,
    limits = c(n + 0.5, 0.5),
    expand = c(0, 0)
  ) +
  coord_equal() +
  labs(x = axis_description, y = axis_description) +
  theme_bw(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

panel_5 <- add_historical_strips(
  main_plot = panel_5_main,
  historical = historical_display,
  title = "5. Temporally moderated additive covariance V_A = A o T_A",
  subtitle = "V_A,ij = A_ij x lambda_Ai x lambda_Aj",
  caption = paste0(
    "Pedigree relatedness is retained, but each pair is reweighted using the ",
    "two members' separately estimated temporal loadings."
  ),
  person_labels = person_labels
)

# -----------------------------------------------------------------------------
# Panel 6: parameter recovery
# -----------------------------------------------------------------------------

make_compact_recovery_plot <- function(recovery_data, labels = NULL) {
  required_columns <- c(
    "parameter", "true_value", "mean_estimate", "mc_lower", "mc_upper"
  )

  absent <- setdiff(required_columns, names(recovery_data))

  if (length(absent) > 0L) {
    stop(
      "The recovery summary is missing required column(s): ",
      paste(absent, collapse = ", ")
    )
  }

  plot_data <- recovery_data %>%
    filter(
      is.finite(true_value),
      is.finite(mean_estimate),
      is.finite(mc_lower),
      is.finite(mc_upper)
    )

  if (is.null(labels)) {
    plot_data <- plot_data %>% mutate(plot_label = parameter)
  } else {
    plot_data <- plot_data %>%
      mutate(
        plot_label = unname(labels[as.character(parameter)]),
        plot_label = ifelse(is.na(plot_label), parameter, plot_label)
      )
  }

  plot_range <- range(
    plot_data$true_value,
    plot_data$mean_estimate,
    plot_data$mc_lower,
    plot_data$mc_upper,
    na.rm = TRUE
  )

  padding <- max(0.04, 0.08 * diff(plot_range))

  ggplot(plot_data, aes(x = true_value, y = mean_estimate)) +
    geom_abline(intercept = 0, slope = 1, linewidth = 0.55) +
    geom_errorbar(
      aes(ymin = mc_lower, ymax = mc_upper),
      width = 0.012,
      linewidth = 0.45
    ) +
    geom_point(size = 2.2) +
    geom_text(
      aes(label = plot_label),
      nudge_x = 0.015,
      nudge_y = 0.015,
      hjust = 0,
      size = 2.5,
      check_overlap = TRUE
    ) +
    coord_equal(
      xlim = c(plot_range[1] - padding, plot_range[2] + 2.5 * padding),
      ylim = c(plot_range[1] - padding, plot_range[2] + 2.5 * padding),
      expand = FALSE
    ) +
    labs(
      title = "6. Parameter recovery",
      subtitle = "Points are Monte Carlo means; bars are empirical 95% intervals.",
      x = "True value",
      y = "Mean estimate"
    ) +
    theme_bw(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9),
      plot.margin = margin(6, 6, 6, 6)
    )
}

if (exists("compact_recovery_plot", inherits = TRUE)) {
  panel_6 <- get("compact_recovery_plot", inherits = TRUE) +
    labs(title = "6. Parameter recovery")
} else {
  if (!exists("recovery_summary", inherits = TRUE)) {
    recovery_path <- file.path(
      base_output_directory,
      "parameter_recovery_summary.csv"
    )

    if (file.exists(recovery_path)) {
      recovery_summary <- readr::read_csv(
        recovery_path,
        show_col_types = FALSE
      )
    }
  }

  if (exists("recovery_summary", inherits = TRUE)) {
    labels_for_recovery <- if (
      exists("parameter_labels", inherits = TRUE)
    ) {
      get("parameter_labels", inherits = TRUE)
    } else {
      NULL
    }

    panel_6 <- make_compact_recovery_plot(
      recovery_data = get("recovery_summary", inherits = TRUE),
      labels = labels_for_recovery
    )
  } else {
    panel_6 <- ggplot() +
      annotate(
        "text",
        x = 0.5,
        y = 0.58,
        label = "6. Parameter recovery",
        fontface = "bold",
        size = 5
      ) +
      annotate(
        "text",
        x = 0.5,
        y = 0.42,
        label = paste0(
          "Run the Monte Carlo script first, or place\n",
          "parameter_recovery_summary.csv in:\n",
          base_output_directory
        ),
        size = 3.5
      ) +
      xlim(0, 1) +
      ylim(0, 1) +
      theme_void() +
      theme(panel.border = element_rect(colour = "grey50", fill = NA))
  }
}

# -----------------------------------------------------------------------------
# Save the six individual panels and one optional composite
# -----------------------------------------------------------------------------

save_panel(panel_0, "panel_0_full_model_equation.png", 12, 8.5)
save_panel(panel_1, "panel_1_additive_relatedness.png", 5.8, 5.4)
save_panel(panel_2, "panel_2_two_person_temporal_inputs.png", 6.6, 5.8)
save_panel(panel_3, "panel_3_pairwise_temporal_weight.png", 6.6, 5.8)
save_panel(panel_4, "panel_4_worked_dyad_comparison.png", 7.2, 5.2)
save_panel(panel_5, "panel_5_temporally_moderated_covariance.png", 6.6, 5.8)
save_panel(panel_6, "panel_6_parameter_recovery.png", 5.8, 5.4)

combined_figure <- (
  panel_0 | panel_1 | panel_2 | panel_3
) / (
  panel_4 | panel_5 | panel_6
) +
  patchwork::plot_layout(
    widths = c(1.6, 1, 1.08, 1.08),
    heights = c(1, 1)
  ) +
  patchwork::plot_annotation(
    title = "How two individuals' temporal positions enter additive covariance",
    theme = theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
    )
  )

save_panel(
  combined_figure,
  "combined_temporal_pair_figure.png",
  20,
  11.5
)

cat(
  "\nTemporal pair panels written to:\n  ",
  normalizePath(panel_output_directory, mustWork = FALSE),
  "\n",
  sep = ""
)

print(dyad_comparison)

panel_1
panel_2
panel_3
panel_4
panel_5
panel_6
