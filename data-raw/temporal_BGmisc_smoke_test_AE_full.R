# temporal_BGmisc_smoke_test_AE_full.R
#
# Basic smoke test for a temporally moderated BGmisc/OpenMx pedigree model.
#
# This script is intentionally staged and minimal:
#   1. Simulate pedigrees with BGmisc::simulatePedigree().
#   2. Construct BGmisc relatedness matrices.
#   3. Simulate phenotype data from a temporal A + E model.
#   4. Build OpenMx family-group models using a BGmisc-style architecture.
#   5. Fit an intercept-only AE model.
#   6. Fit a linear-time AE model.
#   7. Fit a linear-time + historical moderator AE model.
#
# The point is not a full recovery study yet. The point is to test whether
# the temporal covariance algebra builds, runs, and returns finite estimates.

# -----------------------------------------------------------------------------
# Package setup
# -----------------------------------------------------------------------------

required_packages <- c("BGmisc", "OpenMx", "mvtnorm")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall them before running this script."
  )
}

library(BGmisc)
library(OpenMx)
library(mvtnorm)
library(tidyverse)
set.seed(202601)

source("data-raw\\smoketest_helpers.R")

# -----------------------------------------------------------------------------
# Run smoke test
# -----------------------------------------------------------------------------

n_families <- 150
threshold_year <- 1776
sim_components <- c("a", #"cn","ce",
                   # "mt",
                    "e"
                    )
fit_components <- c("a",# "cn","ce",
                   # "mt",
                   "e")

# Data-generating parameters.
# These are linear-loading parameters used for simulation. The fitted model below
# uses exp(loadings), so do not interpret this as a strict recovery design yet.


true_beta <- list(
  a  = c(1.65, -0.5, 0.00, 0.00),
  cn = c(0.00, 0.00, 0.00, 0.00),
  ce = c(0.00, 0.00, 0.00, 0.00),
  mt = c(0.00, 0.00, 0.00, 0.00),
  e  = c(2.05, -.20, 0.00, 0.00)
)


true_gamma <- list(
  a  = -0.5,
  cn = 0.00,
  ce = 0.00,
  mt = 0.00,
  e  = -.50
)

target <- c(
  b_a_0 = if (true_beta$a[1] != 0) {
    log(true_beta$a[1])
  } else {
    0
  },
  b_a_1 = if (true_beta$a[1] != 0) {
    true_beta$a[2] / true_beta$a[1]
  } else {
    0
  },
  g_a_1 = if (true_beta$a[1] != 0) {
    true_gamma$a[1] / true_beta$a[1]
  } else {
    0
  },
  b_cn_0 = if (true_beta$cn[1] != 0) {
    log(true_beta$cn[1])
  } else {
    0
  },
  b_cn_1 = if (true_beta$cn[1] != 0) {
    true_beta$cn[2] / true_beta$cn[1]
  } else {
    0
  },
  g_cn_1 = if (true_beta$cn[1] != 0) {
    true_gamma$cn[1] / true_beta$cn[1]
  } else {
    0
  },
  b_ce_0 = if (true_beta$ce[1] != 0) {
    log(true_beta$ce[1])
  } else {
    0
  },
  b_ce_1 = if (true_beta$ce[1] != 0) {
    true_beta$ce[2] / true_beta$ce[1]
  } else {
    0
  },
  g_ce_1 = if (true_beta$ce[1] != 0) {
    true_gamma$ce[1] / true_beta$ce[1]
  } else {
    0
  },
  b_mt_0 = if (true_beta$mt[1] != 0) {
    log(true_beta$mt[1])
  } else {
    0
  },
  b_mt_1 = if (true_beta$mt[1] != 0) {
    true_beta$mt[2] / true_beta$mt[1]
  } else {
    0
  },
  g_mt_1 = if (true_beta$mt[1] != 0) {
    true_gamma$mt[1] / true_beta$mt[1]
  } else {
    0
  },
  b_e_0 = if (true_beta$e[1] != 0) {
    log(true_beta$e[1])
  } else {
    0
  },
  b_e_1 = if (true_beta$e[1] != 0) {
    true_beta$e[2] / true_beta$e[1]
  } else {
    0
  },
  g_e_1 = if (true_beta$e[1] != 0) {
    true_gamma$e[1] / true_beta$e[1]
  } else {
    0
  }
)


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

cat("Simulated", n_families, "families. Family sizes:\n")
print(vapply(families, function(x) length(x$y), integer(1)))


# nees to add H, y and birth_year_scaled to the family data frame for plotting and analysis
family_peds <- lapply(families, function(x) {
  cbind(x$ped,
    y = x$y, birth_year = x$birth_year,
    birth_year_scaled = x$birth_year_scaled,
    post_1776 = x$H
  )
}) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(fam = factor(fam))
library(ggplot2)
library(ggforce)
family_peds %>%
#  slice_sample(n = 1000) %>%
ggplot(aes(x = birth_year_scaled, y = y)) +
  geom_point(aes(
                 fill = as.factor(post_1776),
                  color = fam,
                 shape = sex),
             alpha = .7) +
  #  ggplot2::facet_wrap(~fam) +
#  geom_mark_hull(
 #   aes(
 #     x = birth_year_scaled,
 #     y = y,
 #     group = interaction(fam, gen),
 #     fill = NULL,
 #     color = fam
 #   ),
 #   concavity = 5,
 #   expand = unit(3, "mm"),
 #   radius = unit(1, "mm"),
  #  alpha = .3,
  #  fill = NA,
  #  linewidth = 0.6
 # ) +
  theme_bw() +
  labs(title = "Simulated Phenotypes by Family", x = "Scaled Birth Year", y = "Phenotype (y)") +
  # add viridis color
  scale_color_viridis_d(option = "plasma") +
  scale_fill_viridis_d()+
  scale_shape_manual(values = c("F" = 21, "M" = 24)) +
  # remove color legend
  theme(legend.position = "bottom") +
  guides(color = "none", shape = guide_legend(override.aes = list(size = 3, alpha = 1, fill = "black")))


# Build family group models for temporal A + E.
group_models <- vector("list", n_families)
for (i in seq_len(n_families)) {
  fam <- families[[i]]
  group_models[[i]] <- buildOneTemporalFamilyGroup(
    group_name = paste0("family", i),
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

# Parent model with all AE temporal terms present.
temporal_model_ae <- buildTemporalPedigreeMx(
  model_name = "TemporalPedigreeSmokeTest_AE",
  group_models = group_models,
  p_hist = 1,
  components = fit_components,
  ci = FALSE
)

# Stage 1: intercept-only AE.
temporal_model_ae_0 <- free_only(
  temporal_model_ae,
  labels_to_free = c("b_a_0", "b_e_0", "mean_y")
)
fit_AE0 <- run_and_report(temporal_model_ae_0, "AE intercept-only", tries = 20)


est <- est_int <- omxGetParameters(fit_AE0)[names(target)]

round(cbind(target = target, estimate = est, diff = est - target), 3)


# Stage 2: AE with linear birth-cohort moderation.
temporal_model_ae_linear <- free_only(
  temporal_model_ae,
  labels_to_free = c("b_a_0", "b_a_1", "b_e_0", "b_e_1", "mean_y")
)
fit_ae_linear <- run_and_report(temporal_model_ae_linear, "AE linear time", tries = 30)


est <- est_linear <- omxGetParameters(fit_ae_linear)[names(target)]

round(cbind(target = target, estimate = est, diff = est - target), 3)


# Stage 3: AE with linear birth-cohort moderation plus one historical moderator.
temporal_model_ae_linear_h <- free_only(
  temporal_model_ae,
  labels_to_free = c("b_a_0", "b_a_1", "g_a_1", "b_e_0", "b_e_1", "g_e_1", "mean_y")
)
fit_ae_linear_h <- run_and_report(temporal_model_ae_linear_h, "AE linear time + historical moderator", tries = 30)

cat("\nTemporal BGmisc-style AE smoke test completed successfully.\n")


est <- est_linear_h <- omxGetParameters(fit_ae_linear_h)[names(target)]

round(cbind(target = target, estimate = est, diff = est - target), 3)


# graph estimates of a as a function of time and historical moderator


graphing_data <- data.frame(
  time = seq(-3, 3, length.out = 100),
  historical = c(0, 1)
)

graphing_data$estimated_a_variance <- exp(est["b_a_0"] + est["b_a_1"] * graphing_data$time + est["g_a_1"] * graphing_data$historical)
graphing_data$true_a_variance <- exp(target["b_a_0"] + target["b_a_1"] * graphing_data$time + target["g_a_1"] * graphing_data$historical)
graphing_data$estimated_e_variance <- exp(est["b_e_0"] + est["b_e_1"] * graphing_data$time + est["g_e_1"] * graphing_data$historical)
graphing_data$true_e_variance <- exp(target["b_e_0"] + target["b_e_1"] * graphing_data$time + target["g_e_1"] * graphing_data$historical)
graphing_data$estimated_total_variance <- graphing_data$estimated_a_variance + graphing_data$estimated_e_variance
graphing_data$true_total_variance <- graphing_data$true_a_variance + graphing_data$true_e_variance
graphing_data$unscaled_time <- graphing_data$time * sd(unlist(lapply(families, function(x) x$birth_year_scaled))) + mean(unlist(lapply(families, function(x) x$birth_year_scaled)))

graphing_data_long <- # have a true and estimated factor
  graphing_data %>%
  tidyr::pivot_longer(
    cols = c(estimated_a_variance, true_a_variance, estimated_e_variance, true_e_variance, estimated_total_variance, true_total_variance),
    names_to = c("type", "component", NA),
    names_sep = "_",
    values_to = "variance"
  )


ggplot2::ggplot(graphing_data_long) +
  ggplot2::geom_line(ggplot2::aes(
    x = unscaled_time, y = variance, linetype = factor(historical),
    color = factor(component)
  )) +
  ggplot2::labs(title = "Estimated Variance as a function of time and historical moderator", x = "Scaled Birth Year", y = "Estimated Variance", color = "Variance Component") +
  ggplot2::theme_bw() +
  facet_wrap(~type)


# summary of the models versus the true parameter

results_summary <- data.frame(
  model = c(
    "true",
    "intercept-only", "linear time", "linear time + historical moderator"
  ),
  b_a_0 = c(
    target["b_a_0"],
    est_int["b_a_0"], est_linear["b_a_0"], est_linear_h["b_a_0"]
  ),
  b_a_1 = c(
    target["b_a_1"],
    NA, est_linear["b_a_1"], est_linear_h["b_a_1"]
  ),
  g_a_1 = c(
    target["g_a_1"],
    NA, NA, est_linear_h["g_a_1"]
  ),
  b_e_0 = c(target["b_e_0"], est_int["b_e_0"], est_linear["b_e_0"], est_linear_h["b_e_0"]),
  b_e_1 = c(
    target["b_e_1"],
    NA, est_linear["b_e_1"], est_linear_h["b_e_1"]
  ),
  g_e_1 = c(
    target["g_e_1"],
    NA, NA, est_linear_h["g_e_1"]
  )
)

print(results_summary)
# -----------------------------------------------------------------------------
# Optional AME test after AE runs
# -----------------------------------------------------------------------------

run_optional_ame <- T

if (run_optional_ame) {
  ame_group_models <- vector("list", n_families)
  for (i in seq_len(n_families)) {
    fam <- families[[i]]
    ame_group_models[[i]] <- buildOneTemporalFamilyGroup(
      group_name = paste0("ame_family", i),
      Addmat = fam$A,
      Nucmat = NULL,
      Extmat = NULL,
      Mtdmat = fam$Mt,
      Dmgmat = NULL,
      full_df_row = fam$y,
      obs_ids = fam$obs_ids,
      birth_year = fam$birth_year_scaled,
      H = fam$H,
      use_exp_loadings = TRUE
    )
  }

  temporal_model_ame <- buildTemporalPedigreeMx(
    model_name = "TemporalPedigreeSmokeTest_AME",
    group_models = ame_group_models,
    p_hist = 1,
    components = c("a", "mt", "e"),
    ci = FALSE
  )

  temporal_model_ame0 <- free_only(
    temporal_model_ame,
    labels_to_free = c("b_a_0", "b_mt_0", "b_e_0", "mean_y")
  )
  fit_ame0 <- run_and_report(temporal_model_ame0, "AME intercept-only", tries = 30)


}
