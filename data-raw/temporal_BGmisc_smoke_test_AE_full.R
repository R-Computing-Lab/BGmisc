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

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

get_generation_vector <- function(ped) {
  possible_names <- c("gen", "Gen", "generation", "Generation", "GEN")
  gen_col <- intersect(possible_names, names(ped))[1]
  if (is.na(gen_col)) {
    stop(
      "Could not find a generation column in the simulated pedigree.\n",
      "Available columns are: ", paste(names(ped), collapse = ", ")
    )
  }
  as.numeric(ped[[gen_col]])
}

make_time_vars <- function(ped, threshold_year = 1776, birth_year_sd = 3,
                             birth_year_base = 1700,
                           gen_gap=30
                             ) {
  gen <- get_generation_vector(ped)
  birth_year <- birth_year_base + gen_gap * (gen - min(gen, na.rm = TRUE)) + rnorm(length(gen), mean = 0, sd = birth_year_sd)
  t_i <- as.numeric(scale(birth_year))
  h_i <- as.numeric(birth_year >= threshold_year)
  H_i <- matrix(h_i, ncol = 1)
  colnames(H_i) <- paste0("post_", threshold_year)

  list(
    birth_year = birth_year,
    t = t_i,
    H = H_i
  )
}

make_lambda <- function(t_i, H_i, beta, gamma) {
  Tpoly <- cbind(1, t_i, t_i^2, t_i^3)
  as.vector(Tpoly %*% matrix(beta, ncol = 1) + H_i %*% matrix(gamma, ncol = 1))
}

as_numeric_matrix <- function(x) {
  x <- as.matrix(x)
  storage.mode(x) <- "numeric"
  x
}

make_symmetric <- function(x, tol = 1e-10) {
  x <- as_numeric_matrix(x)
  if (max(abs(x - t(x)), na.rm = TRUE) > tol) {
    x <- (x + t(x)) / 2
  }
  x
}

simulate_pedigree_safe <- function(kpc = 3, Ngen = 4, marR = 0.6) {
  fmls <- names(formals(BGmisc::simulatePedigree))

  if (all(c("kpc", "Ngen", "marR") %in% fmls)) {
    BGmisc::simulatePedigree(kpc = kpc, Ngen = Ngen, marR = marR)
  } else if (all(c("numGen", "children", "marriageRate") %in% fmls)) {
    BGmisc::simulatePedigree(numGen = Ngen, children = kpc, marriageRate = marR)
  } else {
    BGmisc::simulatePedigree(kpc, Ngen, marR)
  }
}

# -----------------------------------------------------------------------------
# Temporal OpenMx/BGmisc-style builders
# -----------------------------------------------------------------------------



free_only <- function(model, labels_to_free) {
  pars <- omxGetParameters(model)
  omxSetParameters(
    model,
    labels = names(pars),
    free = names(pars) %in% labels_to_free,
    values = ifelse(names(pars) %in% labels_to_free, pars, 0)
  )
}

run_and_report <- function(model, label, tries = 20) {
  cat("\n==============================\n")
  cat("Running ", label, "\n", sep = "")
  cat("==============================\n")

  fit <- mxTryHard(
    model,
    extraTries = tries,
    intervals = FALSE,
    silent = FALSE
  )

  print(summary(fit))
  cat("\nParameter estimates:\n")
  print(omxGetParameters(fit))
  cat("\nOpenMx status:\n")
  print(fit$output$status)

  if (!is.finite(fit$output$fit)) stop(label, ": model fit is not finite.")
  if (!all(is.finite(omxGetParameters(fit)))) stop(label, ": at least one parameter estimate is not finite.")

  invisible(fit)
}

# -----------------------------------------------------------------------------
# Simulation using BGmisc-style phenotype generation
# -----------------------------------------------------------------------------

simulate_temporal_family <- function(
    kpc = 3,
    Ngen = 4,
    marR = 0.6,
    threshold_year = 1776,
    true_beta,
    true_gamma,
    components = c("a", "e"),
    family_id = NULL
) {
  ped_i <- simulate_pedigree_safe(kpc = kpc, Ngen = Ngen, marR = marR)
  if (is.null(family_id)) family_id <- 1
  ped_i$fam<- paste0("FAM ", family_id)
  A_i <- make_symmetric(BGmisc::ped2add(ped_i))
  Cn_i <- make_symmetric(BGmisc::ped2cn(ped_i))
  Ce_i <- make_symmetric(BGmisc::ped2ce(ped_i))
  Mt_i <- make_symmetric(BGmisc::ped2mit(ped_i))

  n_i <- nrow(A_i)
  I_i <- diag(1, n_i)

  tv_i <- make_time_vars(ped_i, threshold_year = threshold_year)
  t_i <- tv_i$t
  H_i <- tv_i$H

  lambda <- list()
  for (k in components) {
    lambda[[k]] <- make_lambda(t_i, H_i, true_beta[[k]], true_gamma[[k]])
  }

  V_i <- matrix(0, n_i, n_i)
  if ("a" %in% components) V_i <- V_i + A_i * tcrossprod(lambda$a)
  if ("cn" %in% components) V_i <- V_i + Cn_i * tcrossprod(lambda$cn)
  if ("ce" %in% components) V_i <- V_i + Ce_i * tcrossprod(lambda$ce)
  if ("mt" %in% components) V_i <- V_i + Mt_i * tcrossprod(lambda$mt)
  if ("e" %in% components) V_i <- V_i + I_i * tcrossprod(lambda$e)

  V_i <- make_symmetric(V_i) + diag(1e-6, n_i)

  y_i <- mvtnorm::rmvnorm(1, sigma = V_i)

  rn <- rownames(A_i)
  if (is.null(rn) || anyNA(rn) || any(rn == "")) rn <- as.character(seq_len(n_i))
  obs_ids <- paste0("S", rn)

  list(
    ped = ped_i,
    y = as.numeric(y_i),
    obs_ids = obs_ids,
    birth_year_scaled = t_i,
    birth_year = tv_i$birth_year,
    H = H_i,
    A = A_i,
    Cn = Cn_i,
    Ce = Ce_i,
    Mt = Mt_i,
    V_true = V_i
  )
}


# -----------------------------------------------------------------------------
# Run smoke test
# -----------------------------------------------------------------------------

n_families <- 150
threshold_year <- 1760
sim_components <- c("a", "e")
fit_components <- c("a", "e")

# Data-generating parameters.
# These are linear-loading parameters used for simulation. The fitted model below
# uses exp(loadings), so do not interpret this as a strict recovery design yet.


true_beta <- list(
  a  = c(0.65, 0.30, 0.00, 0.00),
  cn = c(0.00, 0.00, 0.00, 0.00),
  ce = c(0.00, 0.00, 0.00, 0.00),
  mt = c(0.00, 0.00, 0.00, 0.00),
  e  = c(0.75, 0.00, 0.00, 0.00)
)


true_gamma <- list(
  a  = 0.00,
  cn = 0.00,
  ce = 0.00,
  mt = 0.00,
  e  = 0.20
)

families <- vector("list", n_families)
for (i in seq_len(n_families)) {
  families[[i]] <- simulate_temporal_family(
    kpc = 3,
    Ngen = 4,
    marR = 0.6,
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
family_peds <- lapply(families, function(x)
  cbind(x$ped,y = x$y,birth_year = x$birth_year,
                        birth_year_scaled = x$birth_year_scaled,
                        post_1776 = x$H
                      )


                      ) %>% dplyr::bind_rows() %>%
  dplyr::mutate(fam = factor(fam))

ggplot2::ggplot(family_peds) +
  ggplot2::geom_point(ggplot2::aes(x = birth_year_scaled, y = y, color = post_1760)) +
#  ggplot2::facet_wrap(~fam) +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Simulated Phenotypes by Family", x = "Scaled Birth Year", y = "Phenotype (y)")




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
temporal_model_ae0 <- free_only(
  temporal_model_ae,
  labels_to_free = c("b_a_0", "b_e_0", "mean_y")
)
fit_ae0 <- run_and_report(temporal_model_ae0, "AE intercept-only", tries = 20)

# Stage 2: AE with linear birth-cohort moderation.
temporal_model_ae_linear <- free_only(
  temporal_model_ae,
  labels_to_free = c("b_a_0", "b_a_1", "b_e_0", "b_e_1", "mean_y")
)
fit_ae_linear <- run_and_report(temporal_model_ae_linear, "AE linear time", tries = 30)

# Stage 3: AE with linear birth-cohort moderation plus one historical moderator.
temporal_model_ae_linear_h <- free_only(
  temporal_model_ae,
  labels_to_free = c("b_a_0", "b_a_1", "g_a_1", "b_e_0", "b_e_1", "g_e_1", "mean_y")
)
fit_ae_linear_h <- run_and_report(temporal_model_ae_linear_h, "AE linear time + historical moderator", tries = 30)

cat("\nTemporal BGmisc-style AE smoke test completed successfully.\n")

target <- c(
  b_a_0 = log(true_beta$a[1]),
  b_a_1 = true_beta$a[2] / true_beta$a[1],
  g_a_1 = true_gamma$a[1] / true_beta$a[1],
  b_e_0 = log(true_beta$e[1]),
  b_e_1 = true_beta$e[2] / true_beta$e[1],
  g_e_1 = true_gamma$e[1]/ true_beta$e[1]
)

est <- omxGetParameters(fit_ae_linear_h)[names(target)]

round(cbind(target = target, estimate = est, diff = est - target), 3)

# graph estimates of a as a function of time and historical moderator



graphing_data <- data.frame(
  time = seq(-3, 3, length.out = 100),
  historical = c(0, 1))

graphing_data$estimated_a_variance <- exp(est["b_a_0"] + est["b_a_1"] * graphing_data$time + est["g_a_1"] * graphing_data$historical)
graphing_data$true_a_variance <- exp(target["b_a_0"] + target["b_a_1"] * graphing_data$time + target["g_a_1"] * graphing_data$historical)
graphing_data$estimated_e_variance <- exp(est["b_e_0"] + est["b_e_1"] * graphing_data$time + est["g_e_1"] * graphing_data$historical)
graphing_data$true_e_variance <- exp(target["b_e_0"] + target["b_e_1"] * graphing_data$time + target["g_e_1"] * graphing_data$historical)
graphing_data$estimated_total_variance <- graphing_data$estimated_a_variance + graphing_data$estimated_e_variance
graphing_data$true_total_variance <- graphing_data$true_a_variance + graphing_data$true_e_variance
graphing_data$unscaled_time <- graphing_data$time * sd(unlist(lapply(families, function(x) x$birth_year_scaled))) + mean(unlist(lapply(families, function(x) x$birth_year_scaled)))

graphing_data_long <- # have a true and estimated factor
graphing_data %>%
  tidyr::pivot_longer(cols = c(estimated_a_variance, true_a_variance, estimated_e_variance, true_e_variance, estimated_total_variance, true_total_variance),
                      names_to = c("type", "component", NA),
                      names_sep = "_",
                      values_to = "variance")


ggplot2::ggplot(graphing_data_long) +
  ggplot2::geom_line(ggplot2::aes(x = unscaled_time, y = variance, linetype = factor(historical),
                                  color = factor(component)
                                  )) +
  ggplot2::labs(title = "Estimated Variance as a function of time and historical moderator", x = "Scaled Birth Year", y = "Estimated Variance", color = "Variance Component") +
  ggplot2::theme_bw() + facet_wrap(~type)




# -----------------------------------------------------------------------------
# Optional AME test after AE runs
# -----------------------------------------------------------------------------

run_optional_ame <- FALSE

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
