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

 # V_i <- #make_symmetric(V_i) + diag(1e-6, n_i)

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
