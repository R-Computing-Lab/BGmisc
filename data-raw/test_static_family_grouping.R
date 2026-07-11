# Compare the original one-submodel-per-family implementation with the optional
# grouped static-family implementation using simulated pedigree data.

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


source("data-raw\\smoketest_helpers.R")
set.seed(5)

n_fam <- 50L

ped <- simulatePedigree(
  kpc = 3,
  Ngen = 4,
  sexR = 0.50,
  marR = 0.60,
  beta = FALSE
)

Addmat <- ped2add(ped)
Nucmat <- ped2cn(ped)
Mtdmat <- ped2mit(ped)

Amimat <- NULL
Dmgmat <- NULL
Extmat <- TRUE

fsize <- nrow(Addmat)

obs_ids <- rownames(Addmat)

if (is.null(obs_ids)) {
  obs_ids <- as.character(ped$ID)
}

if (length(obs_ids) != fsize) {
  stop(
    "The number of observed IDs does not match the dimensions of the ",
    "pedigree relatedness matrices."
  )
}

simulate_static_families <- function(
    n_fam = 50,
    kpc = 3,
    Ngen = 4,
    marR = 0.6,
    true_vars = list(
      ad2 = 0.60,
      cn2 = 0.15,
      ce2 = 0.10,
      mt2 = 0.05,
      ee2 = 0.70
    ),
    components = c("a", "cn", "ce", "mt", "e"),
    mean_y = 2,
    seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ped <- simulate_pedigree_safe(
    kpc = kpc,
    Ngen = Ngen,
    marR = marR
  )

  A <- make_symmetric(BGmisc::ped2add(ped))
  Cn <- make_symmetric(BGmisc::ped2cn(ped))
  Ce <- make_symmetric(BGmisc::ped2ce(ped))
  Mt <- make_symmetric(BGmisc::ped2mit(ped))

  n <- nrow(A)
  I <- diag(1, n)

  V_true <- matrix(
    0,
    nrow = n,
    ncol = n
  )

  if ("a" %in% components) {
    V_true <- V_true + true_vars$ad2 * A
  }

  if ("cn" %in% components) {
    V_true <- V_true + true_vars$cn2 * Cn
  }

  if ("ce" %in% components) {
    V_true <- V_true + true_vars$ce2 * Ce
  }

  if ("mt" %in% components) {
    V_true <- V_true + true_vars$mt2 * Mt
  }

  if ("e" %in% components) {
    V_true <- V_true + true_vars$ee2 * I
  }

  V_true <- make_symmetric(V_true)

  y <- mvtnorm::rmvnorm(
    n = n_fam,
    mean = rep(mean_y, n),
    sigma = V_true
  )

  storage.mode(y) <- "double"

  rn <- rownames(A)

  if (
    is.null(rn) ||
    anyNA(rn) ||
    any(rn == "")
  ) {
    rn <- as.character(seq_len(n))
  }

  obs_ids <- paste0("S", rn)

  colnames(y) <- obs_ids
  rownames(y) <- paste0("fam", seq_len(n_fam))

  dimnames(A) <- list(obs_ids, obs_ids)
  dimnames(Cn) <- list(obs_ids, obs_ids)
  dimnames(Ce) <- list(obs_ids, obs_ids)
  dimnames(Mt) <- list(obs_ids, obs_ids)
  dimnames(V_true) <- list(obs_ids, obs_ids)

  list(
    ped = ped,
    data = y,
    obs_ids = obs_ids,
    A = A,
    Cn = Cn,
    Ce = Ce,
    Mt = Mt,
    V_true = V_true,
    true_vars = true_vars
  )
}


set.seed(5)

sim_50 <- simulate_static_families(
  n_fam = 50,
  kpc = 3,
  Ngen = 4,
  marR = 0.6,
  seed = 5,
  true_vars = list(
    ad2 = 0.60,
    cn2 = 0.15,
    ce2 = 0.10,
    mt2 = 0.05,
    ee2 = 0.70
  ),
  components = c(
    "a",
    "cn",
    "ce",
    "mt",
    "e"
  ),
  mean_y = 2
)

sim_10 <- simulate_static_families(
  n_fam = 10,
  kpc = 4,
  Ngen = 4,
  marR = 0.8,
  seed = 15,
  true_vars = list(
    ad2 = 0.60,
    cn2 = 0.15,
    ce2 = 0.10,
    mt2 = 0.05,
    ee2 = 0.70
  ),
  components = c(
    "a",
    "cn",
    "ce",
    "mt",
    "e"
  ),
  mean_y = 2
)

data <- sim_50$data

Addmat <- sim_50$A
Nucmat <- sim_50$Cn
Extmat <- sim_50$Ce
Mtdmat <- sim_50$Mt

Amimat <- NULL
Dmgmat <- NULL

vars <- list(
  ad2 = 0.50,
  dd2 = 0.30,
  cn2 = 0.20,
  ce2 = 0.10,
  mt2 = 0.10,
  am2 = 0.25,
  ee2 = 0.60
)



fit_separate <- fitPedigreeModel(
  model_name = "PedigreeSeparate",
  vars = vars,
  data = data,
  Addmat = Addmat,
  Nucmat = Nucmat,
  Extmat = Extmat,
  Mtdmat = Mtdmat,
  Amimat = Amimat,
 Dmgmat = Dmgmat,
  temporal = FALSE,
  group_static_families = FALSE,
  tryhard = FALSE,
  intervals = FALSE
)

fit_grouped <- fitPedigreeModel(
  model_name = "PedigreeGrouped",
  vars = vars,
  data = data,
  Addmat = Addmat,
  Nucmat = Nucmat,
  Extmat = Extmat,
  Mtdmat = Mtdmat,
  Amimat = Amimat,
  Dmgmat = Dmgmat,
  temporal = FALSE,
  group_static_families = TRUE,
  tryhard = FALSE,
  intervals = FALSE
)

p_separate <- OpenMx::omxGetParameters(
  fit_separate
)

p_grouped <- OpenMx::omxGetParameters(
  fit_grouped
)

common_parameters <- intersect(
  names(p_separate),
  names(p_grouped)
)

parameter_comparison <- data.frame(
  parameter = common_parameters,
  separate = unname(
    p_separate[common_parameters]
  ),
  grouped = unname(
    p_grouped[common_parameters]
  ),
  difference = unname(
    p_grouped[common_parameters] -
      p_separate[common_parameters]
  ),
  absolute_difference = unname(abs(
    p_grouped[common_parameters] -
      p_separate[common_parameters]
  )),
  row.names = NULL,
  check.names = FALSE
)

parameter_comparison <- parameter_comparison[
  order(
    parameter_comparison$absolute_difference,
    decreasing = TRUE
  ),
  ,
  drop = FALSE
]

separate_size_bytes <- as.numeric(
  object.size(fit_separate)
)

grouped_size_bytes <- as.numeric(
  object.size(fit_grouped)
)

comparison <- list(
  number_of_families = nrow(data),

  family_size = ncol(data),

  separate_size_bytes =
    separate_size_bytes,

  grouped_size_bytes =
    grouped_size_bytes,

  bytes_saved =
    separate_size_bytes -
    grouped_size_bytes,

  size_ratio_grouped_to_separate =
    grouped_size_bytes /
    separate_size_bytes,

  percent_size_reduction =
    100 * (
      1 -
      grouped_size_bytes /
        separate_size_bytes
    ),

  separate_status_code =
    fit_separate$output$status$code,

  grouped_status_code =
    fit_grouped$output$status$code,

  separate_minus2ll =
    fit_separate$output$fit,

  grouped_minus2ll =
    fit_grouped$output$fit,

  minus2ll_difference =
    fit_grouped$output$fit -
    fit_separate$output$fit,

  maximum_absolute_parameter_difference =
    max(
      parameter_comparison$absolute_difference
    ),

  fit_equal = isTRUE(all.equal(
    fit_grouped$output$fit,
    fit_separate$output$fit,
    tolerance = 1e-7
  )),

  parameters_equal = isTRUE(all.equal(
    p_grouped[common_parameters],
    p_separate[common_parameters],
    tolerance = 1e-7
  ))
)

print(comparison)
print(parameter_comparison)

p_separate <- OpenMx::omxGetParameters(fit_separate)
p_grouped <- OpenMx::omxGetParameters(fit_grouped)

common_parameters <- intersect(
  names(p_separate),
  names(p_grouped)
)

parameter_comparison <- data.frame(
  parameter = common_parameters,
  separate = unname(p_separate[common_parameters]),
  grouped = unname(p_grouped[common_parameters]),
  difference = unname(
    p_grouped[common_parameters] -
      p_separate[common_parameters]
  ),
  absolute_difference = unname(abs(
    p_grouped[common_parameters] -
      p_separate[common_parameters]
  )),
  row.names = NULL
)

comparison <- list(
  number_of_families = n_fam,
  family_size = fsize,
  separate_size_bytes = as.numeric(
    object.size(fit_separate)
  ),
  grouped_size_bytes = as.numeric(
    object.size(fit_grouped)
  ),
  size_ratio_grouped_to_separate =
    as.numeric(object.size(fit_grouped)) /
    as.numeric(object.size(fit_separate)),
  percent_size_reduction =
    100 * (
      1 -
        as.numeric(object.size(fit_grouped)) /
        as.numeric(object.size(fit_separate))
    ),
  separate_status_code =
    fit_separate$output$status$code,
  grouped_status_code =
    fit_grouped$output$status$code,
  separate_minus2ll =
    fit_separate$output$fit,
  grouped_minus2ll =
    fit_grouped$output$fit,
  minus2ll_difference =
    fit_grouped$output$fit -
    fit_separate$output$fit,
  maximum_absolute_parameter_difference =
    max(parameter_comparison$absolute_difference),
  fit_equal = isTRUE(all.equal(
    fit_grouped$output$fit,
    fit_separate$output$fit,
    tolerance = 1e-7
  )),
  parameters_equal = isTRUE(all.equal(
    p_grouped[common_parameters],
    p_separate[common_parameters],
    tolerance = 1e-7
  ))
)

print(comparison)
print(parameter_comparison)
