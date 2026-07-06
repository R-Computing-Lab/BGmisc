buildTemporalPedigreeModelCovariance <- function(
  p_hist,
  components = c("a", "d", "cn", "ce", "mt", "e"),
  start_beta0 = 0.5,
  start_beta_time = 0,
  start_gamma = 0
) {
  .require_openmx("buildTemporalPedigreeModelCovariance")

  beta_name <- function(k) paste0("B_", k)
  gamma_name <- function(k) paste0("G_", k)

  mats <- list()

  for (k in components) {
    mats[[beta_name(k)]] <- OpenMx::mxMatrix(
      type = "Full",
      nrow = 4,
      ncol = 1,
      free = TRUE,
      values = c(start_beta0, start_beta_time, start_beta_time, start_beta_time),
      labels = paste0("b_", k, "_", 0:3),
      name = beta_name(k)
    )

    if (p_hist > 0) {
      mats[[gamma_name(k)]] <- OpenMx::mxMatrix(
        type = "Full",
        nrow = p_hist,
        ncol = 1,
        free = TRUE,
        values = start_gamma,
        labels = paste0("g_", k, "_", seq_len(p_hist)),
        name = gamma_name(k)
      )
    }
  }

  do.call(OpenMx::mxModel, c(list("ModelOne"), mats))
}


buildOneTemporalFamilyGroup <- function(
    group_name,
    Addmat = NULL,
    Nucmat = NULL,
    Extmat = NULL,
    Mtdmat = NULL,
    Dmgmat = NULL,
    full_df_row,
    obs_ids,
    birth_year,
    H = NULL,
    use_exp_loadings = TRUE
) {
  mats_in <- list(Addmat, Dmgmat, Nucmat, Extmat, Mtdmat)
  fsize <- NULL
  for (m in mats_in) {
    if (!is.null(m)) {
      fsize <- nrow(m)
      break
    }
  }
  if (is.null(fsize)) stop("At least one relatedness matrix must be provided.")

  if (length(obs_ids) != fsize) stop("Length of obs_ids must equal family size.")
  if (length(birth_year) != fsize) stop("Length of birth_year must equal family size.")

  if (is.null(H)) {
    H <- matrix(numeric(0), nrow = fsize, ncol = 0)
  } else {
    H <- as_numeric_matrix(H)
    if (nrow(H) != fsize) stop("H must have nrow equal to family size.")
  }
  p_hist <- ncol(H)

  # One raw-data row with named phenotype columns.
  full_df_row <- matrix(as.numeric(full_df_row), nrow = 1)
  colnames(full_df_row) <- obs_ids
  rownames(full_df_row) <- group_name
  full_df_row <- as.data.frame(full_df_row, check.names = FALSE)
  stopifnot(identical(colnames(full_df_row), obs_ids))

  t_i <- as.numeric(birth_year)
  Tpoly <- cbind(1, t_i, t_i^2, t_i^3)

  mat_spec <- list(
    list(mat = Addmat, mxname = "A",  k = "a",  K = "Ka"),
    list(mat = Dmgmat, mxname = "D",  k = "d",  K = "Kd"),
    list(mat = Nucmat, mxname = "Cn", k = "cn", K = "Kcn"),
    list(mat = Extmat, mxname = "Ce", k = "ce", K = "Kce"),
    list(mat = Mtdmat, mxname = "Mt", k = "mt", K = "Kmt")
  )
  active <- Filter(function(s) !is.null(s$mat), mat_spec)

  fixed_parts <- list(
    mxData(observed = full_df_row, type = "raw", sort = FALSE),
    mxMatrix("Iden", nrow = fsize, ncol = fsize, name = "I"),
    mxMatrix("Full", nrow = fsize, ncol = 4, free = FALSE, values = Tpoly, name = "Tpoly")
  )

  if (p_hist > 0) {
    fixed_parts[[length(fixed_parts) + 1]] <- mxMatrix(
      "Full", nrow = fsize, ncol = p_hist, free = FALSE, values = H, name = "H"
    )
  }

  rel_parts <- lapply(active, function(s) {
    mxMatrix(
      type = "Symm",
      nrow = fsize,
      ncol = fsize,
      free = FALSE,
      values = make_symmetric(s$mat),
      name = s$mxname
    )
  })

  make_eta_alg <- function(k) {
    if (p_hist > 0) {
      mxAlgebraFromString(
        paste0("Tpoly %*% ModelOne.B_", k, " + H %*% ModelOne.G_", k),
        name = paste0("Eta_", k)
      )
    } else {
      mxAlgebraFromString(
        paste0("Tpoly %*% ModelOne.B_", k),
        name = paste0("Eta_", k)
      )
    }
  }

  make_lambda_alg <- function(k) {
    if (use_exp_loadings) {
      mxAlgebraFromString(paste0("exp(Eta_", k, ")"), name = paste0("L_", k))
    } else {
      mxAlgebraFromString(paste0("Eta_", k), name = paste0("L_", k))
    }
  }

  make_K_alg <- function(k, Kname) {
    mxAlgebraFromString(paste0("L_", k, " %*% t(L_", k, ")"), name = Kname)
  }

  eta_parts <- lapply(active, function(s) make_eta_alg(s$k))
  lambda_parts <- lapply(active, function(s) make_lambda_alg(s$k))
  K_parts <- lapply(active, function(s) make_K_alg(s$k, s$K))

  eta_e <- make_eta_alg("e")
  lambda_e <- make_lambda_alg("e")
  K_e <- make_K_alg("e", "Ke")

  rel_terms <- vapply(
    active,
    function(s) paste0("(", s$mxname, " * ", s$K, ")"),
    character(1)
  )
  covariance_algebra <- paste(c(rel_terms, "(I * Ke)"), collapse = " + ")

  model_parts <- c(
    list(group_name),
    fixed_parts,
    rel_parts,
    eta_parts,
    lambda_parts,
    K_parts,
    list(
      eta_e,
      lambda_e,
      K_e,
      mxMatrix(
        "Full",
        nrow = 1,
        ncol = fsize,
        name = "M",
        free = TRUE,
        values = 0,
        labels = "mean_y",
        dimnames = list(NULL, obs_ids)
      ),
      mxAlgebraFromString(
        covariance_algebra,
        name = "V",
        dimnames = list(obs_ids, obs_ids)
      ),
      mxExpectationNormal(covariance = "V", means = "M", dimnames = obs_ids),
      mxFitFunctionML()
    )
  )

  do.call(mxModel, model_parts)
}


buildTemporalFamilyGroups <- function(
  dat,
  obs_ids,
  birth_year_list,
  H_list = NULL,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Dmgmat = NULL,
  prefix = "fam",
  use_exp_loadings = FALSE,
  condenseMatrixSlots = TRUE
) {
  .require_openmx("buildTemporalFamilyGroups")

  numfam <- nrow(dat)

  if (is.null(H_list)) {
    H_list <- vector("list", numfam)
  }

  groups <- vector("list", numfam)

  for (afam in seq_len(numfam)) {
    full_df_row <- matrix(
      dat[afam, ],
      nrow = 1,
      dimnames = list(NULL, obs_ids)
    )

    groups[[afam]] <- buildOneTemporalFamilyGroup(
      group_name = paste0(prefix, afam),
      Addmat = Addmat,
      Nucmat = Nucmat,
      Extmat = Extmat,
      Mtdmat = Mtdmat,
      Dmgmat = Dmgmat,
      full_df_row = full_df_row,
      obs_ids = obs_ids,
      birth_year = birth_year_list[[afam]],
      H = H_list[[afam]],
      use_exp_loadings = use_exp_loadings,
      condenseMatrixSlots = condenseMatrixSlots
    )
  }

  groups
}

buildTemporalFamilyGroups_list <- function(
  dat_list,
  obs_ids_list,
  birth_year_list,
  H_list,
  Addmat_list = NULL,
  Nucmat_list = NULL,
  Extmat_list = NULL,
  Mtdmat_list = NULL,
  Dmgmat_list = NULL,
  prefix = "fam",
  use_exp_loadings = FALSE,
  condenseMatrixSlots = TRUE
) {
  .require_openmx("buildTemporalFamilyGroups_list")

  numfam <- length(dat_list)
  groups <- vector("list", numfam)

  get_or_null <- function(x, i) {
    if (is.null(x)) NULL else x[[i]]
  }

  for (afam in seq_len(numfam)) {
    obs_ids <- obs_ids_list[[afam]]

    full_df_row <- matrix(
      dat_list[[afam]],
      nrow = 1,
      dimnames = list(NULL, obs_ids)
    )

    groups[[afam]] <- buildOneTemporalFamilyGroup(
      group_name = paste0(prefix, afam),
      Addmat = get_or_null(Addmat_list, afam),
      Nucmat = get_or_null(Nucmat_list, afam),
      Extmat = get_or_null(Extmat_list, afam),
      Mtdmat = get_or_null(Mtdmat_list, afam),
      Dmgmat = get_or_null(Dmgmat_list, afam),
      full_df_row = full_df_row,
      obs_ids = obs_ids,
      birth_year = birth_year_list[[afam]],
      H = H_list[[afam]],
      use_exp_loadings = use_exp_loadings,
      condenseMatrixSlots = condenseMatrixSlots
    )
  }

  groups
}




buildTemporalPedigreeMx <- function(
    model_name,
    group_models,
    p_hist,
    components = c("a", "e"),
    ci = FALSE
) {
  group_names <- vapply(group_models, function(m) m$name, character(1))
  components <- unique(c(components, "e"))

  model_one <- buildTemporalPedigreeModelCovariance(
    p_hist = p_hist,
    components = components
  )

  model_parts <- c(
    list(model_name),
    list(model_one),
    group_models,
    list(mxFitFunctionMultigroup(group_names))
  )

  if (ci) {
    ci_names <- unlist(lapply(components, function(k) {
      out <- paste0("b_", k, "_", 0:3)
      if (p_hist > 0) out <- c(out, paste0("g_", k, "_", seq_len(p_hist)))
      out
    }))
    model_parts[[length(model_parts) + 1]] <- mxCI(ci_names)
  }

  do.call(mxModel, model_parts)
}


fitTemporalPedigreeModel <- function(
  model_name = "TemporalPedigreeModel",
  group_models = NULL,
  dat_list = NULL,
  obs_ids_list = NULL,
  birth_year_list = NULL,
  H_list = NULL,
  Addmat_list = NULL,
  Nucmat_list = NULL,
  Extmat_list = NULL,
  Mtdmat_list = NULL,
  Dmgmat_list = NULL,
  p_hist = NULL,
  components = c("a", "d", "cn", "ce", "mt", "e"),
  use_exp_loadings = FALSE,
  tryhard = TRUE,
  intervals = TRUE,
  extraTries = 10,
  runmodel = TRUE
) {
  .require_openmx("fitTemporalPedigreeModel")

  if (is.null(group_models)) {
    if (is.null(dat_list) || is.null(obs_ids_list) || is.null(birth_year_list)) {
      stop("Provide either 'group_models' or dat_list, obs_ids_list, and birth_year_list.")
    }

    if (is.null(H_list)) {
      H_list <- lapply(birth_year_list, function(x) matrix(numeric(0), nrow = length(x), ncol = 0))
    }

    group_models <- buildTemporalFamilyGroups_list(
      dat_list = dat_list,
      obs_ids_list = obs_ids_list,
      birth_year_list = birth_year_list,
      H_list = H_list,
      Addmat_list = Addmat_list,
      Nucmat_list = Nucmat_list,
      Extmat_list = Extmat_list,
      Mtdmat_list = Mtdmat_list,
      Dmgmat_list = Dmgmat_list,
      use_exp_loadings = use_exp_loadings
    )
  }

  if (is.null(p_hist)) {
    if (!is.null(H_list) && length(H_list) > 0 && !is.null(H_list[[1]])) {
      p_hist <- ncol(H_list[[1]])
    } else {
      p_hist <- 0
    }
  }

  temporal_model <- buildTemporalPedigreeMx(
    model_name = model_name,
    group_models = group_models,
    p_hist = p_hist,
    components = components,
    ci = intervals
  )

  if (!runmodel) return(temporal_model)

  if (tryhard) {
    OpenMx::mxTryHard(
      temporal_model,
      silent = TRUE,
      extraTries = extraTries,
      intervals = intervals
    )
  } else {
    OpenMx::mxRun(
      temporal_model,
      intervals = intervals
    )
  }
}
