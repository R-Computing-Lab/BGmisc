#' Create an mxModel for a pedigree
#'
#' This function builds an OpenMx model for a pedigree with specified variance components. It requires the OpenMx package.
#'
#' @param vars A named list or vector of initial variance component values. Names should include
#'   ad2 (additive), dd2 (dominance), cn2 (common nuclear), ce2 (common extended),
#'   mt2 (mitochondrial), am2 (additive-mitochondrial interaction), and ee2 (unique environment).
#'   Default values are provided. Only used when \code{temporal = FALSE}.
#' @param Vad Logical. Include additive genetic variance component. Default is TRUE.
#' @param Vdd Logical. Include dominance genetic variance component. Default is FALSE.
#' @param Vcn Logical. Include common nuclear family environment variance component. Default is TRUE.
#' @param Vce Logical. Include common extended family environment variance component. Default is TRUE.
#' @param Vmt Logical. Include mitochondrial genetic variance component. Default is TRUE.
#' @param Vam Logical. Include additive by mitochondrial interaction variance component. Default is FALSE.
#' @param Ver Logical. Include unique environmental variance component. Default is TRUE.
#' @param temporal Logical. If TRUE, build a time-varying covariance sub-model in which each
#'   component's weight is a function of birth year (and, optionally, historical moderators)
#'   rather than a fixed scalar. See \code{p_hist}, \code{components}, \code{start_beta0},
#'   \code{start_beta_time}, \code{start_gamma}, and \code{time_point_max}. Default is FALSE.
#' @param p_hist Integer. Number of historical moderator columns. Only used when
#'   \code{temporal = TRUE}. Default is 0.
#' @param components Character vector of component keys to include when \code{temporal = TRUE}
#'   (any of "a", "d", "cn", "ce", "mt", "am", "e"). Default is \code{c("a", "e")}.
#' @param start_beta0 Numeric. Starting value for each component's intercept loading. Only used
#'   when \code{temporal = TRUE}. Default is 0.5.
#' @param start_beta_time Numeric. Starting value for each component's time-slope loadings. Only
#'   used when \code{temporal = TRUE}. Default is 0.
#' @param start_gamma Numeric. Starting value for historical-moderator loadings. Only used when
#'   \code{temporal = TRUE}. Default is 0.
#' @param time_point_max Integer. Degree of the polynomial birth-year basis. Only used when
#'   \code{temporal = TRUE}. Default is 3.
#' @param lbound Numeric. A lower bound for the variance components to ensure they remain positive during optimization. Default is 1e-10
#' @return An OpenMx model representing the pedigree with specified variance components.
#' @export
# Todo: add parallize options: mxOption(model= yourModel, key="Number of Threads", value= (omxDetectCores() - 1))



buildPedigreeModelCovariance <- function(
  vars = list(
    ad2 = 0.5,
    dd2 = 0.3,
    cn2 = 0.2,
    ce2 = 0.4,
    mt2 = 0.1,
    am2 = 0.25,
    ee2 = 0.6
  ),
  Vad = TRUE,
  Vdd = FALSE, # check into confirming that variance components are directly estimated
  Vcn = TRUE,
  Vce = TRUE,
  Vmt = TRUE,
  Vam = FALSE,
  Ver = TRUE,
  temporal = FALSE,
  p_hist = 0,
  components = c("a", "e"),
  start_beta0 = 0.5,
  start_beta_time = 0,
  start_gamma = 0,
  time_point_max = NULL,
  mean_degree = 0,
  start_mean = 0,
  mean_hist_free = FALSE,
  start_mean_hist = 0,
  lbound = 1e-10
) {
  .require_openmx("buildPedigreeModelCovariance")

  if (temporal) {
    return(.buildTemporalLoadingModel(
      p_hist = p_hist,
      components = components,
      start_beta0 = start_beta0,
      start_beta_time = start_beta_time,
      start_gamma = start_gamma,
      time_point_max = time_point_max,
      mean_degree = mean_degree,
      start_mean = start_mean,
      mean_hist_free = mean_hist_free,
      start_mean_hist = start_mean_hist
    ))
  }

  # Coerce to list so both c() vectors and list() inputs work with [[ ]]
  vars <- as.list(vars)

  # Declare all possible variance components as (name, label, vars_key) triples,
  # then filter to only those that are enabled. This avoids the repeated
  # c(mat_list, list(...)) accumulation pattern and makes the component table
  # easy to extend.
  vc_spec <- list(
    list(
      flag = Vad, name = "Vad", label = "vad",
      key = "ad2",
      lbound = lbound
    ),
    list(
      flag = Vdd, name = "Vdd", label = "vdd", key = "dd2",
      lbound = lbound
    ),
    list(
      flag = Vcn, name = "Vcn", label = "vcn", key = "cn2",
      lbound = lbound
    ),
    list(
      flag = Vce, name = "Vce", label = "vce", key = "ce2",
      lbound = lbound
    ),
    list(
      flag = Vmt, name = "Vmt", label = "vmt", key = "mt2",
      lbound = lbound
    ),
    list(
      flag = Vam, name = "Vam", label = "vam", key = "am2",
      lbound = lbound
    ),
    list(
      flag = Ver, name = "Ver", label = "ver", key = "ee2",
      lbound = lbound
    )
  )

  mat_list <- lapply(
    Filter(function(s) isTRUE(s$flag), vc_spec),
    function(s) {
      OpenMx::mxMatrix(
        type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars[[s$key]], labels = s$label, name = s$name, lbound = s$lbound
      )
    }
  )

  do.call(OpenMx::mxModel, c(list("ModelOne"), mat_list))
}

#' Build the temporal loading sub-model (internal)
#'
#' Builds the parent \code{ModelOne} sub-model for a time-varying pedigree model: for each
#' requested component, a vector of polynomial-time loadings (\code{B_k}) and, when
#' \code{p_hist > 0}, a vector of historical-moderator loadings (\code{G_k}). Also builds
#' the mean's coefficient vector (\code{B_mean}, and \code{G_mean} when \code{p_hist > 0}),
#' which lets the phenotypic mean vary over the same basis as the loadings.
#'
#' Named for what it builds rather than mirroring the exported
#' \code{\link{buildTemporalPedigreeModelCovariance}}, which differed from it by a single
#' leading dot.
#'
#' @param p_hist Integer. Number of historical moderator columns. Default is 0.
#' @param components Character vector of component keys (any of "a", "d", "cn", "ce", "mt",
#'   "am", "e").
#' @param start_beta0 Numeric starting value for each component's intercept loading.
#' @param start_beta_time Numeric starting value for each component's time-slope loadings.
#' @param start_gamma Numeric starting value for historical-moderator loadings.
#' @param time_point_max Integer degree of the polynomial time basis. Defaults to 3 when NULL.
#' @param mean_degree Integer degree of the temporal mean. 0 (the default) frees only the
#'   intercept, which reproduces a single constant mean. Must not exceed
#'   \code{time_point_max}, since the mean shares the components' \code{Tpoly} basis.
#' @param start_mean Numeric starting value for the mean's intercept. Set this near the
#'   observed phenotype mean; the optimizer starts from it.
#' @param mean_hist_free Logical, length 1 or \code{p_hist}. Whether each historical
#'   moderator is allowed to shift the phenotypic mean. Defaults to FALSE, which
#'   reproduces the previous behaviour of fixing every mean shift at zero. Set TRUE
#'   to estimate them. Note that leaving these fixed while the components' own
#'   \code{G_*} coefficients are free forces any event-driven change in the mean to
#'   be absorbed by the variance components.
#' @param start_mean_hist Numeric starting value for the free elements of
#'   \code{G_mean}.
#' @return An OpenMx model containing the \code{B_*}/\code{G_*} parameter matrices.
#' @keywords internal
.buildTemporalLoadingModel <- function(
  p_hist = 0,
  components = c("a", "e"),
  start_beta0 = 0.5,
  start_beta_time = 0,
  start_gamma = 0,
  time_point_max = NULL,
  mean_degree = 0,
  start_mean = 0,
  mean_hist_free = FALSE,
  start_mean_hist = 0
) {
  .require_openmx(".buildTemporalLoadingModel")

  beta_name <- function(k) paste0("B_", k)
  gamma_name <- function(k) paste0("G_", k)

  if (is.null(time_point_max)) {
    time_point_max <- 3
  }
  if (!is.numeric(time_point_max) || length(time_point_max) != 1 || time_point_max < 0) {
    stop("time_point_max must be a non-negative numeric scalar.")
  }
  time_points <- 0:time_point_max

  mats <- list()

  for (k in components) {
    mats[[beta_name(k)]] <- OpenMx::mxMatrix(
      type = "Full",
      nrow = time_point_max + 1,
      ncol = 1,
      free = TRUE,
      values = c(start_beta0, rep(start_beta_time, time_point_max)),
      labels = paste0("b_", k, "_", time_points),
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

  if (!is.numeric(mean_degree) || length(mean_degree) != 1L ||
    mean_degree < 0 || mean_degree != as.integer(mean_degree)) {
    stop("mean_degree must be a non-negative whole number.")
  }
  if (mean_degree > time_point_max) {
    stop("mean_degree cannot exceed time_point_max; the mean shares the Tpoly basis.")
  }

  # The mean is always expressed over the same Tpoly basis as the loadings, so the
  # family groups need no branch: mean_degree just controls how many of its
  # coefficients are free. At the default of 0 only the intercept is free and the
  # expression evaluates to one constant for every family member, which is what the
  # previous fixed mean matrix did. The intercept keeps the "mean_y" label so
  # existing summaries and downstream parsers still resolve it.
  mats[["B_mean"]] <- OpenMx::mxMatrix(
    type = "Full",
    nrow = time_point_max + 1,
    ncol = 1,
    free = c(rep(TRUE, mean_degree + 1), rep(FALSE, time_point_max - mean_degree)),
    values = c(start_mean, rep(0, time_point_max)),
    labels = c("mean_y", paste0("b_mean_", time_points[-1])),
    name = "B_mean"
  )

  # Fixed at zero by default, for backward compatibility. Freeing these tests
  # whether a historical event shifts the mean as well as the variance components.
  # Recycled to length p_hist so a single TRUE/FALSE applies to all moderators
  # while a vector frees them selectively: a pandemic indicator plausibly shifts
  # mean lifespan, whereas some other regime indicator may shift only the
  # variance, and the two need not share one decision.
  if (p_hist > 0) {
    mean_hist_free <- .recycleFlag(mean_hist_free, p_hist, "mean_hist_free")

    mats[["G_mean"]] <- OpenMx::mxMatrix(
      type = "Full",
      nrow = p_hist,
      ncol = 1,
      free = mean_hist_free,
      values = rep(start_mean_hist, length.out = p_hist),
      labels = paste0("g_mean_", seq_len(p_hist)),
      name = "G_mean"
    )
  } else if (any(mean_hist_free)) {
    warning("mean_hist_free was supplied but p_hist is 0; ignoring.", call. = FALSE)
  }

  do.call(OpenMx::mxModel, c(list("ModelOne"), mats))
}

#' Recycle a logical flag to a required length (internal)
#'
#' @param x A logical vector of length 1 or \code{n}.
#' @param n Required length.
#' @param arg_name Name of the calling argument, used in error messages.
#' @return A logical vector of length \code{n}.
#' @keywords internal
.recycleFlag <- function(x, n, arg_name) {
  if (!is.logical(x) || anyNA(x)) {
    stop(sprintf("%s must be logical and non-missing.", arg_name))
  }
  if (length(x) == 1L) {
    return(rep(x, n))
  }
  if (length(x) != n) {
    stop(sprintf(
      "%s must be length 1 or length p_hist (%d); got %d.",
      arg_name, n, length(x)
    ))
  }
  x
}

#' Determine family size from a family's relatedness matrices (internal)
#'
#' @param mats_in A list of relatedness matrices (possibly containing NULLs); the first
#'   non-NULL matrix's row count is used as the family size.
#' @return Integer family size.
#' @keywords internal
.pedigreeFamilySize <- function(mats_in) {
  for (m in mats_in) {
    if (!is.null(m)) {
      return(nrow(m))
    }
  }
  stop("At least one relatedness matrix must be provided.")
}

#' Build a relatedness mxMatrix (internal)
#'
#' Builds the \code{"Symm"} mxMatrix for one relatedness matrix. Shared by the static
#' and temporal branches of \code{\link{buildOneFamilyGroup}}: the only difference
#' between them is whether the input matrix is symmetrized first (temporal) or used
#' as-is (static, to preserve its existing behavior exactly).
#'
#' @param mat The relatedness matrix.
#' @param fsize Family size.
#' @param name The mxMatrix's name (e.g. "A", "Cn").
#' @param condense Logical. If TRUE, apply \code{\link{condenseMatrixSlots}}.
#' @param symmetrize Logical. If TRUE, symmetrize \code{mat} via \code{\link{make_symmetric}}
#'   before use; if FALSE, coerce via \code{as.matrix} only.
#' @return An mxMatrix object.
#' @keywords internal
.pedigreeRelatednessMatrix <- function(mat, fsize, name, condense = TRUE, symmetrize = FALSE) {
  if (is.null(mat)) stop("Relatedness matrix cannot be NULL.")
  # Symmetrize if requested, then coerce to a base dense matrix. OpenMx::mxMatrix()
  # accepts only a scalar, vector, or base R matrix for 'values'; a Matrix-package
  # sparse object (e.g. the dsCMatrix returned by ped2add(sparse = TRUE)) is rejected
  # by matrixCheckArgument(), so it must be densified here.
  values <- if (symmetrize) make_symmetric(mat) else mat
  values <- as.matrix(values)
  # todo allow this to be sparse and use sparse algebra in OpenMx
  m <- tryCatch(OpenMx::mxMatrix(
    type = "Symm", nrow = fsize, ncol = fsize, free = FALSE,
    values = values, name = name
  ), error = function(e) {
    print(values)
    stop("Error creating mxMatrix for ", name, ": ", e$message)
  })

  if (condense) m <- condenseMatrixSlots(m)
  m
}

#' Build the free mean mxMatrix for a family group (internal)
#'
#' Used by the static branches of \code{\link{buildOneFamilyGroup}}. The temporal branch
#' instead builds its mean as an algebra over \code{Tpoly} and \code{ModelOne.B_mean}, so
#' that the mean can vary across historical time.
#'
#' @param fsize Family size.
#' @param obs_ids Character vector of individual IDs.
#' @param label The mean parameter's label.
#' @return An mxMatrix object.
#' @keywords internal
.pedigreeMeanMatrix <- function(fsize, obs_ids, label) {
  OpenMx::mxMatrix(
    "Full",
    nrow = 1, ncol = fsize, name = "M", free = TRUE,
    values = 0, labels = label, dimnames = list(NULL, obs_ids)
  )
}

#' Build the temporal mean part for a family group (internal)
#'
#' @inheritParams .pedigreeMeanMatrix
#' @param label The mean parameter's label, used only for the constant-mean case.
#' @param mean_basis NULL for a single free mean (default), or the name of a fixed basis
#'   matrix in the group model whose columns the mean is regressed on.
#' @param mean_hist NULL, or the name of the historical-moderator matrix to add to the mean.
#' @param parent Name of the sub-model holding \code{B_mean}/\code{G_mean}.
#' @return An mxMatrix or mxAlgebra named "M".
#' @keywords internal
.pedigreeMeanPart <- function(fsize,
                              obs_ids,
                              label,
                              mean_basis = NULL,
                              mean_hist = NULL,
                              parent = "ModelOne") {
  if (is.null(mean_basis)) {
    return(.pedigreeMeanMatrix(fsize, obs_ids, label))
  }

  expr <- paste0(mean_basis, " %*% ", parent, ".B_mean")
  if (!is.null(mean_hist)) {
    expr <- paste0(expr, " + ", mean_hist, " %*% ", parent, ".G_mean")
  }

  # t() because M must be 1 x fsize while the basis product is fsize x 1.
  OpenMx::mxAlgebraFromString(
    paste0("t(", expr, ")"),
    name = "M",
    dimnames = list(NULL, obs_ids)
  )
}


#' Build one family group model
#'
#' This function constructs an OpenMx model for a single family group based on
#' provided relatedness matrices and observed data. The implied covariance
#' is built as a weighted sum of the supplied relatedness matrices, where
#' the weights are variance component parameters shared across groups via
#' a parent \code{ModelOne} sub-model.
#'
#' @param group_name Name of the family group.
#' @param Addmat Additive genetic relatedness matrix (from \code{\link{ped2add}}).
#' @param Nucmat Nuclear family shared environment relatedness matrix (from \code{\link{ped2cn}}).
#' @param Extmat Common extended family environment relatedness matrix. When non-NULL,
#'   a Vce term scaled by this matrix is added to the covariance. If a non-matrix
#'   value (e.g. \code{TRUE}) is supplied, a unit matrix (all members share equally)
#'   is created automatically.
#' @param Mtdmat Mitochondrial genetic relatedness matrix (from \code{\link{ped2mit}}).
#' @param Amimat Additive by mitochondrial interaction relatedness matrix.
#' @param Dmgmat Dominance genetic relatedness matrix.
#' @param full_df_row A 1-row matrix of observed data with column names matching \code{obs_ids}.
#' @param obs_ids A character vector of individual IDs corresponding to the columns of
#'   \code{full_df_row} and the rows/columns of the relatedness matrices. Must be in the
#'   same order as the relatedness matrix rows.
#' @param condenseMatrixSlots Logical. If TRUE, use the mxCondenseMatrixSlots wrapper to optimize memory usage for large matrices. Default is TRUE.
#' @param temporal Logical. If TRUE, build a time-varying family-group model where each
#'   component's weight is a function of \code{param_year} (and, optionally, \code{H}) instead
#'   of a fixed scalar. Default is FALSE.
#' @param param_year Numeric vector of birth years (or another time index), one per member of
#'   the family, in the same order as \code{obs_ids}. Only used when \code{temporal = TRUE}.
#' @param H Optional numeric matrix of historical moderators, one row per family member (same
#'   order as \code{obs_ids}), one column per moderator. Only used when \code{temporal = TRUE}.
#' @param use_exp_loadings Logical. If TRUE (default), each component's loading is
#'   exponentiated so its implied variance stays positive. Only used when \code{temporal = TRUE}.
#' @param time_point_max Integer degree of the polynomial birth-year basis. Only used when
#'   \code{temporal = TRUE}. Default is 3.
#' @param retain_eta Logical. Retain named temporal linear predictors \code{Eta_*}.
#' @param retain_loadings Logical. Retain named loading vectors \code{L_*}.
#' @param retain_loading_covariances Logical. Retain named loading outer products \code{K*}.
#' @param retain_component_covariances Logical. Retain named relatedness-weighted component
#'   covariance algebras \code{Cov_*}. Any unretained layer is inlined, leaving \code{V}
#'   and the likelihood unchanged.
#' @param residual_covariance_form Character. Use \code{"outer_product"} to preserve the
#'   original \code{I * K_e} representation or \code{"diagonal"} for the exactly equivalent
#'   \code{vec2diag(L_e * L_e)} representation.
#' @param clean_ids Logical. If TRUE, clean the \code{obs_ids} to be syntactically valid R names using \code{make.names}. Default is FALSE.
#' @return An OpenMx model for the specified family group.
#' @export

buildOneFamilyGroup <- function(
  group_name,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  full_df_row,
  obs_ids,
  condenseMatrixSlots = TRUE,
  temporal = FALSE,
  param_year = NULL,
  H = NULL,
  use_exp_loadings = FALSE,
  time_point_max = NULL,
  retain_eta = TRUE,
  retain_loadings = TRUE,
  retain_loading_covariances = TRUE,
  retain_component_covariances = TRUE,
  residual_covariance_form = c("outer_product", "diagonal"),
  clean_ids = FALSE
) {
  .require_openmx("buildOneFamilyGroup")
  if (clean_ids) {
    obs_ids <- make_clean_personids(obs_ids)
    # clean once
    clean_ids <- FALSE
  }
  # Determine family size from first available matrix. Shared by both branches below.
  if ( # not any of the matrices are provided
    is.null(Addmat) && is.null(Dmgmat) && is.null(Nucmat) &&
      is.null(Extmat) && is.null(Mtdmat) && is.null(Amimat)
  ) {
    warning("At least one relatedness matrix should be provided. Using the number of columns in 'full_df_row' as family size.")
    fsize <- ncol(full_df_row)
  } else {
    fsize <- .pedigreeFamilySize(list(Addmat, Dmgmat, Nucmat, Extmat, Mtdmat, Amimat))
  }
  # If Extmat is requested but not supplied as a matrix, create a unit matrix
  # (all members share the extended environment equally).
  if (!is.null(Extmat) && !is.matrix(Extmat)) {
    Extmat <- matrix(1, nrow = fsize, ncol = fsize)
  }

  # One canonical table describing each variance component: which relatedness matrix it
  # uses, its mxMatrix name, the exact static Kronecker algebra term (term, unchanged from
  # the original static implementation), and the temporal loading/loading-matrix names
  # (k / K). Both the static and temporal algebra below are built from this same table and
  # the same relatedness mxMatrix objects (relmat_list), rather than each branch rebuilding
  # its own copy of this table.
  mat_spec <- list(
    list(
      mat = Addmat,
      mxname = "A",
      term = "(A  %x% ModelOne.Vad)",
      k = "a", K = "Ka"
    ),
    list(
      mat = Dmgmat,
      mxname = "D",
      term = "(D  %x% ModelOne.Vdd)",
      k = "d", K = "Kd"
    ),
    list(
      mat = Nucmat,
      mxname = "Cn",
      term = "(Cn %x% ModelOne.Vcn)",
      k = "cn", K = "Kcn"
    ),
    list(
      mat = Extmat,
      mxname = "Ce",
      term = "(Ce %x% ModelOne.Vce)",
      k = "ce", K = "Kce"
    ),
    list(
      mat = Amimat,
      mxname = "Am",
      term = "(Am %x% ModelOne.Vam)",
      k = "am", K = "Kam"
    ),
    list(
      mat = Mtdmat,
      mxname = "Mt",
      term = "(Mt %x% ModelOne.Vmt)",
      k = "mt", K = "Kmt"
    )
  )
  active <- Filter(function(s) !is.null(s$mat), mat_spec)

  # Static values are used as-is (as.matrix); temporal symmetrizes them first. Either
  # way, this single loop replaces what used to be two separate (and, for static,
  # itself duplicated condensed/uncondensed) copies of the same mxMatrix-building code.
  relmat_list <- lapply(active, function(s) {
    .pedigreeRelatednessMatrix(
      s$mat, fsize, s$mxname,
      condense = condenseMatrixSlots, symmetrize = temporal
    )
  })

  if (temporal) {
    return(.temporalFamilyGroupAlgebra(
      group_name = group_name,
      fsize = fsize,
      active = active,
      relmat_list = relmat_list,
      full_df_row = full_df_row,
      obs_ids = obs_ids,
      param_year = param_year,
      H = H,
      use_exp_loadings = use_exp_loadings,
      condenseMatrixSlots = condenseMatrixSlots,
      time_point_max = time_point_max,
      retain_eta = retain_eta,
      retain_loadings = retain_loadings,
      retain_loading_covariances = retain_loading_covariances,
      retain_component_covariances = retain_component_covariances,
      residual_covariance_form = residual_covariance_form
    ))
  }

  # add the identity matrix for the unique environment, which is always included as a term in the algebra
  mat_list <- c(
    list(OpenMx::mxMatrix("Iden", nrow = fsize, ncol = fsize, name = "I")),
    relmat_list
  )

  algebra_terms <- vapply(active, `[[`, character(1), "term")

  # Unique environment is always included
  algebra_terms <- c(algebra_terms, "(I %x% ModelOne.Ver)")

  algebra_str <- paste(algebra_terms, collapse = " + ")

  # Assemble the model via do.call so that the dynamic mat_list is unpacked
  model_args <- c(
    list(name = group_name),
    mat_list,
    list(
      OpenMx::mxData(observed = full_df_row, type = "raw", sort = FALSE),
      .pedigreeMeanMatrix(fsize= fsize, obs_ids= obs_ids, label= "meanLI"),
      OpenMx::mxAlgebraFromString(algebra_str,
        name = "V", dimnames = list(obs_ids, obs_ids)
      ),
      OpenMx::mxExpectationNormal(covariance = "V", means = "M"),
      OpenMx::mxFitFunctionML()
    )
  )

  do.call(OpenMx::mxModel, model_args)
}

#' Build one grouped static family model
#'
#' This function constructs one OpenMx model for multiple independent family
#' observations that share the same pedigree structure, relatedness matrices,
#' variable ordering, expected covariance matrix, and expected mean vector.
#'
#' The observed data contain one family per row. The shared matrices, covariance
#' algebra, expectation, and fit function are stored only once rather than once
#' per family.
#'
#' @param group_name Name of the grouped family model.
#' @param dat A matrix or data frame where each row represents an independent
#'   family and columns correspond to pedigree positions.
#' @param obs_ids A character vector of individual IDs corresponding to the
#'   columns of \code{dat} and the rows/columns of the relatedness matrices.
#' @inheritParams buildOneFamilyGroup
#' @return An OpenMx model containing all static family observations.
#' @keywords internal
.buildGroupedStaticFamily <- function(
  group_name,
  dat,
  obs_ids,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  condenseMatrixSlots = TRUE,
  clean_ids = FALSE
) {
  .require_openmx(".buildGroupedStaticFamily")

  if (clean_ids) {
    obs_ids <- make_clean_personids(obs_ids)
    # clean once
    clean_ids <- FALSE
  }

  dat <- as.data.frame(dat, check.names = FALSE)

  if (nrow(dat) < 1L) {
    stop("'dat' must contain at least one family row.")
  }

  if (ncol(dat) != length(obs_ids)) {
    stop("The number of columns in 'dat' must equal the length of 'obs_ids'.")
  }

  if (anyDuplicated(obs_ids)) {
    stop("'obs_ids' must be unique.")
  }

  colnames(dat) <- obs_ids

  # Determine family size from first available matrix.
  fsize <- .pedigreeFamilySize(list(Addmat, Dmgmat, Nucmat, Extmat, Mtdmat, Amimat))

  if (length(obs_ids) != fsize) {
    stop("Length of 'obs_ids' must equal the shared family size.")
  }

  # If Extmat is requested but not supplied as a matrix, create a unit matrix
  # (all members share the extended environment equally).
  if (!is.null(Extmat) && !is.matrix(Extmat)) {
    Extmat <- matrix(1, nrow = fsize, ncol = fsize)
  }

  # This is the same static variance-component specification used by
  # buildOneFamilyGroup(), but it is constructed only once for all rows of dat.
  mat_spec <- list(
    list(
      mat = Addmat,
      mxname = "A",
      term = "(A  %x% ModelOne.Vad)"
    ),
    list(
      mat = Dmgmat,
      mxname = "D",
      term = "(D  %x% ModelOne.Vdd)"
    ),
    list(
      mat = Nucmat,
      mxname = "Cn",
      term = "(Cn %x% ModelOne.Vcn)"
    ),
    list(
      mat = Extmat,
      mxname = "Ce",
      term = "(Ce %x% ModelOne.Vce)"
    ),
    list(
      mat = Amimat,
      mxname = "Am",
      term = "(Am %x% ModelOne.Vam)"
    ),
    list(
      mat = Mtdmat,
      mxname = "Mt",
      term = "(Mt %x% ModelOne.Vmt)"
    )
  )

  active <- Filter(function(s) !is.null(s$mat), mat_spec)

  # Build one copy of each shared relatedness matrix.
  relmat_list <- lapply(active, function(s) {
    .pedigreeRelatednessMatrix(
      s$mat,
      fsize,
      s$mxname,
      condense = condenseMatrixSlots,
      symmetrize = FALSE
    )
  })

  # Add one identity matrix for the unique environment.
  mat_list <- c(
    list(OpenMx::mxMatrix("Iden", nrow = fsize, ncol = fsize, name = "I")),
    relmat_list
  )

  algebra_terms <- vapply(active, `[[`, character(1), "term")

  # Unique environment is always included.
  algebra_terms <- c(algebra_terms, "(I %x% ModelOne.Ver)")

  algebra_str <- paste(algebra_terms, collapse = " + ")

  # The full data frame is passed to one raw-data model. Each row is an
  # independent family likelihood contribution under the shared M and V.
  model_args <- c(
    list(name = group_name),
    mat_list,
    list(
      OpenMx::mxData(
        observed = dat,
        type = "raw",
        sort = FALSE
      ),
      .pedigreeMeanMatrix(
       fsize= fsize,
      obs_ids=  obs_ids,
       label= "meanLI"
      ),
      OpenMx::mxAlgebraFromString(
        algebra_str,
        name = "V",
        dimnames = list(obs_ids, obs_ids)
      ),
      OpenMx::mxExpectationNormal(
        covariance = "V",
        means = "M",
        dimnames = obs_ids
      ),
      OpenMx::mxFitFunctionML()
    )
  )

  do.call(OpenMx::mxModel, model_args)
}

#' Build the temporal covariance algebra for one family group (internal)
#'
#' Given a family's already-detected size, already-filtered component table, and
#' already-built relatedness mxMatrix objects (all shared with the static branch via
#' \code{\link{buildOneFamilyGroup}}), builds the time-varying covariance algebra: each
#' component's relatedness matrix is combined (via a Hadamard product) with a loading
#' matrix derived from a polynomial birth-year basis and, optionally, historical
#' moderators.
#'
#' @param group_name Name of the family group.
#' @param fsize Family size (number of members), as computed by \code{\link{buildOneFamilyGroup}}.
#' @param active The filtered component table built by \code{\link{buildOneFamilyGroup}}
#'   (one entry per relatedness matrix actually supplied), used for its \code{k} loading
#'   key and \code{mxname} relatedness-matrix name.
#' @param relmat_list The list of already-built (and already condensed, if requested)
#'   \code{"Symm"} mxMatrix objects for \code{active}, in the same order.
#' @param full_df_row A 1-row matrix/vector of observed data.
#' @param obs_ids Character vector of individual IDs, matching \code{full_df_row}.
#' @param param_year Numeric vector of birth years, one per family member, matching \code{obs_ids}.
#' @param H Optional numeric matrix of historical moderators.
#' @param use_exp_loadings Logical. If TRUE, each component's loading is exponentiated.
#' @param condenseMatrixSlots Logical. If TRUE, condense the \code{Tpoly}/\code{H} mxMatrix objects.
#' @param time_point_max Integer degree of the polynomial birth-year basis.
#' @param retain_eta Logical. Retain named \code{Eta_*} algebras.
#' @param retain_loadings Logical. Retain named \code{L_*} algebras.
#' @param retain_loading_covariances Logical. Retain named \code{K*} outer-product algebras.
#' @param retain_component_covariances Logical. Retain named \code{Cov_*} algebras.
#' @param residual_covariance_form Character representation for the residual covariance.
#' @return An OpenMx model for the specified family group.
#' @keywords internal
.temporalFamilyGroupAlgebra <- function(
  group_name,
  fsize,
  active,
  relmat_list,
  full_df_row,
  obs_ids,
  param_year,
  H = NULL,
  use_exp_loadings = TRUE,
  condenseMatrixSlots = TRUE,
  time_point_max = NULL,
  retain_eta = TRUE,
  retain_loadings = TRUE,
  retain_loading_covariances = TRUE,
  retain_component_covariances = TRUE,
  residual_covariance_form = c("outer_product", "diagonal"),
  mean_degree = 0,
  start_mean = 0,
  mean_basis = "Tpoly",
  mean_hist = "H"
) {
  # Checks
  if (length(obs_ids) != fsize) stop("Length of obs_ids must equal family size.")
  if (length(param_year) != fsize) stop("Length of param_year must equal family size.")

  # retain checks
  residual_covariance_form <- match.arg(residual_covariance_form)

  retain_flags <- c(
    retain_eta = retain_eta,
    retain_loadings = retain_loadings,
    retain_loading_covariances = retain_loading_covariances,
    retain_component_covariances = retain_component_covariances
  )
  if (any(lengths(as.list(retain_flags)) != 1L) || any(is.na(retain_flags))) {
    stop("All retain_* arguments must be non-missing logical scalars.")
  }
  # do I really need this data structure? I think I can just use the individual flags directly, but for now I'll keep it.
  # this lets me drop all of the estimates I don't need
  retain_flags_nmd <- retain_flags
  retain_flags <- as.logical(retain_flags)
  retain_eta <- unname(retain_flags_nmd[["retain_eta"]])
  retain_loadings <- unname(retain_flags_nmd[["retain_loadings"]])
  retain_loading_covariances <- unname(retain_flags_nmd[["retain_loading_covariances"]])
  retain_component_covariances <- unname(retain_flags_nmd[["retain_component_covariances"]])


  if (is.null(H)) {
    H <- matrix(numeric(0), nrow = fsize, ncol = 0)
  } else {
    H <- as_numeric_matrix(H)
    if (nrow(H) != fsize) stop("H must have nrow equal to family size.")
  }
  p_hist <- ncol(H)

  if (is.null(time_point_max)) {
    time_point_max <- 3
  }
  if (!is.numeric(time_point_max) || length(time_point_max) != 1 || time_point_max < 0) {
    stop("time_point_max must be a non-negative numeric scalar.")
  }

  # One raw-data row with named phenotype columns.
  full_df_row <- matrix(as.numeric(full_df_row), nrow = 1)
  colnames(full_df_row) <- obs_ids
  rownames(full_df_row) <- group_name
  full_df_row <- as.data.frame(full_df_row, check.names = FALSE)
  # stopifnot(identical(colnames(full_df_row), obs_ids))

  t_i <- as.numeric(param_year)
  # Polynomial basis of degree time_point_max: columns are t_i^0, t_i^1, ..., t_i^time_point_max.
  if (time_point_max == 3) {
    # default
    Tpoly <- cbind(1, t_i, t_i^2, t_i^3)
  } else {
    Tpoly <- sapply(0:time_point_max, function(p) t_i^p)
  }

  Tpoly_mat <- OpenMx::mxMatrix(
    "Full",
    nrow = fsize, ncol = time_point_max + 1, free = FALSE, values = Tpoly, name = "Tpoly"
  )
  if (condenseMatrixSlots) Tpoly_mat <- condenseMatrixSlots(Tpoly_mat)

  fixed_parts <- list(
    OpenMx::mxData(observed = full_df_row, type = "raw", sort = FALSE),
    OpenMx::mxMatrix("Iden", nrow = fsize, ncol = fsize, name = "I"),
    Tpoly_mat
  )

  if (p_hist > 0) {
    H_mat <- OpenMx::mxMatrix(
      "Full",
      nrow = fsize, ncol = p_hist, free = FALSE, values = H, name = "H"
    )
    if (condenseMatrixSlots) H_mat <- condenseMatrixSlots(H_mat)
    fixed_parts[[length(fixed_parts) + 1]] <- H_mat
  }

  # Eta_k = Tpoly %*% B_k + H %*% G_k


  # These helpers construct the same covariance model under every retention combination.
  # A retained object is referenced by name downstream. An unretained object is inlined
  # into the next expression, so retention changes inspectability and object size only.


  eta_expression <- function(k) {
    if (p_hist > 0) {
      paste0("Tpoly %*% ModelOne.B_", k, " + H %*% ModelOne.G_", k)
    } else {
      paste0("Tpoly %*% ModelOne.B_", k)
    }
  }

  eta_reference <- function(k) {
    if (retain_eta) paste0("Eta_", k) else paste0("(", eta_expression(k), ")")
  }

  loading_expression <- function(k) {
    eta_ref <- eta_reference(k)
    if (use_exp_loadings) paste0("exp(", eta_ref, ")") else eta_ref
  }

  # # Eta_k = Tpoly %*% B_k + H %*% G_k
  loading_reference <- function(k) {
    if (retain_loadings) paste0("L_", k) else paste0("(", loading_expression(k), ")")
  }

  loading_covariance_expression <- function(k) {
    loading_ref <- loading_reference(k)
    paste0(loading_ref, " %*% t(", loading_ref, ")")
  }
  # exp(Tpoly %*% B_k + H %*% G_k) %*% t(exp(Tpoly %*% B_k + H %*% G_k))

  loading_covariance_reference <- function(k) {
    if (retain_loading_covariances) {
      paste0("K", k)
    } else {
      paste0("(", loading_covariance_expression(k), ")")
    }
  }

  component_covariance_expression <- function(s) {
    paste0(s$mxname, " * ", loading_covariance_reference(s$k))
  }

  residual_covariance_expression <- function() {
    if (residual_covariance_form == "diagonal") {
      loading_ref <- loading_reference("e")
      paste0("vec2diag(", loading_ref, " * ", loading_ref, ")")
    } else {
      paste0("I * ", loading_covariance_reference("e"))
    }
  }


  eta_keys <- c(vapply(active, `[[`, character(1), "k"), "e")
  eta_parts <- if (retain_eta) {
    lapply(eta_keys, function(k) {
      OpenMx::mxAlgebraFromString(eta_expression(k), name = paste0("Eta_", k))
    })
  } else {
    list()
  }

  loading_parts <- if (retain_loadings) {
    lapply(eta_keys, function(k) {
      OpenMx::mxAlgebraFromString(loading_expression(k), name = paste0("L_", k))
    })
  } else {
    list()
  }


  # turn into loading
  make_lambda_alg <- function(k) {
    if (use_exp_loadings) {
      OpenMx::mxAlgebraFromString(paste0("exp(Eta_", k, ")"), name = paste0("L_", k))
    } else {
      OpenMx::mxAlgebraFromString(paste0("Eta_", k), name = paste0("L_", k))
    }
  }

  # each variance component's covariance matrix is the outer product of its loading vector with itself, scaled by the relatedness matrix
  # Kk is the unscaled loading outer product. It is retained independently because it
  # is useful for evaluating the temporal/moderator scaling before relatedness is applied.
  loading_covariance_parts <- if (retain_loading_covariances) {
    lapply(eta_keys, function(k) {
      OpenMx::mxAlgebraFromString(
        loading_covariance_expression(k),
        name = paste0("K", k)
      )
    })
  } else {
    list()
  }


  # Cov_k is the full relatedness-weighted covariance contribution for component k.
  component_covariance_parts <- list()
  if (retain_component_covariances) {
    component_covariance_parts <- lapply(active, function(s) {
      OpenMx::mxAlgebraFromString(
        component_covariance_expression(s),
        name = paste0("Cov_", s$k)
      )
    })
    component_covariance_parts[[length(component_covariance_parts) + 1L]] <-
      OpenMx::mxAlgebraFromString(
        residual_covariance_expression(),
        name = "Cov_e"
      )
  }

  if (retain_component_covariances) {
    covariance_terms <- c(
      vapply(active, function(s) paste0("Cov_", s$k), character(1)),
      "Cov_e"
    )
  } else {
    covariance_terms <- c(
      vapply(active, component_covariance_expression, character(1)),
      residual_covariance_expression()
    )
  }
  covariance_algebra <- paste(c(covariance_terms), collapse = " + ")

  model_parts <- c(
    list(group_name),
    fixed_parts,
    relmat_list,
    eta_parts,
    loading_parts,
    loading_covariance_parts,
    component_covariance_parts,
    list(
      .pedigreeMeanPart(
        fsize = fsize, obs_ids = obs_ids, label = "mean_y",
        # mean_basis and mean_hist arrive as matrix names, not flags. Passing
        # mean_basis = NULL falls back to the constant mean; the historical shift
        # is only available when there are historical columns to shift on.
        mean_basis = mean_basis,
        mean_hist  = if (!is.null(mean_hist) && p_hist > 0) mean_hist else NULL
      ),
      OpenMx::mxAlgebraFromString(
        covariance_algebra,
        name = "V",
        dimnames = list(obs_ids, obs_ids)
      ),
      OpenMx::mxExpectationNormal(covariance = "V", means = "M", dimnames = obs_ids),
      OpenMx::mxFitFunctionML()
    )
  )

  do.call(OpenMx::mxModel, model_parts)
}

#' Build family group models
#'
#' This function constructs OpenMx models for multiple family groups based on
#' provided relatedness matrices and observed data. All families share the same
#' relatedness matrices (e.g. a fixed pedigree template); only the observed data
#' row (and, for temporal models, birth year / historical moderators) varies by
#' family. For families with different structure or size, see \code{\link{buildFamilyGroups_list}}.
#' @inheritParams buildOneFamilyGroup
#' @param dat A data frame where each row represents a family group and columns correspond to observed variables.
#' @param obs_ids A character vector of individual IDs corresponding to the columns of \code{dat}
#'   and the rows/columns of the relatedness matrices.
#' @param prefix A prefix for naming the family groups. Default is "fam".
#' @param param_year_list A list of numeric birth-year vectors, one per family (row of \code{dat}),
#'   each matching \code{obs_ids} in length and order. Only used when \code{temporal = TRUE}.
#' @param H_list A list of historical-moderator matrices, one per family. Only used when
#'   \code{temporal = TRUE}.
#' @param group_static_families Logical. If TRUE and \code{temporal = FALSE}, build
#'   one OpenMx raw-data submodel containing all rows of \code{dat}. If FALSE,
#'   preserve the original behavior of building one submodel per family.
#' @return A list of OpenMx models for each family group.
#' @export

buildFamilyGroups <- function(
  dat,
  obs_ids,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  prefix = "fam",
  condenseMatrixSlots = TRUE,
  temporal = FALSE,
  param_year_list = NULL,
  H_list = NULL,
  use_exp_loadings = TRUE,
  time_point_max = NULL,
  retain_eta = TRUE,
  retain_loadings = TRUE,
  retain_loading_covariances = TRUE,
  retain_component_covariances = TRUE,
  residual_covariance_form = c("outer_product", "diagonal"),
  clean_ids = TRUE,
  group_static_families = FALSE
) {
  .require_openmx("buildFamilyGroups")
  if (clean_ids == TRUE) {
    obs_ids <- make_clean_personids(obs_ids)
    # clean once
    clean_ids <- FALSE
  }

  if (group_static_families && temporal) {
    stop(
      "'group_static_families = TRUE' is only supported when 'temporal = FALSE'."
    )
  }

  if (group_static_families) {
    return(list(
      .buildGroupedStaticFamily(
        group_name = paste0(prefix, "_grouped"),
        dat = dat,
        obs_ids = obs_ids,
        Addmat = Addmat,
        Nucmat = Nucmat,
        Extmat = Extmat,
        Mtdmat = Mtdmat,
        Amimat = Amimat,
        Dmgmat = Dmgmat,
        condenseMatrixSlots = condenseMatrixSlots,
        clean_ids = clean_ids
      )
    ))
  }

  numfam <- nrow(dat)
  groups <- vector("list", numfam)

  for (afam in seq_len(numfam)) {
    full_df_row <- matrix(dat[afam, ], nrow = 1, dimnames = list(NULL, obs_ids))
    groups[[afam]] <- buildOneFamilyGroup(
      group_name = paste0(prefix, afam),
      Addmat = Addmat,
      Nucmat = Nucmat,
      Extmat = Extmat,
      Mtdmat = Mtdmat,
      Amimat = Amimat,
      Dmgmat = Dmgmat,
      full_df_row = full_df_row,
      obs_ids = obs_ids,
      condenseMatrixSlots = condenseMatrixSlots,
      temporal = temporal,
      param_year = if (temporal) param_year_list[[afam]] else NULL,
      H = if (temporal) H_list[[afam]] else NULL,
      use_exp_loadings = use_exp_loadings,
      time_point_max = time_point_max,
      retain_eta = retain_eta,
      retain_loadings = retain_loadings,
      retain_loading_covariances = retain_loading_covariances,
      retain_component_covariances = retain_component_covariances,
      residual_covariance_form = residual_covariance_form,
      clean_ids = clean_ids
    )
  }

  groups
}

#' Build family group models with per-family relatedness matrices
#'
#' This function constructs OpenMx models for multiple family groups, each with its own
#' relatedness matrices, observed IDs, and (for temporal models) birth years / historical
#' moderators. Use this when families vary in size or structure; for families that all share
#' the same relatedness matrices, see \code{\link{buildFamilyGroups}}.
#' @inheritParams buildOneFamilyGroup
#' @param dat_list A list of numeric vectors of observed data, one per family.
#' @param obs_ids_list A list of character vectors of individual IDs, one per family, matching
#'   \code{dat_list} and the rows/columns of that family's relatedness matrices.
#' @param Addmat_list A list of additive genetic relatedness matrices, one per family.
#' @param Nucmat_list A list of nuclear family shared environment relatedness matrices, one per family.
#' @param Extmat_list A list of common extended family environment relatedness matrices, one per family.
#' @param Mtdmat_list A list of mitochondrial genetic relatedness matrices, one per family.
#' @param Amimat_list A list of additive by mitochondrial interaction relatedness matrices, one per family.
#' @param Dmgmat_list A list of dominance genetic relatedness matrices, one per family.
#' @param prefix A prefix for naming the family groups. Default is "fam".
#' @param param_year_list A list of numeric birth-year vectors, one per family, each matching
#'   the corresponding entry of \code{obs_ids_list}. Only used when \code{temporal = TRUE}.
#' @param H_list A list of historical-moderator matrices, one per family. Only used when
#'   \code{temporal = TRUE}.
#' @return A list of OpenMx models for each family group.
#' @export

buildFamilyGroups_list <- function(
  dat_list,
  obs_ids_list,
  Addmat_list = NULL,
  Nucmat_list = NULL,
  Extmat_list = NULL,
  Mtdmat_list = NULL,
  Amimat_list = NULL,
  Dmgmat_list = NULL,
  prefix = "fam",
  condenseMatrixSlots = TRUE,
  temporal = FALSE,
  param_year_list = NULL,
  H_list = NULL,
  use_exp_loadings = TRUE,
  time_point_max = NULL,
  retain_eta = TRUE,
  retain_loadings = TRUE,
  retain_loading_covariances = TRUE,
  retain_component_covariances = TRUE,
  residual_covariance_form = c("outer_product", "diagonal"),
  clean_ids = TRUE
) {
  .require_openmx("buildFamilyGroups_list")

  numfam <- length(dat_list)
  groups <- vector("list", numfam)

  get_or_null <- function(x, i) {
    if (is.null(x)) NULL else x[[i]]
  }

  for (afam in seq_len(numfam)) {
    if (clean_ids == TRUE) {
      obs_ids <- make_clean_personids(obs_ids_list[[afam]])
      clean_ids <- FALSE
    } else {
      obs_ids <- obs_ids_list[[afam]]
    }
    full_df_row <- matrix(
      dat_list[[afam]],
      nrow = 1,
      dimnames = list(NULL, obs_ids)
    )

    groups[[afam]] <- buildOneFamilyGroup(
      group_name = paste0(prefix, afam),
      Addmat = get_or_null(Addmat_list, afam),
      Nucmat = get_or_null(Nucmat_list, afam),
      Extmat = get_or_null(Extmat_list, afam),
      Mtdmat = get_or_null(Mtdmat_list, afam),
      Amimat = get_or_null(Amimat_list, afam),
      Dmgmat = get_or_null(Dmgmat_list, afam),
      full_df_row = full_df_row,
      obs_ids = obs_ids,
      condenseMatrixSlots = condenseMatrixSlots,
      temporal = temporal,
      param_year = if (temporal) param_year_list[[afam]] else NULL,
      H = if (temporal) get_or_null(H_list, afam) else NULL,
      use_exp_loadings = use_exp_loadings,
      time_point_max = time_point_max,
      retain_eta = retain_eta,
      retain_loadings = retain_loadings,
      retain_loading_covariances = retain_loading_covariances,
      retain_component_covariances = retain_component_covariances,
      residual_covariance_form = residual_covariance_form,
      clean_ids = clean_ids # already cleaned above if requested
    )
  }

  groups
}


#' Build Pedigree mxModel
#'
#' This function constructs an OpenMx pedigree model by combining variance
#' component parameters and family group models. For static (\code{temporal = FALSE})
#' models, it auto-detects which variance components are referenced in the group
#' algebras and creates only those parameters.
#' @inheritParams buildOneFamilyGroup
#' @param model_name Name of the overall pedigree model.
#' @param vars A named list or vector of initial variance component values. Only used
#'   when \code{temporal = FALSE}.
#' @param group_models A list of OpenMx models for each family group.
#' @param ci Logical. If TRUE, include confidence interval computations for the variance components. Default is FALSE
#' @param p_hist Integer. Number of historical moderator columns. Only used when
#'   \code{temporal = TRUE}. Default is 0.
#' @param components Character vector of component keys to include (any of "a", "d", "cn",
#'   "ce", "mt", "am", "e"). Only used when \code{temporal = TRUE}. Default is \code{c("a", "e")}.
#' @param ci_names A vector of parameters for confidence intervals. If not provided, it is derived from components
#' @return An OpenMx pedigree model combining variance components and family groups.
#' @export

buildPedigreeMx <- function(model_name, vars, group_models,
                            ci = FALSE,
                            condenseMatrixSlots = TRUE,
                            temporal = FALSE,
                            p_hist = 0,
                            param_year = NULL,
                            components = c("a", "e"),
                            ci_names = NULL,
                            time_point_max = NULL,
                            mean_degree = 0,
                            start_mean = 0,
                            mean_hist_free = FALSE,
                            start_mean_hist = 0) {
  .require_openmx("buildPedigreeMx")

  if (temporal) {
    components <- unique(c(components, "e"))
    tp_max <- if (is.null(time_point_max)) 3 else time_point_max

    model_one <- buildPedigreeModelCovariance(
      temporal = TRUE,
      p_hist = p_hist,
      components = components,
      time_point_max = time_point_max,
      mean_degree = mean_degree,
      start_mean = start_mean,
      mean_hist_free = mean_hist_free,
      start_mean_hist = start_mean_hist
    )

    ci_obj <- NULL
    if (ci) {
      if(is.null(ci_names)){
      ci_names <- unlist(lapply(components, function(k) {
        out <- paste0("b_", k, "_", 0:tp_max)
        if (p_hist > 0) out <- c(out, paste0("g_", k, "_", seq_len(p_hist)))
        out
      }))
      }
      ci_obj <- OpenMx::mxCI(ci_names)
    }

    return(.assemblePedigreeMx(model_name, model_one, group_models, ci_obj))
  }

  # Auto-detect which variance components the group algebras reference
  # by inspecting the algebra formula strings for ModelOne.V* patterns.
  # This keeps the variance component sub-model in sync with the groups.
  vc_map <- c(
    Vad = "ModelOne.Vad",
    Vdd = "ModelOne.Vdd",
    Vcn = "ModelOne.Vcn",
    Vce = "ModelOne.Vce",
    Vmt = "ModelOne.Vmt",
    Vam = "ModelOne.Vam",
    Ver = "ModelOne.Ver"
  )

  # Collect all algebra formulas from group models
  all_formulas <- vapply(group_models, function(m) {
    if (!is.null(m$V) && !is.null(m$V$formula)) {
      deparse(m$V$formula, width.cutoff = 500L)
    } else {
      ""
    }
  }, character(1))
  all_formulas <- paste(all_formulas, collapse = " ")

  flags <- lapply(vc_map, function(pat) grepl(pat, all_formulas, fixed = TRUE))

  model_one <- buildPedigreeModelCovariance(
    vars,
    Vad = isTRUE(flags$Vad),
    Vdd = isTRUE(flags$Vdd),
    Vcn = isTRUE(flags$Vcn),
    Vce = isTRUE(flags$Vce),
    Vmt = isTRUE(flags$Vmt),
    Vam = isTRUE(flags$Vam),
    Ver = isTRUE(flags$Ver)
  )

  ci_obj <- if (ci & any(flags$Vad, flags$Vdd, flags$Vcn, flags$Vce, flags$Vmt, flags$Vam, flags$Ver)) {
   if(is.null(ci_names)){
     ci_names <- c("vad", "vdd", "vcn", "vce", "vmt", "vam", "ver")[c(flags$Vad, flags$Vdd, flags$Vcn, flags$Vce, flags$Vmt, flags$Vam, flags$Ver)]
   }
    OpenMx::mxCI(ci_names)
  } else {
    NULL
  }

  .assemblePedigreeMx(model_name, model_one, group_models, ci_obj)
}

#' Fit an OpenMx pedigree model to observed data
#'
#' This function constructs and fits an OpenMx model for a pedigree using
#' specified variance components and family group models.
#' @inheritParams buildPedigreeMx
#' @inheritParams buildOneFamilyGroup
#' @param model_name Character. Name for the overall OpenMx model. Default is "PedigreeModel".
#' @param vars A named list or vector of initial variance component values. Only used
#'   when \code{temporal = FALSE}.
#' @param data A matrix or data frame of observed data, where each row is a family
#'   and columns correspond to individuals. Only used when \code{group_models} is NULL
#'   and \code{temporal = FALSE}.
#' @param group_models Optional list of pre-built OpenMx family group models
#'   (from \code{\link{buildOneFamilyGroup}}). If NULL, they are generated from \code{data}
#'   (or, when \code{temporal = TRUE}, from \code{dat_list} and friends) using the provided
#'   relatedness matrices.
#' @param tryhard Logical. If TRUE (default), use \code{mxTryHard} for robust optimization;
#'   if FALSE, use \code{mxRun}.
#' @param intervals Logical. If TRUE (default), compute confidence intervals for the parameters using \code{mxSE} and \code{mxCI}.
#' @param extraTries Numeric. The number of extra optimization attempts to make when \code{tryhard} is TRUE. Default is 10.
#' @param runmodel Logical. If TRUE (default), the model is fitted; if FALSE, the model is returned without fitting.
#' @param dat_list A list of numeric vectors of observed data, one per family. Only used when
#'   \code{temporal = TRUE} and \code{group_models} is NULL.
#' @param obs_ids_list A list of character vectors of individual IDs, one per family. Only used
#'   when \code{temporal = TRUE} and \code{group_models} is NULL.
#' @param param_year_list A list of numeric birth-year vectors, one per family. Only used when
#'   \code{temporal = TRUE} and \code{group_models} is NULL.
#' @param H_list A list of historical-moderator matrices, one per family. Only used when
#'   \code{temporal = TRUE}.
#' @param Addmat_list A list of additive genetic relatedness matrices, one per family. Only used
#'   when \code{temporal = TRUE} and \code{group_models} is NULL.
#' @param Nucmat_list A list of nuclear family shared environment relatedness matrices, one per
#'   family. Only used when \code{temporal = TRUE} and \code{group_models} is NULL.
#' @param Extmat_list A list of common extended family environment relatedness matrices, one per
#'   family. Only used when \code{temporal = TRUE} and \code{group_models} is NULL.
#' @param Mtdmat_list A list of mitochondrial genetic relatedness matrices, one per family. Only
#'   used when \code{temporal = TRUE} and \code{group_models} is NULL.
#' @param Amimat_list A list of additive by mitochondrial interaction relatedness matrices, one per family. Only
#'   used when \code{temporal = TRUE} and \code{group_models} is NULL.
#' @param Dmgmat_list A list of dominance genetic relatedness matrices, one per family. Only used
#'   when \code{temporal = TRUE} and \code{group_models} is NULL.
#' @param p_hist Integer. Number of historical moderator columns. Only used when
#'   \code{temporal = TRUE}. If NULL, inferred from \code{H_list}.
#' @param group_static_families Logical. If TRUE for a static model built from
#'   \code{data}, build one raw-data submodel containing all family rows. If
#'   FALSE, preserve the original one-submodel-per-family implementation.
#' @return A fitted OpenMx model.
#' @export

fitPedigreeModel <- function(
  model_name = "PedigreeModel",
  vars = list(
    ad2 = 0.5,
    dd2 = 0.3,
    cn2 = 0.2,
    ce2 = 0.4,
    mt2 = 0.1,
    am2 = 0.25,
    ee2 = 0.6
  ),
  data = NULL,
  group_models = NULL,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  tryhard = TRUE,
  intervals = TRUE,
  extraTries = 10,
  condenseMatrixSlots = TRUE,
  runmodel = TRUE,
  temporal = FALSE,
  dat_list = NULL,
  obs_ids_list = NULL,
  param_year_list = NULL,
  H_list = NULL,
  Addmat_list = NULL,
  Nucmat_list = NULL,
  Extmat_list = NULL,
  Mtdmat_list = NULL,
  Amimat_list = NULL,
  Dmgmat_list = NULL,
  p_hist = NULL,
  components = c("a", "d", "cn", "ce", "mt", "am", "e"),
  use_exp_loadings = FALSE,
  time_point_max = NULL,
  mean_degree = 0,
  start_mean = 0,
  mean_hist_free = FALSE,
  start_mean_hist = 0,
  retain_eta = TRUE,
  retain_loadings = TRUE,
  retain_loading_covariances = TRUE,
  retain_component_covariances = TRUE,
  residual_covariance_form = c("outer_product", "diagonal"),
  clean_ids = TRUE,
  group_static_families = FALSE
) {
  .require_openmx("fitPedigreeModel")

  if (temporal && group_static_families) {
    stop(
      "'group_static_families = TRUE' is only supported when 'temporal = FALSE'."
    )
  }

  if (!is.null(group_models) && group_static_families) {
    warning(
      "'group_static_families' is ignored when pre-built 'group_models' are supplied."
    )
  }

  if (is.null(group_models)) {
    if (temporal) {
      if (is.null(dat_list) || is.null(obs_ids_list) || is.null(param_year_list)) {
        stop("Provide either 'group_models' or dat_list, obs_ids_list, and param_year_list.")
      }

      if (is.null(H_list)) {
        H_list <- lapply(param_year_list, function(x) matrix(numeric(0), nrow = length(x), ncol = 0))
      }

      group_models <- buildFamilyGroups_list(
        dat_list = dat_list,
        obs_ids_list = obs_ids_list,
        Addmat_list = Addmat_list,
        Nucmat_list = Nucmat_list,
        Extmat_list = Extmat_list,
        Mtdmat_list = Mtdmat_list,
        Amimat_list = Amimat_list,
        Dmgmat_list = Dmgmat_list,
        condenseMatrixSlots = condenseMatrixSlots,
        temporal = TRUE,
        param_year_list = param_year_list,
        H_list = H_list,
        use_exp_loadings = use_exp_loadings,
        time_point_max = time_point_max,
        retain_eta = retain_eta,
        retain_loadings = retain_loadings,
        retain_loading_covariances = retain_loading_covariances,
        retain_component_covariances = retain_component_covariances,
        residual_covariance_form = residual_covariance_form,
        clean_ids = clean_ids
      )
    } else {
      # generate them from data and relatedness matrices
      if (is.null(data)) {
        stop("Either 'group_models' or 'data' must be provided.")
      }
      if (clean_ids == TRUE) {
        obs_ids <- make_clean_personids(colnames(data))
        # clean once
        clean_ids <- FALSE
      } else {
        obs_ids <- colnames(data)
      }
      group_models <- buildFamilyGroups(
        dat = data,
        obs_ids = obs_ids,
        Addmat = Addmat,
        Nucmat = Nucmat,
        Extmat = Extmat,
        Mtdmat = Mtdmat,
        Amimat = Amimat,
        Dmgmat = Dmgmat,
        condenseMatrixSlots = condenseMatrixSlots,
        clean_ids = clean_ids,
        group_static_families = group_static_families
      )
    }
  }

  if (temporal && is.null(p_hist)) {
    if (!is.null(H_list) && length(H_list) > 0 && !is.null(H_list[[1]])) {
      p_hist <- ncol(H_list[[1]])
    } else {
      p_hist <- 0
    }
  }

  pedigree_model <- buildPedigreeMx(
    model_name = model_name,
    vars = vars,
    group_models = group_models,
    ci = intervals,
    condenseMatrixSlots = FALSE, # only need to condense once
    temporal = temporal,
    p_hist = if (temporal) p_hist else 0,
    components = components,
    time_point_max = time_point_max,
    mean_degree = mean_degree,
    start_mean = start_mean,
    mean_hist_free = mean_hist_free,
    start_mean_hist = start_mean_hist
  )
  if (runmodel == TRUE) {
    if (tryhard == TRUE) {
      fitted_model <- OpenMx::mxTryHard(pedigree_model, silent = TRUE, extraTries = extraTries, intervals = intervals)
    } else {
      fitted_model <- OpenMx::mxRun(pedigree_model, intervals = intervals)
    }
  } else {
    fitted_model <- pedigree_model
  }

  fitted_model
}

#' Assemble the top-level pedigree mxModel (internal)
#'
#' Combines the covariance sub-model (named \code{"ModelOne"}), the family-group models,
#' the multigroup fit function, and (optionally) a confidence-interval specification into
#' one mxModel. Shared by both the static and temporal branches of
#' \code{\link{buildPedigreeMx}}, which otherwise only differ in how \code{model_one}
#' and \code{ci_obj} are built.
#'
#' @param model_name Name of the overall pedigree model.
#' @param model_one The covariance sub-model (from \code{\link{buildPedigreeModelCovariance}}).
#' @param group_models A list of OpenMx models for each family group.
#' @param ci_obj An \code{mxCI} object to include, or NULL to omit confidence intervals.
#' @return An OpenMx pedigree model.
#' @keywords internal
.assemblePedigreeMx <- function(model_name, model_one, group_models, ci_obj = NULL) {
  group_names <- vapply(group_models, function(m) m$name, character(1))

  model_parts <- c(
    list(model_name),
    list(model_one),
    group_models,
    list(OpenMx::mxFitFunctionMultigroup(group_names))
  )
  if (!is.null(ci_obj)) model_parts[[length(model_parts) + 1]] <- ci_obj

  do.call(OpenMx::mxModel, model_parts)
}

#' Align Phenotype Vector to Matrix Format for OpenMx
#'
#' This function takes a pedigree data frame, a specified phenotype column, and a vector of IDs to keep, and returns a matrix formatted for use in OpenMx models. The resulting matrix has one row and columns corresponding to the specified IDs, with values taken from the phenotype column of the pedigree.
#' @param ped A data frame representing the pedigree, containing at least the columns specified by \code{phenotype} and \code{personID}.
#' @param phenotype A character string specifying the column name in \code{ped} that
#'  contains the phenotype values to be aligned.
#' @param keep_ids A vector of IDs for which the phenotype values should be extracted and aligned. These IDs should correspond to the values in the \code{personID} of \code{ped}.
#' @param personID A character string specifying the column name in \code{ped} that contains the individual IDs. Default is "ID".
#' @export


alignPhenToMatrix <- function(ped, phenotype, keep_ids, personID = "ID") {
  obs_ids <- make.names(as.character(keep_ids))
  pheno_vals <- ped[[phenotype]][match(as.character(keep_ids), as.character(ped[[personID]]))]
  matrix(as.double(pheno_vals), nrow = 1, dimnames = list(NULL, obs_ids))
}

#' Condense Matrix Slots in an OpenMx Model
#'
#' This function takes an OpenMx model and applies the \code{mxCondenseMatrixSlots} wrapper to optimize memory usage for large matrices. This can be particularly beneficial when working with large pedigree models that include multiple relatedness matrices.
#' @param model An OpenMx model object for which to condense matrix slots. If
#' NULL, the function returns NULL.
#' @return An OpenMx model with condensed matrix slots, or NULL if the input model
#' is NULL.


condenseMatrixSlots <- function(model) {
  .require_openmx("condenseMatrixSlots")
  if (is.null(model)) {
    return(NULL)
  }
  #  no applicable method for `@` applied to an object of class "matrix"
  #  if (is.matrix(model)) {
  #   return(model)
  # }
  OpenMx::imxConDecMatrixSlots(model)
}

#' Make Clean IDs for OpenMx
#'
#' This function takes a vector of IDs and returns a cleaned version suitable for use in OpenMx models. It replaces any illegal characters (such as '.') with underscores and ensures that the IDs are valid R variable names. This is important because OpenMx does not allow certain characters in matrix or variable names, which can lead to errors when building models.
#' @param ids A vector of IDs to be cleaned.
#' @return A vector of cleaned IDs suitable for use in OpenMx models.
#'
make_clean_personids <- function(ids) {
  .require_openmx("make_clean_personids")
  OpenMx::mxMakeNames(as.character(ids))
}
