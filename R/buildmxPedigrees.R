#' Create an mxModel for a pedigree
#'
#' This function builds an OpenMx model for a pedigree with specified variance components. It requires the OpenMx package.
#'
#' @param vars A named list or vector of initial variance component values. Names should include
#'   ad2 (additive), dd2 (dominance), cn2 (common nuclear), ce2 (common extended),
#'   mt2 (mitochondrial), am2 (additive-mitochondrial interaction), and ee2 (unique environment).
#'   Default values are provided.
#' @param Vad Logical. Include additive genetic variance component. Default is TRUE.
#' @param Vdd Logical. Include dominance genetic variance component. Default is FALSE.
#' @param Vcn Logical. Include common nuclear family environment variance component. Default is TRUE.
#' @param Vce Logical. Include common extended family environment variance component. Default is TRUE.
#' @param Vmt Logical. Include mitochondrial genetic variance component. Default is TRUE.
#' @param Vam Logical. Include additive by mitochondrial interaction variance component. Default is FALSE.
#' @param Ver Logical. Include unique environmental variance component. Default is TRUE.
#' @param lbound Numeric. A lower bound for the variance components to ensure they remain positive during optimization. Default is 1e-10
#' @return An OpenMx model representing the pedigree with specified variance components.
#' @export

# condense matrix slots setting (should make it nicer
# gcat expectation fit function that might be helpful. is optimized for large matrices
# doable in python? SHOR author, "I don't exactly remember" is name of package scillm (linear mixed models)

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
  Vdd = FALSE,
  Vcn = TRUE,
  Vce = TRUE,
  Vmt = TRUE,
  Vam = FALSE,
  Ver = TRUE,
  lbound = 1e-10
) {
  .require_openmx("buildPedigreeModelCovariance")

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
#' Builds the \code{"Symm"} mxMatrix for one relatedness matrix.
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
  # keep dense if dense, or sparse if sparse, but symmetrize if requested
  if (inherits(mat, "Matrix")) {
    values <- if (symmetrize) make_symmetric(mat) else mat
  } else {
    values <- if (symmetrize) make_symmetric(mat) else as.matrix(mat)
  }
  m <- OpenMx::mxMatrix(
    type = "Symm", nrow = fsize, ncol = fsize, free = FALSE,
    values = values, name = name
  )
  if (condense) m <- condenseMatrixSlots(m)
  m
}

#' Build the free mean mxMatrix for a family group (internal)
#'
#' Shared by the static and temporal branches of \code{\link{buildOneFamilyGroup}}; they
#' only differ in the parameter label ("meanLI" vs "mean_y").
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
#' @param clean_ids Logical. If TRUE, clean the \code{obs_ids} using \code{\link{make_clean_personids}}. Default is FALSE.
#' @param symmetrize Logical. If TRUE, symmetrize the relatedness matrices before use. Default is FALSE.
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
  symmetrize = FALSE,
  condenseMatrixSlots = TRUE,
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
      condense = condenseMatrixSlots, symmetrize = symmetrize
    )
  })
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
      .pedigreeMeanMatrix(fsize, obs_ids, "meanLI"),
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
        fsize,
        obs_ids,
        "meanLI"
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

#' Build family group models
#'
#' This function constructs OpenMx models for multiple family groups based on
#' provided relatedness matrices and observed data.
#' @inheritParams buildOneFamilyGroup
#' @param dat A data frame where each row represents a family group and columns correspond to observed variables.
#' @param obs_ids A character vector of individual IDs corresponding to the columns of \code{dat}
#'   and the rows/columns of the relatedness matrices.
#' @param prefix A prefix for naming the family groups. Default is "fam".
#' @return A list of OpenMx models for each family group.
#' @export

buildFamilyGroups <- function(
  dat, obs_ids,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  prefix = "fam",
  condenseMatrixSlots = TRUE,
  clean_ids = TRUE,
  group_static_families = FALSE
) {
  .require_openmx("buildFamilyGroups")
  if (clean_ids == TRUE) {
    obs_ids <- make_clean_personids(obs_ids)
    # clean once
    clean_ids <- FALSE
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
#' @param birth_year_list A list of numeric birth-year vectors, one per family, each matching
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
      clean_ids = clean_ids # already cleaned above if requested
    )
  }

  groups
}

#' Build Pedigree mxModel
#'
#' This function constructs an OpenMx pedigree model by combining variance
#' component parameters and family group models. It auto-detects which
#' variance components are referenced in the group algebras and creates
#' only those parameters.
#' @inheritParams buildOneFamilyGroup
#' @param model_name Name of the overall pedigree model.
#' @param vars A named list or vector of initial variance component values.
#' @param group_models A list of OpenMx models for each family group.
#' @param ci Logical. If TRUE, include confidence interval computations for the variance components. Default is FALSE
#' @param components Character vector of component keys to include (any of "a", "d", "cn",
#'   "ce", "mt", "am", "e"). Only used when \code{temporal = TRUE}. Default is \code{c("a", "e")}.
#' @return An OpenMx pedigree model combining variance components and family groups.
#' @export

buildPedigreeMx <- function(model_name, vars, group_models,
                            ci = FALSE,
                            condenseMatrixSlots = TRUE,
                            components = c("a", "e")) {
  .require_openmx("buildPedigreeMx")

  group_names <- vapply(group_models, function(m) m$name, character(1))

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

  ci_obj <- if (ci && any(flags$Vad, flags$Vdd, flags$Vcn, flags$Vce, flags$Vmt, flags$Vam, flags$Ver)) {
    OpenMx::mxCI(c("vad", "vdd", "vcn", "vce", "vmt", "vam", "ver")[c(flags$Vad, flags$Vdd, flags$Vcn, flags$Vce, flags$Vmt, flags$Vam, flags$Ver)])
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
#' @param vars A named list or vector of initial variance component values.
#' @param data A matrix or data frame of observed data, where each row is a family
#'   and columns correspond to individuals. Only used when \code{group_models} is NULL.
#' @param group_models Optional list of pre-built OpenMx family group models
#'   (from \code{\link{buildOneFamilyGroup}}). If NULL, they are generated from \code{data}
#'   using the provided relatedness matrices.
#' @param tryhard Logical. If TRUE (default), use \code{mxTryHard} for robust optimization;
#'   if FALSE, use \code{mxRun}.
#' @param intervals Logical. If TRUE (default), compute confidence intervals for the parameters using \code{mxSE} and \code{mxCI}.
#' @param extraTries Numeric. The number of extra optimization attempts to make when \code{tryhard} is TRUE. Default is 10.
#' @param runmodel Logical. If TRUE (default), the model is fitted; if FALSE, the model is returned without fitting.
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
  runmodel = TRUE
) {
  .require_openmx("fitPedigreeModel")

  if (is.null(group_models)) {
    # generate them from data and relatedness matrices
    if (is.null(data)) {
      stop("Either 'group_models' or 'data' must be provided.")
    }


    obs_ids <- colnames(data)
    group_models <- buildFamilyGroups(
      dat = data,
      obs_ids = obs_ids,
      Addmat = Addmat,
      Nucmat = Nucmat,
      Extmat = Extmat,
      Mtdmat = Mtdmat,
      Amimat = Amimat,
      Dmgmat = Dmgmat,
      condenseMatrixSlots = condenseMatrixSlots
    )
  }

  pedigree_model <- buildPedigreeMx(
    model_name = model_name,
    vars = vars,
    group_models = group_models,
    ci = intervals,
    condenseMatrixSlots = FALSE # only need to condense once
  )
  if (runmodel == TRUE) {
    if (tryhard == TRUE) {
      return_model <- OpenMx::mxTryHard(pedigree_model, silent = TRUE, extraTries = extraTries, intervals = intervals)
    } else {
      return_model <- OpenMx::mxRun(pedigree_model, intervals = intervals)
    }
  } else {
    return_model <- pedigree_model
  }

  return_model
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
