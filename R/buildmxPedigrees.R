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
  Ver = TRUE
) {
  .require_openmx("buildPedigreeModelCovariance")

  # Coerce to list so both c() vectors and list() inputs work with [[ ]]
  vars <- as.list(vars)

  # Build the list of mxMatrix components conditionally
  mat_list <- list()
  if (Vad) {
    mat_list <- c(mat_list, list(OpenMx::mxMatrix(
      type = "Full", nrow = 1, ncol = 1, free = TRUE,
      values = vars[["ad2"]], labels = "vad", name = "Vad", lbound = 1e-10
    )))
  }
  if (Vdd) {
    mat_list <- c(mat_list, list(OpenMx::mxMatrix(
      type = "Full", nrow = 1, ncol = 1, free = TRUE,
      values = vars[["dd2"]], labels = "vdd", name = "Vdd", lbound = 1e-10
    )))
  }
  if (Vcn) {
    mat_list <- c(mat_list, list(OpenMx::mxMatrix(
      type = "Full", nrow = 1, ncol = 1, free = TRUE,
      values = vars[["cn2"]], labels = "vcn", name = "Vcn", lbound = 1e-10
    )))
  }
  if (Vce) {
    mat_list <- c(mat_list, list(OpenMx::mxMatrix(
      type = "Full", nrow = 1, ncol = 1, free = TRUE,
      values = vars[["ce2"]], labels = "vce", name = "Vce", lbound = 1e-10
    )))
  }
  if (Vmt) {
    mat_list <- c(mat_list, list(OpenMx::mxMatrix(
      type = "Full", nrow = 1, ncol = 1, free = TRUE,
      values = vars[["mt2"]], labels = "vmt", name = "Vmt", lbound = 1e-10
    )))
  }
  if (Vam) {
    mat_list <- c(mat_list, list(OpenMx::mxMatrix(
      type = "Full", nrow = 1, ncol = 1, free = TRUE,
      values = vars[["am2"]], labels = "vam", name = "Vam", lbound = 1e-10
    )))
  }
  if (Ver) {
    mat_list <- c(mat_list, list(OpenMx::mxMatrix(
      type = "Full", nrow = 1, ncol = 1, free = TRUE,
      values = vars[["ee2"]], labels = "ver", name = "Ver", lbound = 1e-10
    )))
  }

  do.call(OpenMx::mxModel, c(list("ModelOne"), mat_list))
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
#' @param Extmat Extended family shared environment indicator. When non-NULL,
#'   a common-extended-environment term using a unit matrix is included.
#' @param Mtdmat Mitochondrial genetic relatedness matrix (from \code{\link{ped2mit}}).
#' @param Amimat Additive by mitochondrial interaction relatedness matrix.
#' @param Dmgmat Dominance genetic relatedness matrix.
#' @param full_df_row A 1-row matrix of observed data with column names matching \code{obs_ids}.
#' @param obs_ids A character vector of individual IDs corresponding to the columns of
#'   \code{full_df_row} and the rows/columns of the relatedness matrices. Must be in the
#'   same order as the relatedness matrix rows.
#' @param type Type of observed variable: "continuous", "binary", or "ordinal". Default is "continuous".
#' @param nthresh For ordinal data, the number of thresholds. Default is 1
#' @param thresh_start Starting value for thresholds if \code{type} is "ordinal". Default is 0.
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
  type = c("continuous", "binary", "ordinal"),
  nthresh = 1,
  thresh_start = 0,
  thresh_free = TRUE,
  equate_thresholds = TRUE
) {
  .require_openmx("buildOneFamilyGroup")

  type <- match.arg(type)
  # Determine family size from first available matrix
  fsize <- NULL
  for (m in list(Addmat, Nucmat, Extmat, Mtdmat, Amimat, Dmgmat)) {
    if (!is.null(m)) {
      fsize <- nrow(m)
      break
    }
  }
  if (is.null(fsize)) stop("At least one relatedness matrix must be provided.")

  # ------------------------------------------------------------------
  # Build the list of mxMatrix objects and the algebra terms in lockstep
  # so we never reference a matrix or variance component that doesn't exist.
  # ------------------------------------------------------------------
  mat_list <- list(
    OpenMx::mxMatrix("Iden", nrow = fsize, ncol = fsize, name = "I"),
    OpenMx::mxMatrix("Unit", nrow = fsize, ncol = fsize, name = "U")
  )

  algebra_terms <- character(0)

  if (!is.null(Addmat)) {
    mat_list <- c(mat_list, list(
      OpenMx::mxMatrix("Symm",
        nrow = fsize, ncol = fsize,
        values = as.matrix(Addmat), name = "A"
      )
    ))
    algebra_terms <- c(algebra_terms, "(A %x% ModelOne.Vad)")
  }
  if (!is.null(Dmgmat)) {
    mat_list <- c(mat_list, list(
      OpenMx::mxMatrix("Symm",
        nrow = fsize, ncol = fsize,
        values = as.matrix(Dmgmat), name = "D"
      )
    ))
    algebra_terms <- c(algebra_terms, "(D %x% ModelOne.Vdd)")
  }
  if (!is.null(Nucmat)) {
    mat_list <- c(mat_list, list(
      OpenMx::mxMatrix("Symm",
        nrow = fsize, ncol = fsize,
        values = as.matrix(Nucmat), name = "Cn"
      )
    ))
    algebra_terms <- c(algebra_terms, "(Cn %x% ModelOne.Vcn)")
  }
  if (!is.null(Extmat)) {
    # Extmat signals "include Vce"; the algebra always uses U (unit matrix)
    algebra_terms <- c(algebra_terms, "(U %x% ModelOne.Vce)")
  }
  if (!is.null(Amimat)) {
    mat_list <- c(mat_list, list(
      OpenMx::mxMatrix("Symm",
        nrow = fsize, ncol = fsize,
        values = as.matrix(Amimat), name = "Am"
      )
    ))
    algebra_terms <- c(algebra_terms, "(Am %x% ModelOne.Vam)")
  }
  if (!is.null(Mtdmat)) {
    mat_list <- c(mat_list, list(
      OpenMx::mxMatrix("Symm",
        nrow = fsize, ncol = fsize,
        values = as.matrix(Mtdmat), name = "Mt"
      )
    ))
    algebra_terms <- c(algebra_terms, "(Mt %x% ModelOne.Vmt)")
  }

  # Unique environment is always included
  algebra_terms <- c(algebra_terms, "(I %x% ModelOne.Ver)")

  algebra_str <- paste(algebra_terms, collapse = " + ")

  # Assemble the model via do.call so that the dynamic mat_list is unpacked

  if (type == "continuous") {
  model_args <- c(
    list(name = group_name),
    mat_list,
    list(
      OpenMx::mxData(observed = full_df_row, type = "raw", sort = FALSE),
      OpenMx::mxMatrix("Full",
        nrow = 1, ncol = fsize, name = "M", free = TRUE,
        labels = "meanLI", dimnames = list(NULL, obs_ids)
      ),
      OpenMx::mxAlgebraFromString(algebra_str,
        name = "V", dimnames = list(obs_ids, obs_ids)
      ),
      OpenMx::mxExpectationNormal(covariance = "V", means = "M"),
      OpenMx::mxFitFunctionML()
    )
  )

  return(do.call(OpenMx::mxModel, model_args))
  } else if (type %in% c("binary", "ordinal")) {

    if (type == "binary") {
      nthresh <- 1L
    } else {
      nthresh <- as.integer(nthresh)
    }


    # Compute threshold starting values: accept a full vector or auto-space
    if (length(thresh_start) == nthresh) {
      thresh_values <- thresh_start
    } else {
      thresh_values <- thresh_start[1] + seq(-(nthresh - 1) / 2, (nthresh - 1) / 2, length.out = nthresh)
    }

    thresh_labels <- paste0("thresh", seq_len(nthresh))
    if (equate_thresholds) {
      thresh_labels <- paste0("thresh", seq_len(nthresh), "_eq")
    }
    model_args <- c(
      list(name = group_name),
      mat_list,
      list(
        OpenMx::mxData(observed = full_df_row, type = "raw", sort = FALSE),
        OpenMx::mxMatrix("Full",
                         nrow = 1, ncol = fsize, name = "M",
                         free = FALSE, values = 0,
                         dimnames = list(NULL, obs_ids)
        ),
        OpenMx::mxAlgebraFromString(algebra_str,
                                    name = "Vraw", dimnames = list(obs_ids, obs_ids)
        ),
        OpenMx::mxAlgebra(
          vec2diag(1 / sqrt(diag2vec(Vraw))),
          name = "iSD"
        ),
        OpenMx::mxAlgebra(
          iSD %*% Vraw %*% iSD,
          name = "R",
          dimnames = list(obs_ids, obs_ids)
        ),
        OpenMx::mxMatrix("Full",
                         nrow = nthresh, ncol = fsize, name = "Th",
                         free = thresh_free,
                         values = thresh_values,
                         labels = thresh_labels,
                         dimnames = list(paste0("thr", seq_len(nthresh)), obs_ids)
        ),
        OpenMx::mxExpectationNormal(
          covariance = "R",
          means = "M",
          thresholds = "Th",
          dimnames = obs_ids
        ),
        OpenMx::mxFitFunctionML()
      )
    )

    return(do.call(OpenMx::mxModel, model_args))
  }
}

#' Build family group models
#'
#' This function constructs OpenMx models for multiple family groups based on
#' provided relatedness matrices and observed data.
#'
#' @param dat A data frame where each row represents a family group and columns correspond to observed variables.
#' @param obs_ids A character vector of individual IDs corresponding to the columns of \code{dat}
#'   and the rows/columns of the relatedness matrices.
#' @param Addmat Additive genetic relatedness matrix.
#' @param Nucmat Nuclear family shared environment relatedness matrix.
#' @param Extmat Extended family shared environment relatedness matrix.
#' @param Mtdmat Mitochondrial genetic relatedness matrix.
#' @param Amimat Additive by mitochondrial interaction relatedness matrix.
#' @param Dmgmat Dominance genetic relatedness matrix.
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
  type = c("continuous", "binary", "ordinal"),
  nthresh = 1,
  thresh_start = 0,
  thresh_free = TRUE,
  equate_thresholds = TRUE

) {
  .require_openmx("buildFamilyGroups")

  numfam <- nrow(dat)
  groups <- vector("list", numfam)

  type <- match.arg(type)

  for (afam in seq_len(numfam)) {
    if (type %in% c("binary", "ordinal")) {
      # Preserve ordered-factor structure for threshold models
      full_df_row <- dat[afam, , drop = FALSE]
    } else {
      full_df_row <- matrix(dat[afam, ], nrow = 1, dimnames = list(NULL, obs_ids))
    }
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
      type = type,
      nthresh = nthresh,
      thresh_start = thresh_start,
      thresh_free = thresh_free,
      equate_thresholds = equate_thresholds
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
#'
#' @param model_name Name of the overall pedigree model.
#' @param vars A named list or vector of initial variance component values.
#' @param group_models A list of OpenMx models for each family group.
#' @param ci Logical. If TRUE, include confidence interval computations for the variance components. Default is FALSE
#' @return An OpenMx pedigree model combining variance components and family groups.
#' @export

buildPedigreeMx <- function(model_name, vars, group_models,
                            ci = FALSE) {
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
    if (!is.null(m$Vraw) && !is.null(m$Vraw$formula)) {
      deparse(m$Vraw$formula, width.cutoff = 500L)
    } else if (!is.null(m$V) && !is.null(m$V$formula)) {
      deparse(m$V$formula, width.cutoff = 500L)
    } else {
      ""
    }
  }, character(1))
  all_formulas <- paste(all_formulas, collapse = " ")

  flags <- lapply(vc_map, function(pat) grepl(pat, all_formulas, fixed = TRUE))

  # Auto-detect maxOrdinalPerBlock from the largest family group that

  # uses thresholds, so users don't hit OpenMx's default 20-person limit
  has_thresh <- vapply(group_models, function(m) !is.null(m$Th), logical(1))
  if (any(has_thresh)) {
    max_block <- max(vapply(group_models[has_thresh], function(m) {
      ncol(m$Th$values)
    }, integer(1)))
  } else {
    max_block <- NULL
  }

  mod <- OpenMx::mxModel(
    model_name,
    buildPedigreeModelCovariance(
      vars,
      Vad = isTRUE(flags$Vad),
      Vdd = isTRUE(flags$Vdd),
      Vcn = isTRUE(flags$Vcn),
      Vce = isTRUE(flags$Vce),
      Vmt = isTRUE(flags$Vmt),
      Vam = isTRUE(flags$Vam),
      Ver = isTRUE(flags$Ver)
    ),
    group_models,
    OpenMx::mxFitFunctionMultigroup(group_names),
    ci = if (ci & any(flags$Vad, flags$Vdd, flags$Vcn, flags$Vce, flags$Vmt, flags$Vam, flags$Ver)) {
      OpenMx::mxCI(c("vad", "vdd", "vcn", "vce", "vmt", "vam", "ver")[c(flags$Vad, flags$Vdd, flags$Vcn, flags$Vce, flags$Vmt, flags$Vam, flags$Ver)])
    } else {
      NULL
    }
  )

  if (!is.null(max_block) && max_block > 20L) {
    mod <- OpenMx::mxOption(mod, "maxOrdinalPerBlock", max_block)
  }

  mod
}

#' Fit an OpenMx pedigree model to observed data
#'
#' This function constructs and fits an OpenMx model for a pedigree using
#' specified variance components and family group models.
#'
#' @param model_name Character. Name for the overall OpenMx model. Default is "PedigreeModel".
#' @param vars A named list or vector of initial variance component values.
#' @param data A matrix or data frame of observed data, where each row is a family
#'   and columns correspond to individuals. Only used when \code{group_models} is NULL.
#' @param group_models Optional list of pre-built OpenMx family group models
#'   (from \code{\link{buildOneFamilyGroup}}). If NULL, they are generated from \code{data}
#'   using the provided relatedness matrices.
#' @param intervals Logical. If TRUE (default), compute confidence intervals for the parameters using \code{mxSE} and \code{mxCI}.
#' @param Addmat Additive genetic relatedness matrix. Required when \code{group_models} is NULL.
#' @param Nucmat Common nuclear environment relatedness matrix. Optional.
#' @param Extmat Common extended environment relatedness matrix. Optional.
#' @param Mtdmat Mitochondrial relatedness matrix. Optional.
#' @param Amimat Additive-by-mitochondrial interaction matrix. Optional.
#' @param Dmgmat Dominance genetic relatedness matrix. Optional.
#' @param tryhard Logical. If TRUE (default), use \code{mxTryHard} for robust optimization;
#'   if FALSE, use \code{mxRun}.
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
  tryhard = TRUE,
  intervals = TRUE,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  type = c("continuous", "binary", "ordinal"),
  nthresh = 1,
  thresh_start = 0,
  thresh_free = TRUE,
  equate_thresholds = TRUE
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
      type = type,
      nthresh = nthresh,
      thresh_start = thresh_start,
      thresh_free = thresh_free,
      equate_thresholds = equate_thresholds
    )
  }

  pedigree_model <- buildPedigreeMx(
    model_name = model_name,
    vars = vars,
    group_models = group_models
  )
  if (tryhard == TRUE) {
    fitted_model <- OpenMx::mxTryHard(pedigree_model, silent = TRUE, extraTries = 10, intervals = intervals)
  } else {
    fitted_model <- OpenMx::mxRun(pedigree_model, intervals = intervals)
  }
  fitted_model
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

#' Align Phenotype Vector to Ordered Factor Data Frame
#'
#' This function takes a pedigree data frame, a specified phenotype column, and a vector of IDs
#' to keep, and returns a single-row data frame of ordered factors suitable for OpenMx threshold
#' models (binary or ordinal).
#'
#' @param ped A data frame representing the pedigree.
#' @param phenotype A character string specifying the phenotype column name.
#' @param keep_ids A vector of IDs for which the phenotype values should be extracted.
#' @param levels A vector of the ordered levels for the factor (e.g., \code{c(0, 1)} for binary).
#' @param personID A character string specifying the ID column name. Default is "ID".
#' @return A single-row data frame of ordered factors with columns named after \code{keep_ids}.
#' @export
alignPhenToOrdinal <- function(ped, phenotype, keep_ids, levels, personID = "ID") {
  obs_ids <- make.names(as.character(keep_ids))
  pheno_vals <- ped[[phenotype]][match(as.character(keep_ids), as.character(ped[[personID]]))]

  out <- as.data.frame(
    setNames(
      lapply(pheno_vals, function(x) ordered(x, levels = levels)),
      obs_ids
    ),
    stringsAsFactors = FALSE
  )

  out
}
