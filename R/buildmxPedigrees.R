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
#' @param lbound Numeric.
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
    list(flag = Vad, name = "Vad", label = "vad",
         key = "ad2",
         lbound = lbound
         ),
    list(flag = Vdd, name = "Vdd", label = "vdd", key = "dd2",
         lbound = lbound
         ),
    list(flag = Vcn, name = "Vcn", label = "vcn", key = "cn2",
         lbound = lbound
         ),
    list(flag = Vce, name = "Vce", label = "vce", key = "ce2",
         lbound = lbound
         ),
    list(flag = Vmt, name = "Vmt", label = "vmt", key = "mt2",
         lbound = lbound
         ),
    list(flag = Vam, name = "Vam", label = "vam", key = "am2",
         lbound = lbound
         ),
    list(flag = Ver, name = "Ver", label = "ver", key = "ee2",
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
  condenseMatrixSlots = TRUE
) {
  .require_openmx("buildOneFamilyGroup")

  # Determine family size from first available matrix
  fsize <- NULL
  for (m in list(Addmat, Nucmat, Extmat, Mtdmat, Amimat, Dmgmat)) {
    if (!is.null(m)) {
      fsize <- nrow(m)
      break
    }
  }
  if (is.null(fsize)) stop("At least one relatedness matrix must be provided.")

  # If Extmat is requested but not supplied as a matrix, create a unit matrix
  # (all members share the extended environment equally).
  if (!is.null(Extmat) && !is.matrix(Extmat)) {
    Extmat <- matrix(1, nrow = fsize, ncol = fsize)
  }

  # ------------------------------------------------------------------
  # Build the list of mxMatrix objects and the algebra terms
  # Each entry: list(mat = input_matrix, mxname, algebra_term).
  # ------------------------------------------------------------------
  mat_spec <- list(
    list(mat = Addmat, mxname = "A",  term = "(A  %x% ModelOne.Vad)"),
    list(mat = Dmgmat, mxname = "D",  term = "(D  %x% ModelOne.Vdd)"),
    list(mat = Nucmat, mxname = "Cn", term = "(Cn %x% ModelOne.Vcn)"),
    list(mat = Extmat, mxname = "Ce", term = "(Ce %x% ModelOne.Vce)"),
    list(mat = Amimat, mxname = "Am", term = "(Am %x% ModelOne.Vam)"),
    list(mat = Mtdmat, mxname = "Mt", term = "(Mt %x% ModelOne.Vmt)")
  )
  active <- Filter(function(s) !is.null(s$mat), mat_spec)

if(condenseMatrixSlots) {
    relmat_list <- lapply(active, function(s) {
      condenseMatrixSlots(OpenMx::mxMatrix("Symm",
                                           nrow = fsize, ncol = fsize,
                                           values = as.matrix(s$mat), name = s$mxname
      ))
    })
    } else {
  relmat_list <- lapply(active, function(s) {
    OpenMx::mxMatrix("Symm",
      nrow = fsize, ncol = fsize,
      values = as.matrix(s$mat), name = s$mxname
    )
  })
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

  do.call(OpenMx::mxModel, model_args)
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
#' @param Extmat Common extended family environment relatedness matrix. When non-NULL,
#'   a Vce term scaled by this matrix is added to the covariance. If a non-matrix
#'   value (e.g. \code{TRUE}) is supplied, a unit matrix is created automatically.
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
  condenseMatrixSlots = TRUE
) {
  .require_openmx("buildFamilyGroups")

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
      condenseMatrixSlots = condenseMatrixSlots
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
#' @param condenseMatrixSlots Logical. If TRUE, use the mxCondenseMatrixSlots wrapper to optimize memory usage for large matrices. Default is FALSE.
#' @return An OpenMx pedigree model combining variance components and family groups.
#' @export

buildPedigreeMx <- function(model_name, vars, group_models,
                            ci = FALSE,
                            condenseMatrixSlots = TRUE

                            ) {
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

 OpenMx::mxModel(
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
#' @param Addmat Additive genetic relatedness matrix. Required when \code{group_models} is NULL.
#' @param Nucmat Common nuclear environment relatedness matrix. Optional.
#' @param Extmat Common extended environment relatedness matrix. Optional.
#' @param Mtdmat Mitochondrial relatedness matrix. Optional.
#' @param Amimat Additive-by-mitochondrial interaction matrix. Optional.
#' @param Dmgmat Dominance genetic relatedness matrix. Optional.
#' @param tryhard Logical. If TRUE (default), use \code{mxTryHard} for robust optimization;
#'   if FALSE, use \code{mxRun}.
#' @param intervals Logical. If TRUE (default), compute confidence intervals for the parameters using \code{mxSE} and \code{mxCI}.
#' @param extraTries Numeric. The number of extra optimization attempts to make when \code{tryhard} is TRUE. Default is 10.
#' @param condenseMatrixSlots Logical. If TRUE, use the mxCondenseMatrixSlots wrapper to optimize memory usage for large matrices. Default is FALSE.
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
  condenseMatrixSlots = TRUE
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
  if (tryhard == TRUE) {
    fitted_model <- OpenMx::mxTryHard(pedigree_model, silent = TRUE, extraTries = extraTries, intervals = intervals)
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

#' Condense Matrix Slots in an OpenMx Model
#'
#' This function takes an OpenMx model and applies the \code{mxCondenseMatrixSlots} wrapper to optimize memory usage for large matrices. This can be particularly beneficial when working with large pedigree models that include multiple relatedness matrices.
#' @param model An OpenMx model object for which to condense matrix slots. If
#' NULL, the function returns NULL.
#' @return An OpenMx model with condensed matrix slots, or NULL if the input model
#' is NULL.


condenseMatrixSlots <- function(model) {
  .require_openmx("condenseMatrixSlots")
  if(is.null(model)) return(NULL)
  #  no applicable method for `@` applied to an object of class "matrix"
#  if (is.matrix(model)) {
 #   return(model)
 # }
  OpenMx::imxConDecMatrixSlots(model)
}


