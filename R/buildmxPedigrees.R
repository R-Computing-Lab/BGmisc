#' Create an mxModel for a pedigree
#'
#' This function builds an OpenMx model for a pedigree with specified variance components. It requires the OpenMx package.
#'
#' @param vars A named vector of initial variance component values. Default values are provided.
#' @param Vad Logical. Include additive genetic variance component. Default is TRUE.
#' @param Vdd Logical. Include dominance genetic variance component. Default is FALSE.
#' @param Vcn Logical. Include common nuclear family environment variance component. Default is TRUE
#' @param Vce Logical. Include common extended family environment variance component. Default is TRUE.
#' @param Vmt Logical. Include mitochondrial genetic variance component. Default is TRUE.
#' @param Vam Logical. Include additive by mitochondrial interaction variance component. Default is FALSE.
#' @param Ver Logical. Include unique environmental variance component. Default is TRUE.
#' @return An OpenMx model representing the pedigree with specified variance components.



buildPedigreeModelCovariance <- function(vars = c(ad2 = 0.5,
                                           dd2 = 0.3,
                                           cn2 = 0.2, ce2 = 0.4,
                                           mt2 = 0.1,
                                           am2 = 0.25,
                                           ee2 = 0.6),
                                         Vad = TRUE,
                                         Vdd = FALSE,
                                         Vcn = TRUE,
                                         Vce = TRUE,
                                         Vmt = TRUE,
                                         Vam = FALSE,
                                         Ver = TRUE
) {
  if (require(OpenMx) == FALSE) {
    stop("OpenMx package is required for buildPedigreeModelCovariance function. Please install it.")
  } else {
    library(OpenMx)
  }

  mxModel(
    "ModelOne",
    if (Vad) {
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars$ad2, labels = "vad", name = "Vad", lbound = 1e-10)
    },
    if (Vdd) {
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars$dd2, labels = "vdd", name = "Vdd", lbound = 1e-10)
    },
    if (Vcn) {
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars$cn2, labels = "vcn", name = "Vcn", lbound = 1e-10)
    },
    if (Vce) {
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars$ce2, labels = "vce", name = "Vce", lbound = 1e-10)
    },
    if (Vmt) {
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars$mt2, labels = "vmt", name = "Vmt", lbound = 1e-10)
    },
    if (Vam) {
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars$am2, labels = "vam", name = "Vam", lbound = 1e-10)
    },
    if (Ver) {
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars$ee2, labels = "ver", name = "Ver", lbound = 1e-10)
    }
  )
}

#' Build one family group model
#' This function constructs an OpenMx model for a single family group based on provided relatedness matrices and observed data.
#' @param group_name Name of the family group.
#' @param Addmat Additive genetic relatedness matrix.
#' @param Nucmat Nuclear family shared environment relatedness matrix.
#' @param Extmat Extended family shared environment relatedness matrix.
#' @param Mtdmat Mitochondrial genetic relatedness matrix.
#' @param Amimat Additive by mitochondrial interaction relatedness matrix.
#' @param Dmgmat Dominance genetic relatedness matrix.
#' @param full_df_row A matrix representing the observed data for the family group.
#' @param ytemp A vector of variable names corresponding to the observed data.
#' @return An OpenMx model for the specified family group.


buildOneFamilyGroup <- function(
  group_name,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  full_df_row,
  ytemp
) {
  if (require(OpenMx) == FALSE) {
    stop("OpenMx package is required for buildPedigreeModelCovariance function. Please install it.")
  } else {
    library(OpenMx)
  }

  if (!is.null(Addmat)) {
    fsize <- nrow(Addmat)
  } else if (!is.null(Nucmat)) {
    fsize <- nrow(Nucmat)
  } else if (!is.null(Extmat)) {
    fsize <- nrow(Extmat)
  } else if (!is.null(Mtdmat)) {
    fsize <- nrow(Mtdmat)
  } else if (!is.null(Amimat)) {
    fsize <- nrow(Amimat)
  } else if (!is.null(Dmgmat)) {
    fsize <- nrow(Dmgmat)
  } else {
    stop("At least one relatedness matrix must be provided.")
  }

  mxModel(
    name = group_name,
    mxMatrix("Iden", nrow = fsize, ncol = fsize, name = "I"),
    mxMatrix("Unit", nrow = fsize, ncol = fsize, name = "U"),
    if (!is.null(Addmat)) {
      mxMatrix("Symm", nrow = fsize, ncol = fsize, values = as.matrix(Addmat), name = "A")
    },
    if (!is.null(Dmgmat)) {
      mxMatrix("Symm", nrow = fsize, ncol = fsize, values = as.matrix(Dmgmat), name = "D")
    },
    if (!is.null(Nucmat)) {
      mxMatrix("Symm", nrow = fsize, ncol = fsize, values = as.matrix(Nucmat), name = "Cn")
    },
    if (!is.null(Extmat)) {
      mxMatrix("Symm", nrow = fsize, ncol = fsize, values = as.matrix(Extmat), name = "Ce")
    },
    if (!is.null(Amimat)) {
      mxMatrix("Symm", nrow = fsize, ncol = fsize, values = as.matrix(Amimat), name = "Am")
    },
    if (!is.null(Mtdmat)) {
      mxMatrix("Symm", nrow = fsize, ncol = fsize, values = as.matrix(Mtdmat), name = "Mt")
    },
    mxData(observed = full_df_row, type = "raw", sort = FALSE),
    mxMatrix("Full", nrow = 1, ncol = fsize, name = "M", free = TRUE, labels = "meanLI",
      dimnames = list(NULL, ytemp)),
    mxAlgebra(
      if (!is.null(Addmat)) {
        (A %x% ModelOne.Vad)
      } else {
        0
      }
      + if (!is.null(Extmat)) {
          (U %x% ModelOne.Vce)
        } else {
          0
        }
        + if (!is.null(Mtdmat)) {
            (Mt %x% ModelOne.Vmt)
          } else {
            0
          }
          + if (!is.null(Amimat)) {
              (Am %x% ModelOne.Vam)
            } else {
              0
            }
            + if (!is.null(Dmgmat)) {
                (D %x% ModelOne.Vdd)
              } else {
                0
              }
              +  if (!is.null(Nucmat)) {
                  (Cn %x% ModelOne.Vcn)
                } else {
                  0
                }

                + (I %x% ModelOne.Ver),
      name = "V", dimnames = list(ytemp, ytemp)
    ),
    mxExpectationNormal(covariance = "V", means = "M"),
    mxFitFunctionML()
  )
}
#' Build family group models
#' This function constructs OpenMx models for multiple family groups based on provided relatedness matrices and observed data.
#' @param dat A data frame where each row represents a family group and columns correspond to observed variables.
#' @param ytemp A vector of variable names corresponding to the observed data.
#' @param Addmat Additive genetic relatedness matrix.
#' @param Nucmat Nuclear family shared environment relatedness matrix.
#' @param Extmat Extended family shared environment relatedness matrix.
#' @param Mtdmat Mitochondrial genetic relatedness matrix.
#' @param Amimat Additive by mitochondrial interaction relatedness matrix.
#' @param Dmgmat Dominance genetic relatedness matrix.
#' @param prefix A prefix for naming the family groups. Default is "fam".
#' @return A list of OpenMx models for each family group.
#'

buildFamilyGroups <- function(
  dat, ytemp,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  prefix = "fam"
) {
  numfam <- nrow(dat)
  groups <- vector("list", numfam)

  for (afam in seq_len(numfam)) {
    full_df_row <- matrix(dat[afam, ], nrow = 1, dimnames = list(NULL, ytemp))
    groups[[afam]] <- buildOneFamilyGroup(
      group_name  = paste0(prefix, afam),
      Addmat      = Addmat,
      Nucmat      = Nucmat,
      Extmat      = Extmat,
      Mtdmat      = Mtdmat,
      Amimat      = Amimat,
      Dmgmat      = Dmgmat,
      full_df_row = full_df_row,
      ytemp       = ytemp
    )
  }

  groups
}
#' Build Pedigree mxModel
#' This function constructs an OpenMx pedigree model by combining variance component models and family group models.
#' @param model_name Name of the overall pedigree model.
#' @param vars A named vector of initial variance component values.
#' @param group_models A list of OpenMx models for each family group.
#' @return An OpenMx pedigree model combining variance components and family groups.
#'

buildPedigreeMx <- function(model_name, vars, group_models) {
  group_names <- vapply(group_models, function(m) m$name, character(1))
  mxModel(
    model_name,
    buildPedigreeModelCovariance(vars),
    group_models,
    mxFitFunctionMultigroup(group_names)
  )
}

#' fitPedigreeModel
#' Fit an OpenMx pedigree model to observed data.
#' This function constructs and fits an OpenMx model for a pedigree using specified variance components and family group models.
#'
#'

fitPedigreeModel <- function(
  model_name = "PedigreeModel",
  vars = c(ad2 = 0.5,
    dd2 = 0.3,
    cn2 = 0.2, ce2 = 0.4,
    mt2 = 0.1,
    am2 = 0.25,
    ee2 = 0.6),
  data,
  group_models = NULL
) {
  if (require(OpenMx) == FALSE) {
    stop("OpenMx package is required for fitPedigreeModel function. Please install it.")
  } else {
    library(OpenMx)
  }

  if (is.null(group_models)) {
    # generate them from data
    ytemp <- colnames(data)
    group_models <- buildFamilyGroups(
      dat = data,
      ytemp = ytemp
    )
  }


  pedigree_model <- buildPedigreeMx(model_name, vars, group_models)
  fitted_model <- mxRun(pedigree_model)
  return(fitted_model)
}
