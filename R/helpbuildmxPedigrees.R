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

