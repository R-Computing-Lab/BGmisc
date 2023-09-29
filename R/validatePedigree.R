#' Standardize Column Names in a Dataframe (Internal)
#'
#' This internal function standardizes the column names of a given dataframe.
#' It is useful for ensuring that dataframes with varying column names can be processed by functions
#' expecting specific column names.
#'
#' @param df A dataframe whose column names need to be standardized.
#'
#' @return A dataframe with standardized column names.
#'
#' @keywords internal
standardize_colnames <- function(df) {
  # Internal mapping of standardized names to possible variants
  mapping <- list(
    "fam" = c(
      "fam", "Fam", "FAM",
      "famID", "FamID", "FAMID",
      "famid", "Famid", "FAMid",
      "famfam", "Famfam", "FAMFAM"
    ),
    "ID" = c(
      "ID", "id", "Id",
      "Indiv", "indiv", "INDIV",
      "individual", "Individual", "INDIVIDUAL"
    ),
    "gen" = c(
      "gen", "Gen", "GEN",
      "gens", "Gens", "GENS",
      "generation", "Generation", "GENERATION"
    ),
    "dadID" = c(
      "dadID", "DadID", "DADID",
      "fatherID", "FatherID", "FATHERID"
    ),
    "momID" = c(
      "momID", "MomID", "MOMID",
      "motherID", "MotherID", "MOTHERID"
    ),
    "spt" = c(
      "spt", "Spt", "SPT",
      "sptID", "SptID", "SPTID",
      "sptid", "Sptid", "SPTid",
      "spouse", "Spouse", "SPOUSE",
      "spouseID", "SpouseID", "SPOUSEID",
      "spouseid", "Spouseid", "SPOUSEid",
    ),
    "twinID" = c(
      "twinID", "twinid", "TWINID",
      "twin", "Twin", "TWIN"
    ),
    "sex" = c(
      "sex", "Sex", "SEX",
      "female", "Female", "FEMALE",
      "gender", "Gender", "GENDER",
      "male", "Male", "MALE",
      "man", "Man", "MAN",
      "men", "Men", "MEN",
      "woman", "Woman", "WOMAN",
      "women", "Women", "WOMEN"
    )
  )
  for (standard_name in names(mapping)) {
    for (variant in mapping[[standard_name]]) {
      if (variant %in% colnames(df)) {
        colnames(df)[colnames(df) == variant] <- standard_name
        break # Exit the loop once a match is found
      }
    }
  }

  return(df)
}


#' Repair Pedigree
#'
#' This function applies a list of repair functions sequentially to a pedigree.
#'
#' @param ped A pedigree object.
#' @param repair_funs A list of functions to repair the pedigree.
#' @return A corrected pedigree.
repairPedigree <- function(ped, repair_funs = NULL) {

  corrected_ped <- ped <- standardize_colnames(ped)

  # applies a list of repair functions sequentially to a pedigree.
  if (!is.null(repair_funs)) {
    for (fun in repair_funs) {
      corrected_ped <- fun(corrected_ped)
    }
    return(corrected_ped)
  # if not provided, use the default repair functions
  } else if (is.null(repair_funs)) {
    corrected_ped <- repairIDs(corrected_ped)
    corrected_ped <- repairParentIDs(corrected_ped)

    return(corrected_ped)
  } else {
    print("You should never see this message. If you do, that means the repair_funs variable in repairPedigree is broken")
  }
}

# Repair Missing IDs
repairMissingIDs <- function(ped) {
  # [logic to repair missing IDs]
  return(ped)
}

# Repairing IDs
repairIDs <- function(ped) {
  # Create a list to track the changes made
  changes <- list()
  # [logic to repair IDs]
  return(ped)
}

# Repairing Parent IDs
repairParentIDs <- function(ped) {
  # [logic to repair Parent IDs]
  missing_fathers <- which(is.na(ped$dadID) & !is.na(ped$momID))
  missing_mothers <- which(!is.na(ped$dadID) & is.na(ped$momID))
  validation_results$missing_fathers <- missing_fathers
  validation_results$missing_mothers <- missing_mothers
  return(ped)
}

#' Validate Pedigrees
#'
#' This function validates pedigrees based on several criteria.
#' @param ped A pedigree object
#' @param verbose A logical indicating whether to print progress messages
#' @param ... Additional arguments to be passed to \code{\link{validatePedigree}}
#' @return A logical indicating whether the pedigree is valid and a list of warnings and ids of potentially invalid relationships
#' @export
validatePedigree <- function(ped, verbose = FALSE) {
  corrected_ped <- repairPedigree(ped$ID,
                                  ped$dadID,
                                  ped$momID,
                                  ped$sex)

  # Validation checks
  id_valid <- all(corrected_ped$ID == ped$ID)
  dadID_valid <- all(corrected_ped$dadID == ped$dadID)
  momID_valid <- all(corrected_ped$momID == ped$momID)
  sex_valid <- all(corrected_ped$sex == ped$sex)

  # Compile results
  is_valid <- id_valid && dadID_valid && momID_valid && sex_valid

  # Prepare warnings and feedback
  warnings <- list()

  if (!id_valid) {
    warnings$id_warning <- "IDs in the corrected pedigree do not match the original IDs."
  }
  if (!dadID_valid) {
    warnings$dadID_warning <- "Father IDs in the corrected pedigree do not match the original IDs."
  }
  if (!momID_valid) {
    warnings$momID_warning <- "Mother IDs in the corrected pedigree do not match the original IDs."
  }
  if (!sex_valid) {
    warnings$sex_warning <- "Sex values in the corrected pedigree do not match the original values."
  }

  # Return results
  if (verbose) {
    return(list(is_valid = is_valid, corrected_ped = corrected_ped, warnings = warnings))
  } else if (is_valid) {
    return(corrected_ped)
  } else {
    print("Pedigree is not valid. Refer to the warnings for more details.")
    return(warnings)
  }
}
