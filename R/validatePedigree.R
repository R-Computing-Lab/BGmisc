#' Standardize Column Names in a Dataframe (Internal)
#'
#' This internal function standardizes the column names of a given dataframe.
#' It utilizes regular expressions and the `tolower()` function to match column names
#' against a list of predefined standard names. The approach is case-insensitive and
#' allows for flexible matching of column names.
#'
#' @param df A dataframe whose column names need to be standardized.
#'
#' @return A dataframe with standardized column names.
#'
#' @keywords internal
standardize_colnames <- function(df) {
  # Internal mapping of standardized names to possible variants
  mapping <- list(
    "fam" = "^(?:fam(?:ily)?(?:id)?)",
    "ID" = "^(?:i(?:d$|ndiv(?:idual)?))",
    "gen" = "^(?:gen(?:s|eration)?)",
    "dadID" = "^(?:d(?:ad)?id|fatherid)",
    "momID" = "^(?:m(?:om)?id|motherid)",
    "spt" = "^(?:s(?:pt)?id|spouse(?:id)?)",
    "twinID" = "^(?:twin(?:id)?)",
    "sex" = "^(?:sex|gender|female|m(?:a(?:le|n)|en)|wom[ae]n)"
  )

  lowered_colnames <- tolower(colnames(df))
  for (standard_name in names(mapping)) {
    regex_pattern <- mapping[[standard_name]]
    matched_variant <- grep(regex_pattern, lowered_colnames, value = TRUE)
    if (length(matched_variant) > 0) {
      # Update the first match in original case
      original_matched <- colnames(df)[tolower(colnames(df)) == matched_variant[1]]
      colnames(df)[colnames(df) == original_matched] <- standard_name
    }
  }

  return(df)
}


#' recode_sex
#' An internal utility function for \code{plotPedigree} to handle the recoding of the 'sex' variable in a pedigree dataframe.
#'
#' @param ped A pedigree data.frame containing the relevant columns, including 'sex'.
#' @inheritParams plotPedigree
#' @return A modified version of the input data.frame \code{ped}, containing an additional or modified 'sex_recode' column where the 'sex' values are recoded according to \code{code_male}. NA values in the 'sex' column are preserved.
#' @keywords internal
#' @seealso \code{\link{plotPedigree}}
#'
recode_sex <- function(ped, code_male) {

  # Recode as "F" or "M" based on code_male, preserving NAs
  if (!is.null(code_male)) {
    # Initialize sex_recode as NA, preserving the length of the 'sex' column
    ped$sex_recode <- as.character(NA)
    ped$sex_recode[ped$sex != code_male & !is.na(ped$sex)] <- "F"
    ped$sex_recode[ped$sex == code_male] <- "M"
  } else {
    ped$sex_recode <- ped$sex
  }
  return(ped)
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
