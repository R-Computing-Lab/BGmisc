#' Standardize Column Names in a Dataframe (Internal)
#'
#' This internal function standardizes the column names of a given dataframe.
#' It utilizes regular expressions and the `tolower()` function to match column names
#' against a list of predefined standard names. The approach is case-insensitive and
#' allows for flexible matching of column names.
#'
#' @param df A dataframe whose column names need to be standardized.
#' @param verbose A logical indicating whether to print progress messages.
#' @return A dataframe with standardized column names.
#'
#' @keywords internal
standardize_colnames <- function(df, verbose = FALSE) {
  # Internal mapping of standardized names to possible variants
  mapping <- list(
    "fam" = "^(?:fam(?:ily)?(?:id)?)",
    "ID" = "^(?:i(?:d$|ndiv(?:idual)?)|p(?:erson)?id)",
    "gen" = "^(?:gen(?:s|eration)?)",
    "dadID" = "^(?:d(?:ad)?id|fatherid)",
    "momID" = "^(?:m(?:om)?id|motherid)",
    "spt" = "^(?:s(?:pt)?id|spouse(?:id)?)",
    "twinID" = "^(?:twin(?:id)?)",
    "sex" = "^(?:sex|gender|female|m(?:a(?:le|n)|en)|wom[ae]n)"
  )
  if (verbose) {
    print("Standardizing column names...")
  }
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


#' Repair Pedigree
#'
#' This function applies a list of repair functions sequentially to a pedigree.
#'
#' @param ped A pedigree object.
#' @param repair_funs A list of functions to repair the pedigree.
#' @param verbose Logical. Indicates whether to print progress messages
#' @param check_sex Logical. Indicates that sex should be validated
#' @param check_parents Logical. Indicates that parents should be validated
#' @param check_id Logical. Indicates that IDs should be validated
#' @return A corrected pedigree.
repairPedigree <- function(
    ped,
    repair_funs = NULL,
    check_id = TRUE,
    check_sex = TRUE,
    check_parents = TRUE,
    verbose = FALSE) {
  corrected_ped <- ped <- standardize_colnames(ped)
  if (verbose) {
    print("Repairing pedigree...")
  }
  # applies a list of repair functions sequentially to a pedigree.
  if (!is.null(repair_funs)) {
    for (fun in repair_funs) {
      corrected_ped <- fun(corrected_ped)
    }
    return(corrected_ped)
    # if not provided, use the default repair functions
  } else if (is.null(repair_funs)) {
    if (check_id) {
      corrected_ped <- repairIDs(corrected_ped)
    }
    if (check_sex) {
      corrected_ped <- repairSex(corrected_ped)
    }
    if (check_parents) {
      corrected_ped <- repairParentIDs(corrected_ped)
    }
    return(corrected_ped)
  } else {
    print("You should never see this message. If you do, that means the repair_funs variable in repairPedigree is broken")
  }
}

# To do
# - Missing rows: Sometimes, ID-codes in the mother or father column do not exist in the ID-column. That is, the people listed as someone's parents sometimes do not have their own rows, with columns for their parents etc.
# - Wrong IDs: It is possible that the ID code written for e.g. someone's mother is simply written wrong. This is especially problematic if there are people in the file who actually have the code that was mistakenly given.
# - A person's child being registered as their parent: I randomly found a case of this. A girl was registered as her father's mother.
# - People existing in both the mother and father column. This can happen through error. And it can also happen when same-sex couples have children (e.g. through adoption or fertilization). In the MoBa sample we had some cases of this, where there were same-sex pairs with several children, and where it was switched around from one child to the next whether one or the other of these parents were the father or mother in the registry.


#' Validate Pedigrees
#'
#' This function validates pedigrees based on several criteria.
#' @param ped A pedigree object
#' @inheritParams repairPedigree
#' @param ... Additional arguments to be passed to \code{\link{validatePedigree}}
#' @return A logical indicating whether the pedigree is valid and a list of warnings and ids of potentially invalid relationships
#' @export
validatePedigree <- function(ped,
                             verbose = FALSE,
                             check_sex = TRUE,
                             check_parents = TRUE,
                             check_id = TRUE) {
  corrected_ped <- repairPedigree(ped$ID,
    ped$dadID,
    ped$momID,
    ped$sex,
    check_sex = check_sex,
    check_parents = check_parents,
    check_id = check_id
  )

  # Validation checks

  if (check_id) {
    if (verbose) {
      print("Checking IDs...")
    }
    id_valid <- all(corrected_ped$ID == ped$ID)
  } else {
    id_valid <- TRUE
  }
  if (check_parents) {
    if (verbose) {
      print("Checking parents...")
    }
    dadID_valid <- all(corrected_ped$dadID == ped$dadID)
    momID_valid <- all(corrected_ped$momID == ped$momID)
  } else {
    dadID_valid <- TRUE
    momID_valid <- TRUE
  }
  if (check_sex) {
    if (verbose) {
      print("Checking sex...")
    }
    sex_valid <- all(corrected_ped$sex == ped$sex)
  } else {
    sex_valid <- TRUE
  }


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
