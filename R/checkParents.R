# Challenge: Missing parents: If one parent is missing and the other one isn't, this needs to be handled somehow. Firstly, I think it can cause certain ways of estimating relatedness to give wrong numbers. And secondly, it requires us to make guesses in cases where e.g. two people have the same mother and missing fathers: They could then be either half-sibs, if the missing fathers are different people, or full sibs if not. This then also affects relatedness for their descendants.

#' Validates and Optionally Repairs Parent IDs in a Pedigree Dataframe
#'
#' This function takes a pedigree object and performs two main tasks:
#' 1. Checks for the validity of parent IDs, specifically looking for instances where only one parent ID is missing.
#' 2. Optionally repairs the missing parent IDs based on a specified logic.
#'
#' @param ped A dataframe representing the pedigree data with columns 'ID', 'dadID', and 'momID'.
#' @param verbose A logical flag indicating whether to print progress and validation messages to the console.
#' @param repair A logical flag indicating whether to attempt repairs on missing parent IDs.
#' @param repairsex A logical flag indicating whether to attempt repairs on sex of the parents
#' @param addphantoms A logical flag indicating whether to add phantom parents for missing parent IDs.
#' @param parentswithoutrow A logical flag indicating whether to add parents without a row in the pedigree.
#' @param famID Character. Column name for family IDs.
#' @param personID Character. Column name for individual IDs.
#' @param momID Character. Column name for maternal IDs.
#' @param dadID Character. Column name for paternal IDs.
#' @param code_male The code value used to represent male sex in the 'sex' column of \code{ped}.
#' @param code_female The code value used to represent female sex in the 'sex' column of \code{ped}.
#'
#' @return Depending on the value of `repair`, either a list containing validation results or a repaired dataframe is returned.
#' @examples
#' \dontrun{
#' ped <- data.frame(ID = 1:4, dadID = c(NA, 1, 1, 2), momID = c(NA, NA, 2, 2))
#' checkParentIDs(ped, verbose = TRUE, repair = FALSE)
#' }
#' @export
checkParentIDs <- function(ped, verbose = FALSE, repair = FALSE,
                           repairsex = repair,
                           addphantoms = repair,
                           parentswithoutrow = repair,
                           famID = "famID",
                           personID = "ID",
                           momID = "momID",
                           dadID = "dadID",
                           code_male = NULL,
                           code_female = NULL) {
  # Standardize column names in the input dataframe
  ped <- standardizeColnames(ped, verbose = verbose)

  # Initialize a list to store validation results
  validation_results <- list()

  if (verbose == TRUE) {
    cat("Step 1: Checking for single parents...\n")
  }


  # Identify missing fathers and mothers
  missing_fathers <- ped$ID[which(is.na(ped$dadID) & !is.na(ped$momID))]
  missing_mothers <- ped$ID[which(!is.na(ped$dadID) & is.na(ped$momID))]

  # Update the validation_results list
  if (length(missing_fathers) > 0) {
    validation_results$missing_fathers <- missing_fathers
  }
  if (length(missing_mothers) > 0) {
    validation_results$missing_mothers <- missing_mothers
  }

  validation_results$single_parents <- length(validation_results) > 0


  if (verbose && validation_results$single_parents) cat("Missing single parents found.\n")
  if (verbose && !validation_results$single_parents) cat("No missing single parents found.\n")


  # do all parents have a row

  # Check if all parents have a row in the pedigree
  all_parents <- unique(c(ped$momID, ped$dadID))
  all_parents <- all_parents[!is.na(all_parents)]
  rowless_parents <- all_parents[!all_parents %in% ped$ID]

  if (length(rowless_parents) > 0) {
    validation_results$rowless_parents <- rowless_parents
    if (verbose == TRUE) {
      cat("Some parents are not listed in the pedigree:\n")
      message(rowless_parents)
    }
  } else {
    if (verbose == TRUE) {
      cat("All parents are listed in the pedigree.\n")
    }
  }
  validation_results$missing_parents <- validation_results$single_parents & length(rowless_parents) > 0

  if (verbose == TRUE) {
    cat("Step 2: Determining the if moms are the same sex and dads are same sex\n")
  }
  # Determine modal sex values for moms and dads


  mom_results <- checkParentSex(ped, parent_col = "momID", verbose = verbose)
  dad_results <- checkParentSex(ped, parent_col = "dadID", verbose = verbose)

  validation_results$mom_sex <- mom_results$unique_sexes
  validation_results$dad_sex <- dad_results$unique_sexes

  validation_results$wrong_sex_moms <- mom_results$inconsistent_parents
  validation_results$wrong_sex_dads <- dad_results$inconsistent_parents
  validation_results$female_moms <- mom_results$all_same_sex
  validation_results$male_dads <- dad_results$all_same_sex
  if (!is.null(code_male) && !is.null(code_female)) {
    validation_results$male_var <- code_male
    validation_results$female_var <- code_female
    validation_results$sex_code_source <- "user_provided_codes"
  } else {
    validation_results$female_var <- mom_results$modal_sex
    validation_results$male_var <- dad_results$modal_sex
    validation_results$sex_code_source <- "modal_parent_sex"
  }
  # Are any parents in both momID and dadID?
  momdad <- intersect(ped$dadID, ped$momID)
  if (length(momdad) > 0 && !is.na(momdad)) {
    validation_results$parents_in_both <- momdad
    if (verbose == TRUE) {
      cat(paste(
        "Some individuals appear in both momID and dadID roles.\n",
        "These individuals are:\n"
      ))
      message(momdad)
    }
  }


  if (!repair) {
    if (verbose == TRUE) {
      cat("Validation Results:\n")
      message(validation_results)
    }
    return(validation_results)
  } else {
    if (verbose == TRUE) {
      cat("Validation Results:\n")
      message(validation_results)
      cat("Step 3: Attempting to repair missing parents...\n")
    }

    cat("REPAIR IN EARLY ALPHA\n")
    # Initialize a list to track changes made during repair
    changes <- list(
      corrected_mom_sex = character(0),
      corrected_dad_sex = character(0),
      phantom_dads_added = c(),
      phantom_moms_added = c()
    )
    if (repairsex) {
      # Fix sex of existing parents if wrong
      mom_indices <- match(ped$momID, ped$ID)
      dad_indices <- match(ped$dadID, ped$ID)


      if (length(validation_results$female_var) > 0 && !is.na(validation_results$female_var)) {
        corrected_moms <- ped$ID[mom_indices[!is.na(mom_indices)]]
        ped$sex[mom_indices[!is.na(mom_indices)]] <- validation_results$female_var
        changes$corrected_mom_sex <- corrected_moms
        if (verbose && length(corrected_moms) > 0) {
          cat("Corrected sex of moms for:", paste(corrected_moms, collapse = ", "), "\n")
        }
      } else {
        corrected_moms <- ped$ID[mom_indices[!is.na(mom_indices)]]
        ped$sex[mom_indices[!is.na(mom_indices)]] <- 0

        changes$corrected_mom_sex <- corrected_moms
        if (verbose && length(corrected_moms) > 0) {
          cat("Corrected sex of moms for:", paste(corrected_moms, collapse = ", "), "\n")
        }
      }
      if (length(validation_results$male_var) > 0 && !is.na(validation_results$male_var)) {
        corrected_dads <- ped$ID[dad_indices[!is.na(dad_indices)]]
        ped$sex[dad_indices[!is.na(dad_indices)]] <- validation_results$male_var
        changes$corrected_dad_sex <- corrected_dads
        if (verbose && length(corrected_dads) > 0) {
          cat("Corrected sex of dads for:", paste(corrected_dads, collapse = ", "), "\n")
        }
      } else {
        corrected_dads <- ped$ID[dad_indices[!is.na(dad_indices)]]
        ped$sex[dad_indices[!is.na(dad_indices)]] <- 1
        changes$corrected_dad_sex <- corrected_dads
        if (verbose && length(corrected_dads) > 0) {
          cat("Corrected sex of dads for:", paste(corrected_dads, collapse = ", "), "\n")
        }
      }
    }
  }
  if (addphantoms == TRUE) {
    # Generate new IDs
    newIDbase <- if (is.numeric(ped$ID)) max(ped$ID, na.rm = TRUE) + 1 else paste0("phantom-", seq_len(nrow(ped)))
    # Initialize a dataframe to store new entries
    # needs to have the same columns as ped
    # get the columns of ped
    ped_columns <- names(ped)
    #   ID famID momID dadID        name sex affected
    # 1  1     1    NA    NA Walder Frey   M        0
    # Create a new dataframe with the same columns as ped and same data types
    new_entries <- data.frame(matrix(ncol = length(ped_columns), nrow = 0))
    colnames(new_entries) <- ped_columns
    new_entry_base <- new_entries[1, ]

    added_counter <- 0

    inferred_sex <- if (length(validation_results$male_var) > 0 && !is.na(validation_results$male_var)) validation_results$male_var else 1

    # Add dads when missing
    for (idx in which(is.na(ped$dadID) & !is.na(ped$momID))) {
      newID <- if (is.numeric(ped$ID)) newIDbase + added_counter else paste0("phantom-dad-", ped$ID[idx])
      added_counter <- added_counter + 1
      ped$dadID[idx] <- newID
      if ("famID" %in% names(ped)) {
        newFAMID <- unique(ped$famID[idx])
        newFAMID <- newFAMID[!is.na(newFAMID)]

        new_entry <- addParentRow(new_entry_base, newID = newID, dadID = NA, momID = NA, sex = inferred_sex, famID = newFAMID)
      } else {
        new_entry <- addParentRow(new_entry_base, newID = newID, dadID = NA, momID = NA, sex = inferred_sex)
      }
      new_entries <- rbind(new_entries, new_entry)
    }

    # Add moms when missing
    inferred_sex <- if (length(validation_results$female_var) > 0 && !is.na(validation_results$female_var)) {
      validation_results$female_var
    } else {
      0
    }

    for (idx in which(!is.na(ped$dadID) & is.na(ped$momID))) {
      newID <- if (is.numeric(ped$ID)) newIDbase + added_counter else paste0("phantom-mom-", ped$ID[idx])
      added_counter <- added_counter + 1
      ped$momID[idx] <- newID

      if ("famID" %in% names(ped)) {
        newFAMID <- unique(ped$famID[idx])
        newFAMID <- newFAMID[!is.na(newFAMID)]

        new_entry <- addParentRow(new_entry_base, newID = newID, dadID = NA, momID = NA, sex = inferred_sex, famID = newFAMID)
      } else {
        new_entry <- addParentRow(new_entry_base, newID = newID, dadID = NA, momID = NA, sex = inferred_sex)
      }
      new_entries <- rbind(new_entries, new_entry)
    }

    # merge the new entries with the original ped
    ped <- merge(ped, new_entries, all = TRUE)

    if (verbose == TRUE) {
      cat("Added", nrow(new_entries), "phantom parents.\n")
    }
    changes$phantom_dads_added <- new_entries$ID[which(new_entries$sex == validation_results$male_var)]
    changes$phantom_moms_added <- new_entries$ID[which(new_entries$sex == validation_results$female_var)]
    if (verbose && length(changes$phantom_dads_added) > 0) {
      cat("Added phantom dads for:", paste(changes$phantom_dads_added, collapse = ", "), "\n")
    }
    if (verbose && length(changes$phantom_moms_added) > 0) {
      cat("Added phantom moms for:", paste(changes$phantom_moms_added, collapse = ", "), "\n")
    }
  }
  # add parents who appear in momID or dadID but are missing from ID
  if (parentswithoutrow) {
    # Add parents who appear in momID or dadID but are missing from ID
    ped <- addRowlessParents(ped = ped, verbose = verbose, validation_results = validation_results)
  }

  if (verbose == TRUE) {
    cat("Changes Made:\n")
    message(changes)
  }

  # restore orginal names that the user orginally provided
  names(ped)[names(ped) == "ID"] <- personID
  names(ped)[names(ped) == "momID"] <- momID
  names(ped)[names(ped) == "dadID"] <- dadID
  names(ped)[names(ped) == "famID"] <- famID
  return(ped)
}
#' Repair Parent IDs
#'
#' This function repairs parent IDs in a pedigree.
#' @inheritParams checkParentIDs
#' @inherit checkParentIDs details
#' @return A corrected pedigree
repairParentIDs <- function(ped, verbose = FALSE,
                            famID = "famID",
                            personID = "ID",
                            momID = "momID",
                            dadID = "dadID") {
  checkParentIDs(
    ped = ped, verbose = verbose, repair = TRUE,
    personID = personID, momID = momID, dadID = dadID, famID = famID
  )
}

#' Add addRowlessParents
#'
#' This function adds parents who appear in momID or dadID but are missing from ID
#' @inheritParams checkParentIDs
#' @param validation_results validation results

addRowlessParents <- function(ped, verbose, validation_results) {
  # Add parents who appear in momID or dadID but are missing from ID
  new_entries <- data.frame()

  listed_parents <- unique(c(ped$momID, ped$dadID))
  listed_parents <- listed_parents[!is.na(listed_parents)]

  existing_ids <- ped$ID
  missing_parents <- setdiff(listed_parents, existing_ids)

  if (length(missing_parents) > 0) {
    if (verbose == TRUE) {
      cat("Adding parents who were listed in momID/dadID but missing from ID:\n")
      message(missing_parents)
    }
    ped_columns <- names(ped)
    # Create a new dataframe with the same columns as ped and same data types
    new_entries <- data.frame(matrix(ncol = length(ped_columns), nrow = 0))
    colnames(new_entries) <- ped_columns

    new_entry_base <- new_entries[1, ]


    for (pid in missing_parents) {
      role <- unique(
        c(
          if (pid %in% ped$momID) "mom" else NULL,
          if (pid %in% ped$dadID) "dad" else NULL
        )
      )
      inferred_sex <- if ("mom" %in% role) validation_results$female_var else validation_results$male_var

      if ("famID" %in% names(ped)) {
        newFAMID <- unique(ped$famID[which(ped$momID == pid | ped$dadID == pid)])
        newFAMID <- newFAMID[!is.na(newFAMID)]


        if (length(newFAMID) > 1) {
          newFAMID <- NA
        }

        new_entry <- addParentRow(new_entry_base, newID = pid, dadID = NA, momID = NA, sex = inferred_sex, famID = newFAMID)
      } else {
        new_entry <- addParentRow(new_entry_base, newID = pid, dadID = NA, momID = NA, sex = inferred_sex)
      }

      new_entries <- rbind(new_entries, new_entry)
    }

    ped <- merge(ped, new_entries, all = TRUE)
    if (verbose == TRUE) {
      cat("Added phantom parents for:", paste(new_entries$ID, collapse = ", "), "\n")
    }
  }


  return(ped)
}


#' Create a properly formatted parent row for the pedigree
#'
#' @param template_row A single row from ped, used as a template for column structure
#' @param newID The new parent's ID
#' @param sex The new parent's sex value (e.g., 0 for female, 1 for male, or "F"/"M")
#' @param momID The new parent's mother ID (default is NA)
#' @param dadID The new parent's father ID (default is NA)
#' @param famID The new parent's family ID (default is NA)
#' @return A single-row dataframe for the new parent
addParentRow <- function(template_row, newID, sex,
                         momID = NA,
                         dadID = NA,
                         famID = NA) {
  new_row <- template_row
  new_row[] <- NA # set all columns to NA
  new_row$ID <- newID
  new_row$momID <- momID
  new_row$dadID <- dadID
  new_row$sex <- sex

  if ("famID" %in% names(template_row)) {
    new_row$famID <- famID
  }
  # You can add more column initializations here if needed

  return(new_row)
}
