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
                           parentswithoutrow = repair) {
  # Standardize column names in the input dataframe
  ped_og <- ped
  ped <- standardizeColnames(ped, verbose = verbose)


  # Initialize a list to store validation results
  validation_results <- list()

  if (verbose) {
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
    if (verbose) {
      cat("Some parents are not listed in the pedigree:\n")
      print(rowless_parents)
    }
  } else {
    if (verbose) {
      cat("All parents are listed in the pedigree.\n")
    }
  }
  validation_results$missing_parents <- validation_results$single_parents & length(rowless_parents) > 0

  if (verbose) {
    cat("Step 2: Determining the if moms are the same sex and dads are same sex\n")
  }
  # Determine modal sex values for moms and dads
  mom_sexes <- ped$sex[ped$ID %in% ped$momID]
  dad_sexes <- ped$sex[ped$ID %in% ped$dadID]

  # Determine the most frequent sex for moms and dads
  most_frequent_sex_mom <- names(sort(table(mom_sexes), decreasing = TRUE))[1]
  most_frequent_sex_dad <- names(sort(table(dad_sexes), decreasing = TRUE))[1]

  # are all moms/dads the same sex?
  validation_results$mom_sex <- unique(mom_sexes)
  validation_results$dad_sex <- unique(dad_sexes)

  # Store the most frequent sex for moms and dads
  if (is.numeric(ped$sex)) {
    validation_results$female_var <- as.numeric(most_frequent_sex_mom)
    validation_results$male_var <- as.numeric(most_frequent_sex_dad)
  } else if (is.character(ped$sex) | is.factor(ped$sex)) {
    validation_results$female_var <- most_frequent_sex_mom
    validation_results$male_var <- most_frequent_sex_dad
  } else {
    print("You should never see this. If you do, then you have a problem with the data type of the sex variable")
  }

  # verbose
  if (length(validation_results$mom_sex) == 1) {
    if (verbose) {
      cat(paste0(
        "All moms are '",
        validation_results$female_var,
        "'.\n"
      ))
    }
    validation_results$female_moms <- TRUE
  } else {
    validation_results$female_moms <- FALSE
  }

  if (length(validation_results$dad_sex) == 1) {
    if (verbose) {
      cat(paste0(
        "All dads are '",
        validation_results$male_var,
        "'.\n"
      ))
    }
    validation_results$male_dads <- TRUE
  } else {
    validation_results$male_dads <- FALSE
  }
  # Check for inconsistent sex
  wrong_sex_moms <- ped$ID[which(ped$sex[ped$ID %in% ped$momID] != validation_results$female_var)]
  wrong_sex_dads <- ped$ID[which(ped$sex[ped$ID %in% ped$dadID] != validation_results$male_var)]

  validation_results$wrong_sex_moms <- wrong_sex_moms
  validation_results$wrong_sex_dads <- wrong_sex_dads

  if (verbose) {
    if (length(wrong_sex_moms) > 0) {
      cat("Some individuals listed as moms are not coded as", validation_results$female_var, "\n")
    }
    if (length(wrong_sex_dads) > 0) {
      cat("Some individuals listed as dads are not coded as", validation_results$male_var, "\n")
    }
  }

  # Are any parents in both momID and dadID?
  momdad <- intersect(ped$dadID, ped$momID)
  if (!is.na(momdad) && length(momdad) > 0) {
    validation_results$parents_in_both <- momdad
    if (verbose) {
      cat(paste(
        "Some individuals appear in both momID and dadID roles.\n",
        "These individuals are:\n"
      ))
      print(momdad)
    }
  }


  if (!repair) {
    if (verbose) {
      cat("Validation Results:\n")
      print(validation_results)
    }
    return(validation_results)
  } else {
    if (verbose) {
      cat("Validation Results:\n")
      print(validation_results)
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



      if (!is.na(validation_results$female_var)) {
        corrected_moms <- ped$ID[mom_indices[!is.na(mom_indices)]]
        ped$sex[mom_indices[!is.na(mom_indices)]] <- validation_results$female_var
        changes$corrected_mom_sex <- corrected_moms
        if (verbose && length(corrected_moms) > 0) {
          cat("Corrected sex of moms for:", paste(corrected_moms, collapse = ", "), "\n")
        }
      }
      if (!is.na(validation_results$male_var)) {
        corrected_dads <- ped$ID[dad_indices[!is.na(dad_indices)]]
        ped$sex[dad_indices[!is.na(dad_indices)]] <- validation_results$male_var
        changes$corrected_dad_sex <- corrected_dads
        if (verbose && length(corrected_dads) > 0) {
          cat("Corrected sex of dads for:", paste(corrected_dads, collapse = ", "), "\n")
        }
      }
    }
  }
  if (addphantoms) {
    # Generate new IDs
    newIDbase <- if (is.numeric(ped$ID)) max(ped$ID, na.rm = TRUE) + 1 else paste0("phantom-", seq_len(nrow(ped)))
    new_entries <- data.frame()
    added_counter <- 0

    # Add dads when missing
    for (idx in which(is.na(ped$dadID) & !is.na(ped$momID))) {
      new_id <- if (is.numeric(ped$ID)) newIDbase + added_counter else paste0("phantom-dad-", ped$ID[idx])
      added_counter <- added_counter + 1
      ped$dadID[idx] <- new_id
      new_entry <- ped[1, ]
      new_entry$ID <- new_id
      new_entry$dadID <- NA
      new_entry$momID <- NA
      new_entry$sex <- validation_results$male_var
      new_entries <- rbind(new_entries, new_entry)
    }

    # Add moms when missing
    for (idx in which(!is.na(ped$dadID) & is.na(ped$momID))) {
      new_id <- if (is.numeric(ped$ID)) newIDbase + added_counter else paste0("phantom-mom-", ped$ID[idx])
      added_counter <- added_counter + 1
      ped$momID[idx] <- new_id
      new_entry <- ped[1, ]
      new_entry$ID <- new_id
      new_entry$dadID <- NA
      new_entry$momID <- NA
      new_entry$sex <- validation_results$female_var
      new_entries <- rbind(new_entries, new_entry)
    }

    # merge the new entries with the original ped
    ped <- merge(ped, new_entries, all = TRUE)

    if (verbose) {
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
  # add phantom parents
  if (parentswithoutrow) {
    # Add parents who appear in momID or dadID but are missing from ID
    listed_parents <- unique(c(ped$momID, ped$dadID))
    listed_parents <- listed_parents[!is.na(listed_parents)]

    existing_ids <- ped$ID
    missing_parents <- setdiff(listed_parents, existing_ids)

    if (length(missing_parents) > 0) {
      if (verbose) {
        cat("Adding parents who were listed in momID/dadID but missing from ID:\n")
        print(missing_parents)
      }

      for (pid in missing_parents) {
        role <- unique(
          c(
            if (pid %in% ped$momID) "mom" else NULL,
            if (pid %in% ped$dadID) "dad" else NULL
          )
        )
        inferred_sex <- if ("mom" %in% role) validation_results$female_var else validation_results$male_var

        new_row <- ped[1, ]
        new_row$ID <- pid
        new_row$dadID <- NA
        new_row$momID <- NA
        new_row$sex <- inferred_sex
        new_entries <- rbind(new_entries, new_row)
      }
    }
    ped <- merge(ped, new_entries, all = TRUE)
  }

  if (verbose) {
    cat("Changes Made:\n")
    print(changes)
  }
  return(ped)
}
#' Repair Parent IDs
#'
#' This function repairs parent IDs in a pedigree.
#' @inheritParams checkParentIDs
#' @inherit checkParentIDs details
#' @return A corrected pedigree
repairParentIDs <- function(ped, verbose = FALSE) {
  checkParentIDs(ped = ped, verbose = verbose, repair = TRUE)
}

#' Add Phantom Parents
#'
#' This function adds phantom parents to a pedigree.
#' @inheritParams checkParentIDs
#' @param validation_results validation results

addPhantoms <- function(ped, verbose, validation_results) {
  # Add parents who appear in momID or dadID but are missing from ID
  new_entries <- data.frame()

  listed_parents <- unique(c(ped$momID, ped$dadID))
  listed_parents <- listed_parents[!is.na(listed_parents)]

  existing_ids <- ped$ID
  missing_parents <- setdiff(listed_parents, existing_ids)

  if (length(missing_parents) > 0) {
    if (verbose) {
      cat("Adding parents who were listed in momID/dadID but missing from ID:\n")
      print(missing_parents)
    }

    for (pid in missing_parents) {
      role <- unique(
        c(
          if (pid %in% ped$momID) "mom" else NULL,
          if (pid %in% ped$dadID) "dad" else NULL
        )
      )
      inferred_sex <- if ("mom" %in% role) validation_results$female_var else validation_results$male_var

      new_row <- ped[1, ]
      new_row$ID <- pid
      new_row$dadID <- NA
      new_row$momID <- NA
      new_row$sex <- inferred_sex
      new_entries <- rbind(new_entries, new_row)
    }
  }
  ped <- merge(ped, new_entries, all = TRUE)
  if (verbose) {
    cat("Added phantom parents for:", paste(new_entries$ID, collapse = ", "), "\n")
  }
  return(ped)
}
