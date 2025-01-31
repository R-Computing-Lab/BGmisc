#' Validates and Optionally Repairs Unique IDs in a Pedigree Dataframe
#'
#' This function takes a pedigree object and performs two main tasks:
#' 1. Checks for the uniqueness of individual IDs.
#' 2. Optionally repairs non-unique IDs based on a specified logic.
#'
#' @param ped A dataframe representing the pedigree data with columns `ID`, `dadID`, and `momID`.
#' @param verbose A logical flag indicating whether to print progress and validation messages to the console.
#' @param repair A logical flag indicating whether to attempt repairs on non-unique IDs.
#'
#' @return Depending on `repair` value, either returns a list containing validation results or a repaired dataframe
#' @examples
#' \dontrun{
#' ped <- data.frame(ID = c(1, 2, 2, 3), dadID = c(NA, 1, 1, 2), momID = c(NA, NA, 2, 2))
#' checkIDs(ped, verbose = TRUE, repair = FALSE)
#' }
#' @export
checkIDs <- function(ped, verbose = FALSE, repair = FALSE) {
  # Standardize column names in the input dataframe
  ped <- standardizeColnames(ped)

  # Initialize a list to store validation results
  validation_results <- list()

  if (verbose) {
    cat("Checking IDs...\n")
    cat("Step 1: Checking for unique IDs...\n")
  }

  # Identify non-unique IDs
  duplicated_ids <- ped$ID[duplicated(ped$ID) | duplicated(ped$ID, fromLast = TRUE)]


  # Update the validation_results list
  if (length(duplicated_ids) > 0) {
    if (verbose) {
      cat(paste0(length(duplicated_ids), " non-unique IDs found.\n"))
    }
    validation_results$all_unique_ids <- FALSE
    validation_results$total_non_unique_ids <- length(duplicated_ids)
    validation_results$non_unique_ids <- unique(duplicated_ids)
  } else {
    if (verbose) {
      cat("All IDs are unique.\n")
    }
    validation_results$all_unique_ids <- TRUE
    validation_results$total_non_unique_ids <- 0
    validation_results$non_unique_ids <- NULL
  }
  if (verbose) {
    cat("Step 2: Checking for within row duplicats...\n")
    cat("Is own father?\n")
  }
  is_own_father <- ped$ID[ped$ID == ped$dadID & !is.na(ped$dadID)]
  if (verbose) {
    cat("Is own mother?\n")
  }
  is_own_mother <- ped$ID[ped$ID == ped$momID & !is.na(ped$momID)]
  if (verbose) {
    cat("Is mother father?\n")
  }
  duplicated_parents <- ped$ID[ped$dadID == ped$momID & !is.na(ped$dadID) & !is.na(ped$momID)]

  # get the total number of within row duplicates
  validation_results$total_own_father <- length(is_own_father)
  validation_results$total_own_mother <- length(is_own_mother)
  validation_results$total_duplicated_parents <- length(duplicated_parents)
  validation_results$total_within_row_duplicates <- sum(length(is_own_father), length(is_own_mother), length(duplicated_parents))
  # Update the validation_results list

  if (validation_results$total_within_row_duplicates > 0) {
    if (verbose) {
      cat(paste0(
        validation_results$total_within_row_duplicates,
        " within row duplicates found.\n"
      ))
    }
    validation_results$within_row_duplicates <- TRUE
    if (validation_results$total_own_father > 0) {
      validation_results$is_own_father_ids <- unique(is_own_father)
      if (verbose) {
        cat(paste0(
          validation_results$total_own_father,
          " individuals are their own fathers.\n"
        ))
      }
    }
    if (validation_results$total_own_mother > 0) {
      validation_results$is_own_mother_ids <- unique(is_own_mother)
      if (verbose) {
        cat(paste0(
          validation_results$total_own_mother,
          " individuals are their own mothers.\n"
        ))
      }
    }
    if (validation_results$total_duplicated_parents > 0) {
      validation_results$duplicated_parents_ids <- unique(duplicated_parents)
      if (verbose) {
        cat(paste0(
          validation_results$total_duplicated_parents,
          " individuals have the same mother and father.\n"
        ))
      }
    }
  } else {
    if (verbose) {
      cat("No within row duplicates found.\n")
    }
    validation_results$within_row_duplicates <- FALSE
    validation_results$total_within_row_duplicates <- 0
    validation_results$is_own_father_ids <- NULL
    validation_results$is_own_mother_ids <- NULL
    validation_results$duplicated_parents_ids <- NULL
  }
  if (verbose) {
    cat("Validation Results:\n")
    print(validation_results)
  }
  if (repair) {
    if (verbose) {
      cat("Attempting to repair:\n")
      cat("Step 1: Attempting to repair non-unique IDs...\n")
    }

    # Initialize a list to track changes made during repair
    changes <- list()
    if (verbose) {
      cat("Is the row a between-person duplicate?\n")
    }
    repaired_ped <- ped
    # if there are non-unique IDs
    if (length(validation_results$non_unique_ids) > 0) {
      # loop through each non-unique ID
      for (id in validation_results$non_unique_ids) {
        rows_with_id <- repaired_ped[repaired_ped$ID == id, ]
        # If all rows with the same ID are truly identical, keep only the first occurrence
        if (nrow(unique(rows_with_id)) == 1) {
          # Mark as removed in the changes list
          changes[[paste0("ID", id)]] <- "Removed duplicates"
          # Keep only the first row, remove the rest
          repaired_ped <- repaired_ped[-which(repaired_ped$ID == id)[-1], ] # Remove all but the first occurrence
        } else {
          # Mark as kept in the changes list
          changes[[paste0("ID", id)]] <- "Kept duplicates"
        }
      }
    }
    if (verbose) {
      cat("Step 2: No repair for parents who are their children at this time\n")
    }

    if (verbose) {
      cat("Changes Made:\n")
      print(changes)
    }
    return(repaired_ped)
  } else {
    return(validation_results)
  }
}

#' Repair Missing IDs
#'
#' This function repairs missing IDs in a pedigree.
#' @param ped A pedigree object
#' @param verbose A logical indicating whether to print progress messages
#' @return A corrected pedigree
repairIDs <- function(ped, verbose = FALSE) {
  checkIDs(ped = ped, verbose = verbose, repair = TRUE)
}
