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
  ped <- standardizeColnames(ped, verbose = verbose)

  # Initialize a list to store validation results
  validation_results <- list()

  if (verbose == TRUE) {
    cat("Checking IDs...\n")
    cat("Step 1: Checking for unique IDs...\n")
  }

  # Identify non-unique IDs
  id_check <- checkIDuniqueness(ped = ped, verbose = verbose)

  if (verbose == TRUE) {
    cat("Step 2: Checking for within row duplicats...\n")
  }
  row_check <- checkWithinRowDuplicates(ped = ped, verbose = verbose)

  validation_results <- c(id_check, row_check)

  if (verbose == TRUE) {
    cat("Validation Results:\n")
    message(validation_results)
  }
  if (repair == TRUE) {
    if (verbose == TRUE) {
      cat("Attempting to repair:\n")
      cat("Step 1: Attempting to repair non-unique IDs...\n")
    }

    # Initialize a list to track changes made during repair
    changes <- list()
    if (verbose == TRUE) {
      cat("Is the row a between-person duplicate?\n")
    }
    repaired_ped <- ped
    # if there are non-unique IDs
    if (length(validation_results$non_unique_ids) > 0) {
      # loop through each non-unique ID

      processed <- dropIdenticalDuplicateIDs(ped = repaired_ped,
        ids = validation_results$non_unique_ids,
        changes = changes)
      repaired_ped <- processed$ped
      changes <- processed$changes
    }
    if (verbose == TRUE) {
      cat("Step 2: No repair for parents who are their children at this time\n")
    }

    if (verbose == TRUE) {
      cat("Changes Made:\n")
      message(changes)
    }
    return(repaired_ped)
  } else {
    return(validation_results)
  }
}

#' Repair Missing IDs
#'
#' This function repairs missing IDs in a pedigree.
#' @inheritParams checkIDs
#' @return A corrected pedigree
#' @export
repairIDs <- function(ped, verbose = FALSE) {
  checkIDs(ped = ped, verbose = verbose, repair = TRUE)
}

#' Check for duplicated individual IDs
#'
#' This function checks for duplicated individual IDs in a pedigree.
#'
#' @inheritParams checkIDs
#' @return A list containing the results of the check
#'
checkIDuniqueness <- function(ped, verbose = FALSE) {
  # Identify non-unique IDs

  duplicated_ids <- ped$ID[duplicated(ped$ID) | duplicated(ped$ID, fromLast = TRUE)]

  if (verbose == TRUE) {
    if (length(duplicated_ids) > 0) {
      cat(length(duplicated_ids), " non-unique IDs found.\n")
    } else {
      cat("All IDs are unique.\n")
    }
  }

  # Update the validation_results list
  list(
    all_unique_ids = length(duplicated_ids) == 0,
    total_non_unique_ids = length(duplicated_ids),
    non_unique_ids = if (length(duplicated_ids) > 0) unique(duplicated_ids) else NULL
  )
}


#' Check for within-row duplicates (self-parents, same mom/dad)
#'
#' This function checks for within-row duplicates in a pedigree.
#'
#' @inheritParams checkIDs
#' @return A list containing the results of the check
#'
checkWithinRowDuplicates <- function(ped, verbose = FALSE) {
  # is the individual their own father or mother?
  is_own_father <- ped$ID[ped$ID == ped$dadID & !is.na(ped$dadID)]
  is_own_mother <- ped$ID[ped$ID == ped$momID & !is.na(ped$momID)]

  # is mother and father the same?
  duplicated_parents <- ped$ID[
    ped$dadID == ped$momID &
      !is.na(ped$dadID) & !is.na(ped$momID)
  ]

  # get the total number of within row duplicates
  total <- length(is_own_father) + length(is_own_mother) + length(duplicated_parents)

  if (verbose == TRUE) {
    if (total > 0) {
      cat(total, " within row duplicates found.\n")
      if (length(is_own_father) > 0) cat(length(is_own_father), " individuals are their own fathers.\n")
      if (length(is_own_mother) > 0) cat(length(is_own_mother), " individuals are their own mothers.\n")
      if (length(duplicated_parents) > 0) cat(length(duplicated_parents), " individuals have the same mother and father.\n")
    } else {
      cat("No within row duplicates found.\n")
    }
  }
  # Update the validation_results list
  list(
    total_own_father = length(is_own_father),
    total_own_mother = length(is_own_mother),
    total_duplicated_parents = length(duplicated_parents),
    total_within_row_duplicates = total,
    within_row_duplicates = total > 0,
    is_own_father_ids = if (length(is_own_father) > 0) unique(is_own_father) else NULL,
    is_own_mother_ids = if (length(is_own_mother) > 0) unique(is_own_mother) else NULL,
    duplicated_parents_ids = if (length(duplicated_parents) > 0) unique(duplicated_parents) else NULL
  )
}
