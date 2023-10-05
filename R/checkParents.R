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
#'
#' @return Depending on the value of `repair`, either a list containing validation results or a repaired dataframe is returned.
#' @examples
#' \dontrun{
#' ped <- data.frame(ID = 1:4, dadID = c(NA, 1, 1, 2), momID = c(NA, NA, 2, 2))
#' checkParentIDs(ped, verbose = TRUE, repair = FALSE)
#' }
#' @export
checkParentIDs <- function(ped, verbose = FALSE, repair = FALSE) {
  # Standardize column names in the input dataframe
  ped <- standardize_colnames(ped)

  # Initialize a list to store validation results
  validation_results <- list()

  if (verbose) {
    cat("Step 1: Checking for missing parents...\n")
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

  # If no missing parents are found
  if (length(validation_results) == 0) {
    if (verbose) {
      cat("No missing single parents found.\n")
    }
    validation_results$missing_parents <- FALSE
  }
  # are all moms the same sex?
  validation_results$mom_sex <- unique(ped$sex[ped$ID %in% ped$momID])

  if (length(validation_results$mom_sex) == 1) {
    if (verbose) {
      cat(paste0(
        "All moms are '",
        validation_results$mom_sex,
        "'.\n"
      ))
    }
    validation_results$female_moms <- 1
    validation_results$female_var <- validation_results$mom_sex
  } else {
    validation_results$female_moms <- FALSE
    # to do; find a way to code the proportions
  }
  # are all dads the same sex?
  validation_results$dad_sex <- unique(ped$sex[ped$ID %in% ped$dadID])
  if (length(validation_results$dad_sex) == 1) {
    if (verbose) {
      cat(paste0(
        "All dads are '",
        validation_results$dad_sex,
        "'.\n"
      ))
    }
    validation_results$male_dads <- 1
    validation_results$male_var <- validation_results$dad_sex
  } else {
    validation_results$male_dads <- FALSE
    # to do; find a way to code the proportions
  }

  # Are any parents in both momID and dadID?
  momdad <- intersect(ped$dadID, ped$momID)
  if (length(momdad) > 0) {
    validation_results$parents_in_both <- momdad
  }


  if (length(unique(ped$sex)) == 2) {
  }


  male_moms <- ped$ID[which(ped$dadID & !is.na(ped$momID))]

  if (repair) {
    if (verbose) {
      cat("Validation Results:\n")
      print(validation_results)
      cat("Step 2: Attempting to repair missing parents...\n")
    }
    cat("REPAIR IN EARLY ALPHA\n")
    # Initialize a list to track changes made during repair
    changes <- list()

    # [Insert logic to repair parent IDs here]

    # Update the pedigree dataframe after repair
    repaired_ped <- ped

    if (verbose) {
      cat("Changes Made:\n")
      print(changes)
    }
    return(repaired_ped)
  } else {
    return(validation_results)
  }
}
#' Repair Parent IDs
#'
#' This function repairs parent IDs in a pedigree.
#' @param ped A pedigree object
#' @param verbose A logical indicating whether to print progress messages
#' @return A corrected pedigree
repairParentIDs <- function(ped, verbose = FALSE) {
  checkParentIDs(ped = ped, verbose = verbose, repair = TRUE)
}
