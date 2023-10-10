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
  # If missing parents are found
  else {
    if (verbose) {
      cat("Missing single parents found.\n")
    }
    validation_results$missing_parents <- TRUE
  }

  if (verbose) {
    cat("Step 2: Determining the if moms are the same sex and dads are same sex\n")
  }

  # Determine the most frequent sex for moms and dads
  most_frequent_sex_mom <- names(sort(table(ped$sex[ped$ID %in% ped$momID]), decreasing = TRUE))[1]
  most_frequent_sex_dad <- names(sort(table(ped$sex[ped$ID %in% ped$dadID]), decreasing = TRUE))[1]

  # are all moms/dads the same sex?
  validation_results$mom_sex <- unique(ped$sex[ped$ID %in% ped$momID])
  validation_results$dad_sex <- unique(ped$sex[ped$ID %in% ped$dadID])

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
    # Check for inconsistent gender roles
    wrong_sex_moms <- ped$ID[which(ped$sex[ped$ID %in% ped$momID] != validation_results$female_var)]
    wrong_sex_dads <- ped$ID[which(ped$sex[ped$ID %in% ped$dadID] != validation_results$male_var)]



  # Are any parents in both momID and dadID?
  momdad <- intersect(ped$dadID, ped$momID)
  if (length(momdad) > 0) {
    validation_results$parents_in_both <- momdad
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
