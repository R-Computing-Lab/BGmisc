#' Validates and Optionally Repairs Sex Coding in a Pedigree Dataframe
#'
#' This function performs two main tasks:
#' 1. Optionally recodes the 'sex' variable based on a given code for males.
#' 2. Optionally repairs the sex coding based on a specified logic.
#'
#' @param ped A dataframe representing the pedigree data with a 'sex' column.
#' @param code_male The code used to represent males in the 'sex' column. If NULL, no recoding is performed.
#' @param verbose A logical flag indicating whether to print progress and validation messages to the console.
#' @param repair A logical flag indicating whether to attempt repairs on the sex coding.
#' @param recode A logical flag indicating whether to recode the 'sex' variable.
#'
#' @return Depending on the value of `repair`, either a list containing validation results or a repaired dataframe is returned.
#' @examples
#' \dontrun{
#'   ped <- data.frame(ID = c(1, 2, 3), sex = c("M", "F", "M"))
#'   checkSex(ped, code_male = "M", verbose = TRUE, repair = FALSE, recode = TRUE)
#' }
#' @export
#'
checkSex <- function(ped, code_male = NULL, verbose = FALSE, repair = FALSE, recode = FALSE) {

  # Initialize a list to store validation results
  validation_results <- list()

  if (recode) {
    if (verbose) {
      cat("Step 1: Recoding sex variable...\n")
    }
    # Recode as "F" or "M" based on code_male, preserving NAs
    if (!is.null(code_male)) {
      # Initialize sex_recode as NA, preserving the length of the 'sex' column
      ped$sex_recode <- as.character(NA)
      ped$sex_recode[ped$sex != code_male & !is.na(ped$sex)] <- "F"
      ped$sex_recode[ped$sex == code_male] <- "M"
    } else {
      ped$sex_recode <- ped$sex
    }
    # Update the validation_results list
    validation_results$recoded_sex <- TRUE
  }

  if (repair) {
    if (verbose) {
      cat("Step 3: Attempting to repair sex coding...\n")
    }
    # Initialize a list to track changes made during repair
    changes <- list()
    # [Insert logic to repair sex coding here]
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



#' Repairs Sex Coding in a Pedigree Dataframe
#'
#' This function serves as a wrapper around `checkSex` to specifically handle
#' the repair of the sex coding in a pedigree dataframe.
#'
#' @inheritParams checkSex
#' @return A dataframe where the sex coding has been repaired.
#' @examples
#' \dontrun{
#'   ped <- data.frame(ID = c(1, 2, 3), sex = c("M", "F", "M"))
#'   repairSex(ped, code_male = "M", verbose = TRUE)
#' }
#' @export
#'
#' @seealso \code{\link{checkSex}}
repairSex <- function(ped, verbose = FALSE, code_male = NULL, recode = TRUE) {
  checkSex(ped = ped, verbose = verbose, repair = TRUE, code_male = code_male)
}

#' Recodes Sex Variable in a Pedigree Dataframe
#'
#' This function serves as a wrapper around `checkSex` to specifically handle
#' the recoding of the 'sex' variable in a pedigree dataframe.
#' @inheritParams checkSex
#' @inheritParams plotPedigree
#' @return A modified version of the input data.frame \code{ped}, containing an additional or modified 'sex_recode' column where the 'sex' values are recoded according to \code{code_male}. NA values in the 'sex' column are preserved.
#' @keywords internal
#' @seealso \code{\link{plotPedigree}}
recodeSex <- function(ped, verbose = FALSE,code_male = NULL) {
  checkSex(ped = ped, verbose = verbose, repair = FALSE, code_male = code_male, recode = TRUE)
}
