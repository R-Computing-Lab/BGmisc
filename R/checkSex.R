#' Validates and Optionally Repairs Sex Coding in a Pedigree Dataframe
#'
#' This function performs two main tasks:
#' 1. Optionally recodes the 'sex' variable based on a given code for males.
#' 2. Optionally repairs the sex coding based on a specified logic.
#'
#' @param ped A dataframe representing the pedigree data with a 'sex' column.
#' @param code_male The code used to represent males in the 'sex' column. If both male and female are NULL, no recoding is performed.
#' @param code_female The code used to represent females in the 'sex' column. If both male and female are NULL, no recoding is performed.
#' @param verbose A logical flag indicating whether to print progress and validation messages to the console.
#' @param repair A logical flag indicating whether to attempt repairs on the sex coding.
#'
#' @return Depending on the value of `repair`, either a list containing validation results or a repaired dataframe is returned.
#' @examples
#' \dontrun{
#' ped <- data.frame(ID = c(1, 2, 3), sex = c("M", "F", "M"))
#' checkSex(ped, code_male = "M", verbose = TRUE, repair = FALSE)
#' }
#' @export
#'
checkSex <- function(ped, code_male = NULL, code_female = NULL,verbose = FALSE, repair = FALSE) {
  # Standardize column names in the input dataframe
  ped <- standardize_colnames(ped)

  # bypass the rest of the function if recode_only is TRUE

  # Initialize a list to store validation results
  validation_results <- list()

  # Initialize a list to track changes made during repair
  changes <- list()


  if (verbose) {
    cat("Step 1: Checking how many genders...\n")
  }
  # check how many genders

  validation_results$sex_unique <- unique(ped$sex)
  validation_results$sex_length <- length(unique(ped$sex))
  if (verbose) {
    cat(paste0(
      validation_results$sex_length, " unique values found.\n "
    ),
    paste0(validation_results$sex_unique))
  }


  if (repair) {
    if (verbose) {
      cat("Step 2: Attempting to repair sex coding...\n")
    }

    # [Insert logic to repair sex coding here]
    if (validation_results$sex_length == 2) {
      most_frequent_sex_dad <- names(sort(table(ped$sex[ped$ID %in% ped$dadID]), decreasing = TRUE))[1]
      original_ped <- ped
      ped <- recodeSex(ped, code_male = most_frequent_sex_dad)
      # Count and record the change
      num_changes <- sum(original_ped$sex != ped$sex)
      # Record the change and the count
      changes[[length(changes) + 1]] <- sprintf(
        "Recode sex based on most frequent sex in dads: %s. Total gender changes made: %d",
        most_frequent_sex_dad, num_changes)
    }
    # Update the pedigree dataframe after repair
    repaired_ped <- ped

    if (verbose) {
      cat("Changes Made:\n")
      print(changes)
    }
    return(list(validation_results, repaired_ped, changes))
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
#' ped <- data.frame(ID = c(1, 2, 3), sex = c("M", "F", "M"))
#' repairSex(ped, code_male = "M", verbose = TRUE)
#' }
#' @export
#'
#' @seealso \code{\link{checkSex}}
repairSex <- function(ped, verbose = FALSE, code_male = NULL, code_female = NULL) {
  checkSex(ped = ped, verbose = verbose, repair = TRUE, code_male = code_male, code_female = code_female)
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
recodeSex <- function(
    ped, verbose = FALSE, code_male = NULL, code_female = NULL, code_na = NULL,
    recode_male = "M", recode_female = "F", recode_na = NA_character_) {
  if (!is.null(code_na)) {
    ped$sex[ped$sex == code_na] <- NA
  }
  # Recode as "F" or "M" based on code_male, preserving NAs
  if (!is.null(code_male) & is.null(code_female)) {
    # Initialize sex_recode as NA, preserving the length of the 'sex' column
    ped$sex_recode <- recode_na
    ped$sex_recode[ped$sex != code_male & !is.na(ped$sex)] <- recode_female
    ped$sex_recode[ped$sex == code_male] <- recode_male
    # overwriting temp recode variable
    ped$sex <- ped$sex_recode
    ped$sex_recode <- NULL

  } else if(is.null(code_male) & !is.null(code_female)) {
    # Initialize sex_recode as NA, preserving the length of the 'sex' column
    ped$sex_recode <- recode_na
    ped$sex_recode[ped$sex != code_female & !is.na(ped$sex)] <- recode_male
    ped$sex_recode[ped$sex == code_female] <- recode_female
    # overwriting temp recode variable
    ped$sex <- ped$sex_recode
    ped$sex_recode <- NULL
  }
  return(ped)
}

#%%% to do,: give option to rename the variable to anything the user specificies, recode doesn't actually make changes
