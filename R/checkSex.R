#' Validates and Optionally Repairs Sex Coding in a Pedigree Dataframe
#'
#' This function checks and optionally modifies the coding of the biological 'sex' variable in a pedigree dataset.
#' It serves two primary purposes:
#' 1. Recodes the 'sex' variable based on specified codes for males and females, if provided.
#' 2. Identifies and optionally repairs inconsistencies in sex coding that could break the algorithm for constructing genetic pedigrees.
#'
#' The validation process identifies:
#' - The unique sex codes present in the dataset.
#' - Whether individuals listed as fathers or mothers have inconsistent sex codes.
#' - Instances where an individual's recorded sex does not align with their parental role.
#'
#' If `repair = TRUE`, the function standardizes sex coding by:
#' - Assigning individuals listed as fathers the most common male code in the dataset.
#' - Assigning individuals listed as mothers the most common female code.
#'
#'
#' @details This function uses the terms 'male' and 'female' in a biological context, referring to chromosomal and other biologically-based characteristics necessary for constructing genetic pedigrees. The biological aspect of sex used in genetic analysis (genotype) is distinct from the broader, richer concept of gender identity (phenotype).
#'
#' We recognize the importance of using language and methodologies that affirm and respect the full spectrum of gender identities.  The developers of this
#' package express unequivocal support for folx in the transgender
#' and LGBTQ+ communities.
#'
#' @param ped A dataframe representing the pedigree data with a 'sex' column.
#' @param code_male The current code used to represent males in the 'sex' column.
#' @param code_female The current code used to represent females in the 'sex' column. If both are NULL, no recoding is performed.
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
checkSex <- function(ped, code_male = NULL, code_female = NULL, verbose = FALSE, repair = FALSE) {
  # Standardize column names in the input dataframe
  ped <- standardizeColnames(ped, verbose = verbose)

  # TO DO: bypass the rest of the function if recode_only is TRUE

  # Initialize a list to store validation results
  validation_results <- list()


  if (verbose) {
    cat("Step 1: Checking how many sexes/genders...\n")
  }

  # Check unique values in 'sex'
  validation_results$sex_unique <- unique(ped$sex)
  validation_results$sex_length <- length(unique(ped$sex))
  if (verbose) {
    cat(paste0(validation_results$sex_length, " unique values found.\n"))
    cat(paste0("Unique values: ", paste0(validation_results$sex_unique, collapse = ", "), "\n"))
  }
  # Are there multiple sexes/genders in the list of dads and moms?

  table_sex_dad <- sort(table(ped$sex[ped$ID %in% ped$dadID]), decreasing = TRUE)
  table_sex_mom <- sort(table(ped$sex[ped$ID %in% ped$momID]), decreasing = TRUE)

  validation_results$all_sex_dad <- names(table_sex_dad)
  validation_results$all_sex_mom <- names(table_sex_mom)

  validation_results$most_frequent_sex_dad <- validation_results$all_sex_dad[1]
  validation_results$most_frequent_sex_mom <- validation_results$all_sex_mom[1]

  # List ids for dads that are female, moms that are male
  if (length(validation_results$all_sex_dad) > 1) {
    df_dads <- ped[ped$ID %in% ped$dadID, ]
    validation_results$ID_female_dads <- df_dads$ID[df_dads$sex != validation_results$most_frequent_sex_dad]
    validation_results$ID_child_female_dads <- ped$ID[ped$dadID %in% validation_results$ID_female_dads]
    remove(df_dads)
  }
  if (length(validation_results$all_sex_mom) > 1) {
    df_moms <- ped[ped$ID %in% ped$momID, ]
    validation_results$ID_male_moms <- df_moms$ID[df_moms$sex != validation_results$most_frequent_sex_mom]
    validation_results$ID_child_male_moms <- ped$ID[ped$momID %in% validation_results$ID_male_moms]
    remove(df_moms)
  }

  if (repair) {
    if (verbose) {
      cat("Step 2: Attempting to repair sex coding...\n")
    }
    # Initialize a list to track changes made during repair
    changes <- list()
    original_ped <- ped

    if (validation_results$sex_length == 2) {
      # Recode all dads to the most frequent male value
      ped <- recodeSex(ped, code_male = validation_results$most_frequent_sex_dad)
      # Count and record the change
      num_changes <- sum(original_ped$sex != ped$sex)
      # Record the change and the count
      changes[[length(changes) + 1]] <- sprintf(
        "Recode sex based on most frequent sex in dads: %s. Total sex changes made:  %d",
        validation_results$most_frequent_sex_dad, num_changes
      )
    }
    # Return the repaired pedigree dataframe

    if (verbose) {
      cat("Changes Made:\n")
      print(changes)
    }
    return(ped)
  } else {
    if (verbose) {
      cat("Checks Made:\n")
      print(validation_results)
    }

    return(validation_results)
  }
}


#' Repairs Sex Coding in a Pedigree Dataframe
#'
#' This function serves as a wrapper around `checkSex` to specifically handle
#' the repair of the sex coding in a pedigree dataframe.
#'
#' @inheritParams checkSex
#' @inheritParams plotPedigree
#' @inherit checkSex details
#' @return A modified version of the input data.frame \code{ped}, containing an additional or modified 'sex_recode' column where the 'sex' values are recoded according to \code{code_male}. NA values in the 'sex' column are preserved.
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
#' This function serves as is primarily used internally, by plotting functions etc.
#' It sets the `repair` flag to TRUE automatically and forwards any additional parameters to `checkSex`.
#'
#' @inheritParams checkSex
#' @inheritParams plotPedigree
#' @param code_na The current value used for missing values.
#' @param recode_na The value to use for missing values. Default is NA_character_
#' @param recode_male The value to use for males. Default is "M"
#' @param recode_female The value to use for females. Default is "F"
#' @inherit checkSex details
#' @return A modified version of the input data.frame \code{ped}, containing an additional or modified 'sex_recode' column where the 'sex' values are recoded according to \code{code_male}. NA values in the 'sex' column are preserved.
#' @export
#' @seealso \code{\link{plotPedigree}}
#' @export
recodeSex <- function(
    ped, verbose = FALSE, code_male = NULL, code_na = NULL, code_female = NULL,
    recode_male = "M", recode_female = "F", recode_na = NA_character_) {
  if (!is.null(code_na)) {
    ped$sex[ped$sex == code_na] <- NA
  }
  # Recode as "F" or "M" based on code_male, preserving NAs
  if (!is.null(code_male) && !is.null(code_female)) {
    # Initialize sex_recode as NA, preserving the length of the 'sex' column
    ped$sex_recode <- recode_na
    ped$sex_recode[ped$sex == code_female] <- recode_female
    ped$sex_recode[ped$sex == code_male] <- recode_male
    # Overwriting temp recode variable
    ped$sex <- ped$sex_recode
    ped$sex_recode <- NULL
  } else if (!is.null(code_male) && is.null(code_female)) {
    # Initialize sex_recode as NA, preserving the length of the 'sex' column
    ped$sex_recode <- recode_na
    ped$sex_recode[ped$sex != code_male & !is.na(ped$sex)] <- recode_female
    ped$sex_recode[ped$sex == code_male] <- recode_male
    # Overwriting temp recode variable
    ped$sex <- ped$sex_recode
    ped$sex_recode <- NULL
  } else if (is.null(code_male) && !is.null(code_female)) {
    # Initialize sex_recode as NA, preserving the length of the 'sex' column
    ped$sex_recode <- recode_na
    ped$sex_recode[ped$sex != code_female & !is.na(ped$sex)] <- recode_male
    ped$sex_recode[ped$sex == code_female] <- recode_female
    # Overwriting temp recode variable
    ped$sex <- ped$sex_recode
    ped$sex_recode <- NULL
  } else {
    if (verbose) {
      warning("Both code male and code female are empty. No recoding was done.")
    }
  }
  return(ped)
}
