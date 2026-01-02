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
#' We recognize the importance of using language and methodologies that affirm and respect the full spectrum of gender identities.
#' The developers of this package express unequivocal support for folx in the transgender
#' and LGBTQ+ communities.
#'
#' @param ped A dataframe representing the pedigree data with a 'sex' column.
#' @param code_male The current code used to represent males in the 'sex' column.
#' @param code_female The current code used to represent females in the 'sex' column. If both are NULL, no recoding is performed.
#' @param code_unknown The current code used to represent unknown
#' @param verbose A logical flag indicating whether to print progress and validation messages to the console.
#' @param repair A logical flag indicating whether to attempt repairs on the sex coding.
#' @param momID The column name for maternal IDs. Default is "momID".
#' @param dadID The column name for paternal IDs. Default is "dadID".
#'
#' @return Depending on the value of `repair`, either a list containing validation results or a repaired dataframe is returned.
#' @examples
#' \dontrun{
#' ped <- data.frame(ID = c(1, 2, 3), sex = c("M", "F", "M"))
#' checkSex(ped, code_male = "M", verbose = TRUE, repair = FALSE)
#' }
#' @export
#'
checkSex <- function(ped, code_male = NULL,
                     code_female = NULL,
                     code_unknown = NULL,
                     verbose = FALSE, repair = FALSE,
                     momID = "momID",
                     dadID = "dadID") {
  # Standardize column names in the input dataframe
  ped <- standardizeColnames(ped, verbose = verbose)

  # TO DO: bypass the rest of the function if recode_only is TRUE

  # Initialize a list to store validation results
  validation_results <- list()


  if (verbose == TRUE) {
    message("Step 1: Checking how many sexes/genders...\n")
  }

  # Check unique values in 'sex'
  validation_results$sex_unique <- unique(ped$sex)
  validation_results$sex_length <- length(unique(ped$sex))
  if (verbose == TRUE) {
    message(validation_results$sex_length, " unique sex codes found: ", paste(validation_results$sex_unique, collapse = ", "), "\n")
  }


  # Are there multiple sexes/genders in the list of dads and moms?

  dad_results <- checkParentSex(ped, parent_col = dadID, verbose = verbose)
  mom_results <- checkParentSex(ped, parent_col = momID, verbose = verbose)

  validation_results$all_sex_dad <- dad_results$unique_sexes
  validation_results$all_sex_mom <- mom_results$unique_sexes
  validation_results$most_frequent_sex_dad <- dad_results$modal_sex
  validation_results$most_frequent_sex_mom <- mom_results$modal_sex
  validation_results$ID_female_dads <- dad_results$inconsistent_parents
  validation_results$ID_child_female_dads <- dad_results$inconsistent_children
  validation_results$ID_male_moms <- mom_results$inconsistent_parents
  validation_results$ID_child_male_moms <- mom_results$inconsistent_children

  if (repair == FALSE) {
    if (verbose == TRUE) {
      cat("Checks Made:\n")
      message(validation_results)
    }
    return(validation_results)
  } else {
    if (verbose == TRUE) {
      message("Step 2: Attempting to repair sex coding...\n")
    }
    # Initialize a list to track changes made during repair
    changes <- list()
    original_ped <- ped

    if (validation_results$sex_length == 2) {
      # Recode all dads to the most frequent male value
      ped <- recodeSex(ped,
        code_male = validation_results$most_frequent_sex_dad,
        code_female = validation_results$most_frequent_sex_mom,
        code_unknown = code_unknown
      )
      # Count and record the change
      num_changes <- sum(original_ped$sex != ped$sex)
      # Record the change and the count
      changes[[length(changes) + 1]] <- sprintf(
        "Recode sex based on most frequent sex in dads: %s. Total sex changes made:  %d",
        validation_results$most_frequent_sex_dad, num_changes
      )
    }
    # Return the repaired pedigree dataframe

    if (verbose == TRUE) {
      cat("Changes Made:\n")
      message(changes)
    }
    return(ped)
  }
}


#' Repairs Sex Coding in a Pedigree Dataframe
#'
#' This function serves as a wrapper around `checkSex` to specifically handle
#' the repair of the sex coding in a pedigree dataframe.
#'
#' @inheritParams checkSex
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
repairSex <- function(ped, verbose = FALSE,
                      code_male = NULL,
                      code_female = NULL,
                      code_unknown = NULL) {
  checkSex(
    ped = ped, verbose = verbose, repair = TRUE,
    code_male = code_male,
    code_female = code_female,
    code_unknown = code_unknown
  )
}

#' Recodes Sex Variable in a Pedigree Dataframe
#'
#' This function serves as is primarily used internally, by plotting functions etc.
#' It sets the `repair` flag to TRUE automatically and forwards any additional parameters to `checkSex`.
#'
#' @inheritParams checkSex
#' @param code_na The current value used for missing values.
#' @param recode_na The value to use for missing values. Default is NA_character_
#' @param recode_male The value to use for males. Default is "M"
#' @param recode_female The value to use for females. Default is "F"
#' @param recode_unknown The value to use for unknown values. Default is "U"
#' @inherit checkSex details
#' @return A modified version of the input data.frame \code{ped}, containing an additional or modified 'sex_recode' column where the 'sex' values are recoded according to \code{code_male}. NA values in the 'sex' column are preserved.
#' @export

recodeSex <- function(
  ped, verbose = FALSE, code_male = NULL, code_na = NULL, code_female = NULL,
  code_unknown = NULL,
  recode_male = "M",
  recode_female = "F",
  recode_unknown = "U",
  recode_na = NA_character_
) {
  if (is.null(code_male) && is.null(code_female)) {
    if (verbose == TRUE) {
      warning("Both code male and code female are empty. No recoding was done.")
    }
    return(ped)
  }
  # First, set any code_na values to NA
  if (!is.null(code_na)) {
    ped$sex[ped$sex == code_na] <- NA
  }

  # Initialize sex_recode as NA, preserving the length of the 'sex' column
  ped$sex_recode <- recode_na


  if (!is.null(code_male)) {
    ped$sex_recode[ped$sex == code_male] <- recode_male
  }
  if (!is.null(code_female)) {
    ped$sex_recode[ped$sex == code_female] <- recode_female
  }

  # handle unknown codes
  if (!is.null(code_unknown) && !is.na(code_unknown)) {
    ped$sex_recode[ped$sex == code_unknown] <- recode_unknown
  } else if (!is.null(code_unknown) && is.na(code_unknown)) {
    ped$sex_recode[is.na(ped$sex)] <- recode_unknown
  } else if (!is.null(code_male) && !is.null(code_female)) {
    ped$sex_recode[!ped$sex %in% c(code_male, code_female) & !is.na(ped$sex)] <- recode_unknown
  }


  # Handle cases where only one of code
  # just male
  if (!is.null(code_male) && is.null(code_female)) {
    if (!is.null(code_unknown)) {
      ped$sex_recode[ped$sex != code_male & !is.na(ped$sex) & ped$sex != code_unknown] <- recode_female
    } else if (is.null(code_unknown)) {
      ped$sex_recode[ped$sex != code_male & !is.na(ped$sex)] <- recode_female
    }
  }
  # just female
  if (is.null(code_male) && !is.null(code_female)) {
    if (!is.null(code_unknown)) {
      ped$sex_recode[ped$sex != code_female & !is.na(ped$sex) & ped$sex != code_unknown] <- recode_male
    } else if (is.null(code_unknown)) {
      ped$sex_recode[ped$sex != code_female & !is.na(ped$sex)] <- recode_male
    }
  }

  # Overwriting temp recode variable
  ped$sex <- ped$sex_recode
  ped$sex_recode <- NULL
  return(ped)
}


#' Check Parental Role Sex Consistency
#'
#' Validates sex coding consistency for a given parental role (momID or dadID).
#'
#' @param ped Pedigree dataframe.
#' @param parent_col The column name for parent IDs ("momID" or "dadID").
#' @param sex_col The column name for sex coding. Default is "sex".
#' @param verbose Logical, whether to print messages.
#'
#'
#' @return A list containing role, unique sex codes, modal sex, inconsistent parents, and linked children.
checkParentSex <- function(ped, parent_col, sex_col = "sex", verbose = FALSE) {
  parent_ids <- ped[[parent_col]]
  parent_rows <- ped[ped$ID %in% parent_ids, ]

  if (nrow(parent_rows) == 0) {
    if (verbose == TRUE) cat(paste0("No individuals found in role: ", parent_col, "\n"))
    return(list(
      role = parent_col,
      unique_sexes = NULL,
      modal_sex = NA,
      all_same_sex = NA,
      inconsistent_parents = NULL,
      inconsistent_children = NULL
    ))
  }


  # Are there multiple sexes/genders in the list of dads and moms?
  parent_sexes <- parent_rows[[sex_col]]
  unique_sexes <- unique(parent_sexes)

  # are all moms/dads the same sex?
  all_same_sex <- length(unique_sexes) == 1

  # Store the most frequent sex for moms and dads
  modal_sex <- names(sort(table(parent_sexes), decreasing = TRUE))[1]

  if (all(is.na(modal_sex)) && verbose == TRUE) {
    cat(paste0("All parents in role ", parent_col, " have missing sex values.\n"))
  }

  # Type coercion based on ped$sex type
  if (is.numeric(ped[[sex_col]])) {
    modal_sex <- as.numeric(modal_sex)
  } else if (is.character(ped[[sex_col]])) {
    modal_sex <- as.character(modal_sex)
  }

  # List ids for dads that are female, moms that are male
  inconsistent_parents <- parent_rows$ID[parent_rows[[sex_col]] != modal_sex]

  child_col <- parent_col
  inconsistent_children <- ped$ID[ped[[child_col]] %in% inconsistent_parents]


  if (verbose == TRUE) {
    cat(paste0("Role: ", parent_col, "\n"))
    cat(length(unique_sexes), " unique sex codes found: ", paste(unique_sexes, collapse = ", "), "\n")
    cat("Modal sex code: ", modal_sex, "\n")

    if (all_same_sex) {
      cat("All parents consistently coded.\n")
    } else {
      cat(length(inconsistent_parents), " parents have inconsistent sex coding.\n")
    }
  }

  return(list(
    role = parent_col,
    unique_sexes = unique_sexes,
    modal_sex = modal_sex,
    all_same_sex = all_same_sex,
    inconsistent_parents = inconsistent_parents,
    inconsistent_children = inconsistent_children
  ))
}

#' Get the Modal Value of a Vector
#'
#' This function calculates the modal value of a vector, which is the most frequently occurring value.
#' If the vector is empty or contains only NA values, it returns NA.
#'
#' @param x A vector of values.
#'
#' @return The modal value of the vector. If the vector is empty or contains only NA values, returns NA.
#' @keywords internal
#' @importFrom stats na.omit
.getModalValue <- function(x) {
  if (length(stats::na.omit(x)) == 0) {
    return(NA)
  }
  freq_table <- sort(table(x), decreasing = TRUE)
  modal <- names(freq_table)[1]
  return(modal)
}
