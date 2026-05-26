#' Initialize an Empty Individual Record
#'
#' @description Creates a named list with all GEDCOM initialized to NA_character_.
#'
#' @param all_var_names A character vector of variable names.
#' @return A named list representing an empty individual record.
#' @importFrom stats setNames
initializeRecord <- function(all_var_names) {
  stats::setNames(as.list(rep(NA_character_, length(all_var_names))), all_var_names)
}


#' collapse Names
#'
#' This function combines the `name_given` and `name_given_pieces` columns in a data frame. If both columns have non-missing values that differ, a warning is issued and the original `name_given` is retained. If one column is missing, the other is used. The same logic applies to the `name_surn` and `name_surn_pieces` columns.
#'
#' @inheritParams readGedcom
#' @param df_temp A data frame containing the columns to be combined.
#' @return A data frame with the combined columns.
collapseNames <- function(verbose, df_temp) {
  if (verbose == TRUE) message("Combining Duplicate Columns")

  if (!all(is.na(df_temp$name_given_pieces)) || !all(is.na(df_temp$name_given))) {
    result <- combineColumns(df_temp$name_given, df_temp$name_given_pieces)
    df_temp$name_given <- result$combined
    if (!result$retain_col2) df_temp$name_given_pieces <- NULL
  }

  if (!all(is.na(df_temp$name_surn_pieces)) || !all(is.na(df_temp$name_surn))) {
    result <- combineColumns(df_temp$name_surn, df_temp$name_surn_pieces)
    df_temp$name_surn <- result$combined
    if (!result$retain_col2) df_temp$name_surn_pieces <- NULL
  }
  df_temp
}

#' Combine Columns
#'
#' This function combines two columns, handling conflicts and merging non-conflicting data.
#' @param col1 The first column to combine.
#' @param col2 The second column to combine.
#' @return A list with the combined column and a flag indicating if the second column should be retained.
#' @keywords internal
# Helper function to check for conflicts and merge columns
combineColumns <- function(col1, col2) {
  col1_lower <- stringr::str_to_lower(col1)
  col2_lower <- stringr::str_to_lower(col2)
  conflicts <- !is.na(col1_lower) & !is.na(col2_lower) & col1_lower != col2_lower
  if (any(conflicts)) {
    warning("Columns have conflicting values. They were not merged.")
    list(combined = col1, retain_col2 = TRUE)
  } else {
    combined <- ifelse(is.na(col1), col2, col1)
    list(combined = combined, retain_col2 = FALSE)
  }
}
