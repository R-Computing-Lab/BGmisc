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
  if (verbose == TRUE) message("Combining Duplicate Name Columns...")

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

#' Detect GEDCOM Version from File Lines
#'
#' @param lines Character vector of lines from a GEDCOM file.
#' @return A string such as `"5.5.1"`, `"7.0"`, or `"unknown"`.
#' @keywords internal
detectGedcomVersion <- function(lines) {
  head_idx <- which(grepl("^0 HEAD\\b", lines))[1L]
  if (is.na(head_idx)) {
    return("unknown")
  }

  # End of HEAD is the next level-0 record
  if (head_idx >= length(lines)) {
    return("unknown")
  }
  next_l0 <- which(grepl("^0 ", lines[(head_idx + 1L):length(lines)]))[1L]
  head_end <- if (is.na(next_l0)) length(lines) else head_idx + next_l0 - 1L
  head_block <- lines[head_idx:head_end]

  gedc_idx <- which(grepl("^1 GEDC\\b", head_block))[1L]
  if (is.na(gedc_idx)) {
    return("unknown")
  }

  # Guard: if GEDC is the last line of HEAD, there is no VERS to look ahead to
  if (gedc_idx >= length(head_block)) {
    return("unknown")
  }

  # Look ahead within HEAD block for the VERS line under GEDC
  lookahead <- head_block[seq(gedc_idx + 1L, min(gedc_idx + 5L, length(head_block)))]
  vers_line <- lookahead[grepl("^2 VERS\\b", lookahead)][1L]
  if (is.na(vers_line)) {
    return("unknown")
  }

  val <- extractInfo(vers_line, "VERS")
  if (is.na(val) || !nzchar(val)) {
    return("unknown")
  }
  val
}

#' Convert GEDCOM Latitude String to Numeric
#'
#' Converts GEDCOM-style latitude strings like `"N51.5074"` or `"S33.8688"` to
#' signed decimal degrees. Returns `NA` for `NA` or unrecognised-prefix input.
#'
#' @param x Character vector of GEDCOM latitude values.
#' @return Numeric vector of decimal degrees (positive = N, negative = S).
#' @examples
#' BGmisc:::gedcomLatToNumeric(c("N51.5074", "S33.8688", NA))
#' @keywords internal
gedcomLatToNumeric <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x) & (startsWith(x, "N") | startsWith(x, "S"))
  out[ok] <- as.numeric(substring(x[ok], 2)) * ifelse(startsWith(x[ok], "N"), 1, -1)
  out
}

#' Convert GEDCOM Longitude String to Numeric
#'
#' Converts GEDCOM-style longitude strings like `"E151.2093"` or `"W0.1278"` to
#' signed decimal degrees. Returns `NA` for `NA` or unrecognised-prefix input.
#'
#' @param x Character vector of GEDCOM longitude values.
#' @return Numeric vector of decimal degrees (positive = E, negative = W).
#' @examples
#' BGmisc:::gedcomLonToNumeric(c("E151.2093", "W0.1278", NA))
#' @keywords internal
gedcomLonToNumeric <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x) & (startsWith(x, "E") | startsWith(x, "W"))
  out[ok] <- as.numeric(substring(x[ok], 2)) * ifelse(startsWith(x[ok], "E"), 1, -1)
  out
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
