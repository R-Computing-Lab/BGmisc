#' Summarize Pedigree Data
#'
#' This function summarizes pedigree data, including calculating summary statistics for all numeric variables,
#' and finding the originating member for each family, maternal, and paternal line.
#'
#' @param pedigree_data A data frame containing the pedigree data.
#' @param personID A character string indicating the column name for the person ID variable.
#' @param momID A character string indicating the column name for the mother ID variable.
#' @param dadID A character string indicating the column name for the father ID variable.
#' @returns A list containing summary statistics for family, maternal, and paternal lines, as well as the 5 oldest and biggest lines.
#' @import data.table
#' @export
summarizePedigree <- function(ped, famID = "famID", personID = "ID",
                              momID = "momID", dadID = "dadID",
                              matID = "matID", patID = "patID",
                              yrb = NULL,
                               nbiggest = 5, noldest = 5) {

  # checks
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required for this function. Please install it and try again.")
  }
  if(personID %in% c(famID, momID, dadID, matID)) {
    stop("personID cannot be the same as any of the other ID variables.")
  }
  if(!all(c(personID, momID, dadID) %in% names(ped))) {
    stop("personID, momID, and dadID must be columns in the pedigree data.")
  }
  if(!is.null(yrb) && !yrb %in% names(ped)) {
    stop("yrb must be a column in the pedigree data.")
  }
  # Convert to data.table
  ped_dt <- data.table::as.data.table(ped)

  # Build the pedigree using the provided functions
  if(!famID %in% names(ped_dt)) {
    ped_dt <- BGmisc::ped2fam(ped_dt, personID = personID, momID = momID, dadID = dadID, famID = famID)
  }
  if(!matID %in% names(ped_dt)) {
    ped_dt <- BGmisc::ped2maternal(ped_dt, personID = personID, momID = momID, dadID = dadID, matID = matID)
  }
  if(!patID %in% names(ped_dt)) {
    ped_dt <- BGmisc::ped2paternal(ped_dt, personID = personID, momID = momID, dadID = dadID, patID = patID)
  }

  # Function to calculate summary statistics for all numeric variables
  calculate_summary_dt <- function(data, group_var) {
    numeric_cols <- sapply(data, is.numeric)
    summary_stats <- data[, lapply(.SD, function(x) {
      list(
        count = .N,
        mean = mean(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE)
      )
    }), by = group_var, .SDcols = numeric_cols]

    # Flatten the nested lists
    summary_stats <- summary_stats[, unlist(.SD, recursive = FALSE), by = group_var]
    return(summary_stats)
  }

  # Function to find the originating member for each line
  find_originating_member <- function(data, group_var) {
    data[order(byr), .SD[1], by = group_var]
  }

  # Calculate summary statistics for families, maternal lines, and paternal lines
  family_summary_dt <- calculate_summary_dt(ped_dt, famID)
  maternal_summary_dt <- calculate_summary_dt(ped_dt, matID)
  paternal_summary_dt <- calculate_summary_dt(ped_dt, patID)

  # Find the originating member for each line
  originating_member_family <- find_originating_member(ped_dt, famID)
  originating_member_maternal <- find_originating_member(ped_dt, matID)
  originating_member_paternal <- find_originating_member(ped_dt, patID)

  # Merge summary statistics with originating members for additional information
  family_summary_dt <- merge(family_summary_dt, originating_member_family, by = famID, suffixes = c("", "_founder"))
  maternal_summary_dt <- merge(maternal_summary_dt, originating_member_maternal, by = matID, suffixes = c("", "_founder"))
  paternal_summary_dt <- merge(paternal_summary_dt, originating_member_paternal, by = patID, suffixes = c("", "_founder"))

  if(!is.null(byr)){
  # Identify the 5 oldest lines
  oldest_families <- family_summary_dt[order(byr)][1:noldest]
  oldest_maternal <- maternal_summary_dt[order(byr)][1:noldest]
  oldest_paternal <- paternal_summary_dt[order(byr)][1:noldest]
  }
  # Identify the 5 biggest lines
  biggest_families <- family_summary_dt[order(-count)][1:nbiggest]
  biggest_maternal <- maternal_summary_dt[order(-count)][1:nbiggest]
  biggest_paternal <- paternal_summary_dt[order(-count)][1:nbiggest]

  # Output the results as a list
  if(!is.null(byr)){
    output <- list(
      family_summary = family_summary_dt,
      maternal_summary = maternal_summary_dt,
      paternal_summary = paternal_summary_dt,
      oldest_families = oldest_families,
      oldest_maternal = oldest_maternal,
      oldest_paternal = oldest_paternal,
      biggest_families = biggest_families,
      biggest_maternal = biggest_maternal,
      biggest_paternal = biggest_paternal
    )
  } else {
  output <- list(
    family_summary = family_summary_dt,
    maternal_summary = maternal_summary_dt,
    paternal_summary = paternal_summary_dt,
    biggest_families = biggest_families,
    biggest_maternal = biggest_maternal,
    biggest_paternal = biggest_paternal
  )
  }
  return(output)
}

