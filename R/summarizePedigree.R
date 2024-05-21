#' Summarize Pedigree Data
#'
#' This function summarizes pedigree data, including calculating summary statistics for all numeric variables,
#' and finding the originating member for each family, maternal, and paternal line.
#' @inheritParams ped2fam
#' @inheritParams ped2maternal
#' @inheritParams ped2paternal
#' @param nbiggest The number of biggest lines to return.
#' @param noldest The number of oldest lines to return.
#' @param byr The column name for birth year.
#' @param type The type of summary statistics to calculate. Options are "fathers", "mothers", and "families".
#' @returns A list containing summary statistics for family, maternal, and paternal lines, as well as the 5 oldest and biggest lines.
#' @import data.table
#' @export
summarizePedigrees <- function(ped, famID = "famID", personID = "ID",
                              momID = "momID", dadID = "dadID",
                              matID = "matID", patID = "patID",
                              byr = NULL, type = c("fathers", "mothers", "families"),
                               nbiggest = 5, noldest = 5) {

  if(personID %in% c(famID, momID, dadID, matID)) {
    stop("personID cannot be the same as any of the other ID variables.")
  }
  if(!all(c(personID, momID, dadID) %in% names(ped))) {
    stop("personID, momID, and dadID must be columns in the pedigree data.")
  }
  if(!is.null(byr) && !byr %in% names(ped)) {
    stop("byr must be a column in the pedigree data.")
  }
  # Convert to data.table
  ped_dt <- data.table::as.data.table(ped)

  # Build the pedigree using the provided functions
  if(!famID %in% names(ped_dt) & "families" %in% type ) {
    ped_dt <- ped2fam(ped_dt, personID = personID, momID = momID, dadID = dadID, famID = famID)
  }
  if(!matID %in% names(ped_dt) & "mothers" %in% type ) {
    ped_dt <- ped2maternal(ped_dt, personID = personID, momID = momID, dadID = dadID, matID = matID)
  }
  if(!patID %in% names(ped_dt) & "fathers" %in% type) {
    ped_dt <- ped2paternal(ped_dt, personID = personID, momID = momID, dadID = dadID, patID = patID)
  }

  # Function to calculate summary statistics for all numeric variables
  calculate_summary_dt <- function(data, group_var) {
    numeric_cols <- sapply(data, is.numeric)
    summary_stats <- data[, lapply(.SD, function(x) {
      list(
        count = .N,
        mean = base::mean(x, na.rm = TRUE),
        median = stats::median(x, na.rm = TRUE),
        min = base::min(x, na.rm = TRUE),
        max = base::max(x, na.rm = TRUE),
        sd = stats::sd(x, na.rm = TRUE)
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

  # Initialize output list
  output <- list()

  # Calculate summary statistics for families, maternal lines, and paternal lines



  if("families" %in% type) {
    family_summary_dt <- calculate_summary_dt(ped_dt, famID)
    # Find the originating member for each line
    originating_member_family <- find_originating_member(ped_dt, famID)
    # Merge summary statistics with originating members for additional information
    family_summary_dt <- merge(family_summary_dt, originating_member_family,
                               by = famID, suffixes = c("", "_founder"))
    output$family_summary <- family_summary_dt
  }
  if("mothers" %in% type) {
    maternal_summary_dt <- calculate_summary_dt(ped_dt, matID)
    originating_member_maternal <- find_originating_member(ped_dt, matID)
    maternal_summary_dt <- merge(maternal_summary_dt, originating_member_maternal, by = matID, suffixes = c("", "_founder"))
    output$maternal_summary <- maternal_summary_dt
  }
  if("fathers" %in% type) {
    paternal_summary_dt <- calculate_summary_dt(ped_dt, patID)
    originating_member_paternal <- find_originating_member(ped_dt, patID)
    paternal_summary_dt <- merge(paternal_summary_dt, originating_member_paternal, by = patID, suffixes = c("", "_founder"))
    output$paternal_summary <- paternal_summary_dt
  }

  # Optionally find the superlative lines

  ## oldest
  if(!is.null(byr) && noldest > 0 & noldest <= nrow(ped_dt)) {
    if("families" %in% type) {
      output$oldest_families <- family_summary_dt[order(get(byr))][1:noldest]
    }
    if("mothers" %in% type) {
      output$oldest_maternal <- maternal_summary_dt[order(get(byr))][1:noldest]
    }
    if("fathers" %in% type) {
      output$oldest_paternal <- paternal_summary_dt[order(get(byr))][1:noldest]
    }
  }

  # biggest lines
  if(!is.null(nbiggest) & nbiggest > 0 & nbiggest <= nrow(ped_dt)) {
    if("families" %in% type) {
      output$biggest_families <- family_summary_dt[order(-get("count"))][1:nbiggest]
    }
    if("mothers" %in% type) {
      output$biggest_maternal <- maternal_summary_dt[order(-get("count"))][1:nbiggest]
    }
    if("fathers" %in% type) {
      output$biggest_paternal <- paternal_summary_dt[order(-get("count"))][1:nbiggest]
    }
  }

  return(output)
}

#' Summarize the maternal lines in a pedigree
#' @inheritParams summarizePedigrees
#' @seealso [summarizePedigrees ()]
#' @export
#'
summarizeMatrilines <- function(ped, famID = "famID", personID = "ID",
                                momID = "momID", dadID = "dadID",
                                matID = "matID", patID = "patID",
                                byr = NULL,
                                nbiggest = 5, noldest = 5) {
  # Call to wrapper function
  summarizePedigrees(ped = ped,
                    personID = personID,
                    nbiggest = nbiggest,
                    noldest = noldest,
                    byr=byr,
                    momID = momID, dadID = dadID,
                    famID = famID, matID=matID, patID=patID,
                    type = "mothers")
}

#' Summarize the paternal lines in a pedigree
#' @inheritParams summarizePedigrees
#' @seealso [summarizePedigrees ()]
#' @export
#'
summarizePatrilines <- function(ped, famID = "famID", personID = "ID",
                                momID = "momID", dadID = "dadID",
                                matID = "matID", patID = "patID",
                                byr = NULL,
                                nbiggest = 5, noldest = 5) {
  # Call to wrapper function
  summarizePedigrees(ped = ped,
                    personID = personID,
                    nbiggest = nbiggest,
                    noldest = noldest,
                    byr=byr,
                    momID = momID, dadID = dadID,
                    famID = famID, matID=matID, patID = patID,
                    type = "fathers")
}

#' Summarize the families in a pedigree
#' @inheritParams summarizePedigrees
#' @seealso [summarizePedigrees ()]
#' @export

summarizeFamilies <- function(ped, famID = "famID", personID = "ID",
                                momID = "momID", dadID = "dadID",
                                matID = "matID", patID = "patID",
                                byr = NULL,
                                nbiggest = 5, noldest = 5) {
  # Call to wrapper function
  summarizePedigrees(ped = ped,
                    personID = personID,
                    nbiggest = nbiggest,
                    noldest = noldest,
                    byr=byr,
                    momID = momID, dadID = dadID,
                    famID = famID, matID=matID, patID = patID,
                    type = "families")
}
