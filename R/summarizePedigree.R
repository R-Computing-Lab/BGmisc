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
#' @param skipvar A character vector of variables to skip when calculating summary statistics.
#' @returns A list containing summary statistics for family, maternal, and paternal lines, as well as the 5 oldest and biggest lines.
#' @import data.table
#' @export
summarizePedigrees <- function(ped, famID = "famID", personID = "ID",
                              momID = "momID", dadID = "dadID",
                              matID = "matID", patID = "patID",
                              byr = NULL, type = c("fathers", "mothers", "families"),
                               nbiggest = 5, noldest = 5, skipvar= NULL) {
  # Check that the ID variables are not the same
  if(personID %in% c(famID, momID, dadID, matID, patID)) {
    stop("personID cannot be the same as any of the other ID variables.")
  }
  if(!all(c(personID, momID, dadID) %in% names(ped))) {
    stop("personID, momID, and dadID must be columns in the pedigree data.")
  }
  if(!is.null(byr) && !byr %in% names(ped)) {
    stop("byr must be a column in the pedigree data.")
  }
  # Check that the skipvar exists
if(is.null(skipvar)){
    skip_var <- c(personID, famID, momID, dadID, matID, patID)
} else {
    skip_var <- c(skipvar, personID , famID, momID, dadID, matID, patID)
}
  # Build the pedigree using the provided functions
  if(!famID %in% names(ped) & "families" %in% type ) {
    ped <- ped2fam(ped, personID = personID, momID = momID, dadID = dadID, famID = famID)
  }
  if(!matID %in% names(ped) & "mothers" %in% type ) {
    ped <- ped2maternal(ped, personID = personID, momID = momID, dadID = dadID, matID = matID)
  }
  if(!patID %in% names(ped) & "fathers" %in% type) {
    ped <- ped2paternal(ped, personID = personID, momID = momID, dadID = dadID, patID = patID)
  }
  # Convert to data.table
  ped_dt <- data.table::as.data.table(ped)
  # Function to calculate summary statistics for all numeric variables

  # Identify numeric columns excluding the group_var and skip_var



  calculate_summary_dt <- function(data, group_var, skip_var) {
    numeric_cols <- setdiff(names(data)[sapply(data, is.numeric)], c(group_var, skip_var))
    summary_stats <- data[, {
      count <- .N  # Calculate count once per group
      stats_list <- lapply(numeric_cols, function(colname) {
        x <- .SD[[colname]]
        stats <- list(
       #   count = .N,
          mean = as.double(base::mean(x, na.rm = TRUE)),
          median = as.double(stats::median(x, na.rm = TRUE)),
          min = as.double(base::min(x, na.rm = TRUE)),
          max = as.double(base::max(x, na.rm = TRUE)),
          sd = as.double(stats::sd(x, na.rm = TRUE))
        )
        names(stats) <- paste0(colname, "_", names(stats))
        stats
      })
      stats <- unlist(stats_list, recursive = FALSE)
      c(list(count = count), stats)
    }, by = group_var, .SDcols = numeric_cols]

    # Flatten the nested lists
    summary_stats <- data.table::as.data.table(summary_stats[, lapply(.SD, unlist), by = group_var])

    return(summary_stats)

  }

  # Function to find the originating member for each line
  find_originating_member <- function(data, group_var,sort_var = personID) {
    data[order(get(sort_var)), .SD[1], by = group_var]
  }

  # Initialize output list
  output <- list()

  # Calculate summary statistics for families, maternal lines, and paternal lines



  if("families" %in% type) {
    family_summary_dt <- calculate_summary_dt(ped_dt, famID, skip_var = skip_var)
    # Find the originating member for each line
    originating_member_family <- find_originating_member(ped_dt, famID)
    # Merge summary statistics with originating members for additional information
    family_summary_dt <- merge(family_summary_dt, originating_member_family,
                               by = famID, suffixes = c("", "_founder"))
    output$family_summary <- family_summary_dt
  }
  if("mothers" %in% type) {
    maternal_summary_dt <- calculate_summary_dt(ped_dt, matID, skip_var = skip_var)
    originating_member_maternal <- find_originating_member(ped_dt, matID)
    maternal_summary_dt <- merge(maternal_summary_dt, originating_member_maternal, by = matID, suffixes = c("", "_founder"))
    output$maternal_summary <- maternal_summary_dt
  }
  if("fathers" %in% type) {
    paternal_summary_dt <- calculate_summary_dt(ped_dt, patID, skip_var = skip_var)
    originating_member_paternal <- find_originating_member(ped_dt, patID)
    paternal_summary_dt <- merge(paternal_summary_dt, originating_member_paternal, by = patID, suffixes = c("", "_founder"))
    output$paternal_summary <- paternal_summary_dt
  }

  # Optionally find the superlative lines
  # & noldest <= unique(ped_dt[[famID]])
  ## oldest
  if(!is.null(byr) && noldest > 0) {
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
                                nbiggest = 5, noldest = 5,skipvar = NULL) {
  # Call to wrapper function
  summarizePedigrees(ped = ped,
                    personID = personID,
                    nbiggest = nbiggest,
                    noldest = noldest,
                    byr=byr,
                    momID = momID, dadID = dadID,
                    famID = famID, matID=matID, patID=patID,skipvar = skipvar,
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
                                nbiggest = 5, noldest = 5,skipvar = NULL) {
  # Call to wrapper function
  summarizePedigrees(ped = ped,
                    personID = personID,
                    nbiggest = nbiggest,
                    noldest = noldest,
                    byr=byr,
                    momID = momID, dadID = dadID,
                    famID = famID, matID=matID, patID = patID,
                    type = "fathers",skipvar = skipvar)
}

#' Summarize the families in a pedigree
#' @inheritParams summarizePedigrees
#' @seealso [summarizePedigrees ()]
#' @export

summarizeFamilies <- function(ped, famID = "famID", personID = "ID",
                                momID = "momID", dadID = "dadID",
                                matID = "matID", patID = "patID",
                                byr = NULL,
                                nbiggest = 5, noldest = 5,skipvar = NULL) {
  # Call to wrapper function
  summarizePedigrees(ped = ped,
                    personID = personID,
                    nbiggest = nbiggest,
                    noldest = noldest,
                    byr=byr,
                    momID = momID, dadID = dadID,
                    famID = famID, matID=matID, patID = patID,
                    type = "families",skipvar = skipvar)
}
