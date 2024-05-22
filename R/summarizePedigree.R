#' Summarize Pedigree Data
#'
#' This function summarizes pedigree data, including calculating summary statistics for all numeric variables,
#' and finding the originating member for each family, maternal, and paternal line.
#' @inheritParams ped2fam
#' @inheritParams ped2maternal
#' @inheritParams ped2paternal
#' @param nbiggest The number of biggest lines to return.
#' @param noldest The number of oldest lines to return.
#' @param byr Optional column name for birth year.
#' @param type The type of summary statistics to calculate. Options are "fathers", "mothers", and "families".
#' @param skip_var A character vector of variables to skip when calculating summary statistics.
#' @param five_num_summary Logical, if TRUE, include the 5-number summary (min, Q1, median, Q3, max) in the summary statistics.
#' @param founder_sort_var The variable to sort the founders by. If NULL, the founders will be sorted by birth year (`byr`) if that's present and by `personID` otherwise.

#' @returns A data.frame (or list) containing summary statistics for family, maternal, and paternal lines, as well as the 5 oldest and biggest lines.
#' @import data.table
#' @export
summarizePedigrees <- function(ped, famID = "famID", personID = "ID",
                              momID = "momID", dadID = "dadID",
                              matID = "matID", patID = "patID",
                              type = c("fathers", "mothers", "families"), byr = NULL, founder_sort_var = NULL,
                              nbiggest = 5, noldest = 5, skip_var = NULL, five_num_summary = FALSE, verbose=FALSE) {

  # Fast Fails

  ## Check that the ID variables are not the same
  if(personID %in% c(famID, momID, dadID, matID, patID)) {
    stop("personID cannot be the same as any of the other ID variables.")
  }
  ## Check that neccesarry variables are present
  if(!all(c(personID, momID, dadID) %in% names(ped))) {
    stop("personID, momID, and dadID must be columns in the pedigree data.")
  }
  if(is.null(type)) {
    stop("Type must be specified. Options are 'fathers', 'mothers', and 'families'.")
  }
  if(!is.null(byr) && !byr %in% names(ped)) {
    stop("If byr is specified, byr must be a column in the pedigree data. If you do not want to sort by birth year, set byr = NULL.")
  }
  if(!is.null(founder_sort_var)  && !founder_sort_var %in% names(ped)) {
    stop("If you set founder_sort_var, that variable must be a column in the pedigree data. If you want to sort by using the default, set founder_sort_var = NULL. The default is to sort by birth year if that's present and by personID otherwise.")
  }

  # user inputs
  ## what variables to skip
  if(is.null(skip_var)){
    skip_var <- c(personID, famID, momID, dadID, matID, patID)
  } else {
    skip_var <- c(skip_var, personID , famID, momID, dadID, matID, patID)
  }
  ## How to sort/identify the founders
  ## If founder_sort_var is NULL, sort by byr if it's present, otherwise sort by personID
  if(is.null(founder_sort_var) & is.null(byr)) {
    founder_sort_var <- personID
  } else if(is.null(founder_sort_var) & !is.null(byr)) {
    founder_sort_var <- byr
  }


  # Build the pedigree using the provided functions
  if(!famID %in% names(ped) & "families" %in% type ) {
    if(verbose) message("Counting families...")
    ped <- ped2fam(ped, personID = personID, momID = momID, dadID = dadID, famID = famID)
  }
  if(!matID %in% names(ped) & "mothers" %in% type ) {
    if(verbose) message("Counting mothers...")
    ped <- ped2maternal(ped, personID = personID, momID = momID, dadID = dadID, matID = matID)
  }
  if(!patID %in% names(ped) & "fathers" %in% type) {
    if(verbose) message("Counting fathers...")
    ped <- ped2paternal(ped, personID = personID, momID = momID, dadID = dadID, patID = patID)
  }

  # Convert to data.table
  ped_dt <- data.table::as.data.table(ped)

  # Function to calculate summary statistics for all numeric variables
  calculate_summary_dt <- function(data, group_var, skip_var, five_num_summary= FALSE) {
    # Identify numeric columns excluding the group_var and skip_var
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
        if (five_num_summary) {
          stats <- c(stats, list(
            Q1 = as.double(stats::quantile(x, 0.25, na.rm = TRUE)),
            Q3 = as.double(stats::quantile(x, 0.75, na.rm = TRUE))
          ))
        }
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
  find_originating_member <- function(data, group_var, sort_var = personID) {
    data[order(get(sort_var)), .SD[1], by = group_var]
  }

  # Initialize...
  ## Output list
  output <- list()
  ## Size of families
  n_fathers <- n_mothers <- n_families <- NULL
  # Calculate summary statistics for families, maternal lines, and paternal lines

  if("families" %in% type) {
    if(verbose) message("Summarizing families...")
    family_summary_dt <- calculate_summary_dt(ped_dt, famID, skip_var = skip_var, five_num_summary = five_num_summary)
    # Find the originating member for each line
    originating_member_family <- find_originating_member(ped_dt, group_var = famID, sort_var = founder_sort_var )
    # Merge summary statistics with originating members for additional information
    family_summary_dt <- merge(family_summary_dt, originating_member_family,
                               by = famID, suffixes = c("", "_founder"))
    output$family_summary <- family_summary_dt
    n_families <- nrow(family_summary_dt)
  }

  if("mothers" %in% type) {
    if(verbose) message("Summarizing maternal lines...")
    maternal_summary_dt <- calculate_summary_dt(ped_dt, matID, skip_var = skip_var, five_num_summary = five_num_summary)
    originating_member_maternal <- find_originating_member(ped_dt, group_var = matID, sort_var = founder_sort_var)
    maternal_summary_dt <- merge(maternal_summary_dt, originating_member_maternal, by = matID, suffixes = c("", "_founder"))
    output$maternal_summary <- maternal_summary_dt
    n_mothers <- nrow(maternal_summary_dt)
  }
  if("fathers" %in% type) {
    if(verbose) message("Summarizing paternal lines...")
    paternal_summary_dt <- calculate_summary_dt(ped_dt, patID, skip_var = skip_var, five_num_summary = five_num_summary)
    originating_member_paternal <- find_originating_member(ped_dt, group_var = patID,  sort_var = founder_sort_var)
    paternal_summary_dt <- merge(paternal_summary_dt, originating_member_paternal, by = patID, suffixes = c("", "_founder"))
    output$paternal_summary <- paternal_summary_dt
    n_fathers <- nrow(paternal_summary_dt)
  }

  # Optionally find the superlative lines
  # & noldest <= unique(ped_dt[[famID]])
  # determin number of lines


  ## oldest
  if(!is.null(byr) & noldest > 0) {
    if(!is.null(n_families) & "families" %in% type) {
      if(verbose) message("Finding oldest families...")
      output$oldest_families <- family_summary_dt[order(get(byr))][1:min(c(noldest,n_families),na.rm = TRUE)]
    }
    if(!is.null(n_mothers) & "mothers" %in% type ) {
      if(verbose) message("Finding oldest maternal lines...")
      output$oldest_maternal <- maternal_summary_dt[order(get(byr))][1:min(c(noldest,n_mothers),na.rm = TRUE)]
    }
    if(!is.null(n_fathers) & "fathers" %in% type) {
      if(verbose) message("Finding oldest paternal lines...")
      output$oldest_paternal <- paternal_summary_dt[order(get(byr))][1:min(c(noldest,n_fathers),na.rm = TRUE)]
    }
  }

  # biggest lines
  if(!is.null(nbiggest) & nbiggest > 0) {
    if(!is.null(n_families) & "families" %in% type) {
      output$biggest_families <- family_summary_dt[order(-get("count"))][1:min(c(nbiggest,n_families),na.rm = TRUE)]
    }
    if(!is.null(n_mothers) & "mothers" %in% type  ) {
      output$biggest_maternal <- maternal_summary_dt[order(-get("count"))][1:min(c(nbiggest,n_mothers),na.rm = TRUE)]
    }
    if(!is.null(n_fathers) & "fathers" %in% type ) {
      output$biggest_paternal <- paternal_summary_dt[order(-get("count"))][1:min(c(nbiggest,n_fathers),na.rm = TRUE)]
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
                                byr = NULL, founder_sort_var = NULL,
                                nbiggest = 5, noldest = 5, skip_var = NULL, five_num_summary = FALSE, verbose=FALSE) {
  # Call to wrapper function
  summarizePedigrees(ped = ped,
                    personID = personID,
                    nbiggest = nbiggest,
                    noldest = noldest,
                    byr=byr,
                    momID = momID, dadID = dadID,
                    famID = famID, matID=matID, patID=patID,skip_var = skip_var,
                    type = "mothers", verbose=verbose, five_num_summary = five_num_summary, founder_sort_var = founder_sort_var)
}

#' Summarize the paternal lines in a pedigree
#' @inheritParams summarizePedigrees
#' @seealso [summarizePedigrees ()]
#' @export
#'
summarizePatrilines <- function(ped, famID = "famID", personID = "ID",
                                momID = "momID", dadID = "dadID",
                                matID = "matID", patID = "patID",
                                byr = NULL, founder_sort_var = NULL,
                                nbiggest = 5, noldest = 5, skip_var = NULL, five_num_summary = FALSE, verbose=FALSE) {
  # Call to wrapper function
  summarizePedigrees(ped = ped,
                     personID = personID,
                     nbiggest = nbiggest,
                     noldest = noldest,
                     byr=byr,
                     momID = momID, dadID = dadID,
                     famID = famID, matID=matID, patID=patID, skip_var = skip_var,
                     type = "fathers", verbose=verbose, five_num_summary = five_num_summary, founder_sort_var = founder_sort_var)
}

#' Summarize the families in a pedigree
#' @inheritParams summarizePedigrees
#' @seealso [summarizePedigrees ()]
#' @export

summarizeFamilies <- function(ped, famID = "famID", personID = "ID",
                              momID = "momID", dadID = "dadID",
                              matID = "matID", patID = "patID",
                              byr = NULL, founder_sort_var = NULL,
                              nbiggest = 5, noldest = 5, skip_var = NULL, five_num_summary = FALSE, verbose=FALSE) {
  # Call to wrapper function
  summarizePedigrees(ped = ped,
                     personID = personID,
                     nbiggest = nbiggest,
                     noldest = noldest,
                     byr=byr,
                     momID = momID, dadID = dadID,
                     famID = famID, matID=matID, patID=patID, skip_var = skip_var,
                     type = "families", verbose=verbose, five_num_summary = five_num_summary, founder_sort_var = founder_sort_var)
}
