utils::globalVariables(c(".N", ".SD"))

#' Summarize Pedigree Data
#'
#' This function summarizes pedigree data, by computing key summary statistics
#' for all numeric variables and identifying the originating member (founder)
#' for each family, maternal, and paternal lineage.
#'
#' The function calculates standard descriptive statistics, including the count
#' of individuals in each lineage, means, medians, minimum and maximum values,
#' and standard deviations.
#' Additionally, if `five_num_summary = TRUE`, the function includes the first
#' and third quartiles (Q1, Q3) to provide a more detailed distributional
#' summary. Users can also specify variables to exclude from the analysis via
#' `skip_var`.
#'
#' Beyond summary statistics, the function identifies the founding member of
#' each lineage based on the specified sorting variable (`founder_sort_var`),
#' defaulting to birth year (`byr`) when available or `personID` otherwise.
#' Users can retrieve the largest and oldest lineages by setting `nbiggest`
#' and `noldest`, respectively.
#'
#' @inheritParams ped2fam
#' @inheritParams ped2maternal
#' @inheritParams ped2paternal
#' @param nbiggest Integer. Number of largest lineages to return (sorted by count).
#' @param noldest Integer. Number of oldest lineages to return (sorted by birth year).
#' @param byr Character. Optional column name for birth year. Used to determine the oldest lineages.
#' @param type Character vector. Specifies which summaries to compute.
#'   Options: `"fathers"`, `"mothers"`, `"families"`. Default includes all three.
#' @param skip_var Character vector. Variables to exclude from summary calculations.
#' @param five_num_summary Logical. If `TRUE`, includes the first quartile (Q1) and third quartile (Q3) in addition to
#'   the minimum, median, and maximum values.
#' @param include_founder Logical. If `TRUE`, includes the founder (originating member) of each lineage in the output.
#' @param founder_sort_var Character. Column used to determine the founder of each lineage.
#'   Defaults to `byr` (if available) or `personID` otherwise.
#' @param network_checks Logical. If `TRUE`, performs network checks on the pedigree data.
#' @param verbose Logical, if TRUE, print progress messages.
#' @returns A data.frame (or list) containing summary statistics for family, maternal, and paternal lines, as well as the 5 oldest and biggest lines.
#' @importFrom data.table as.data.table
#' @export
summarizePedigrees <- function(ped, famID = "famID", personID = "ID",
                               momID = "momID", dadID = "dadID",
                               matID = "matID", patID = "patID",
                               type = c("fathers", "mothers", "families"),
                               byr = NULL,
                               include_founder = FALSE,
                               founder_sort_var = NULL,
                               nbiggest = 5,
                               noldest = nbiggest,
                               skip_var = NULL,
                               five_num_summary = FALSE,
                               network_checks = FALSE,
                               verbose = FALSE) {
  # Fast Fails

  ## Check that the ID variables are not the same
  if (personID %in% c(famID, momID, dadID, matID, patID)) {
    stop("personID cannot be the same as any of the other ID variables.")
  }
  ## Check that neccesarry variables are present
  if (!all(c(personID, momID, dadID) %in% names(ped))) {
    stop("personID, momID, and dadID must be columns in the pedigree data.")
  }
  if (is.null(type)) {
    stop("Type must be specified. Options are 'fathers', 'mothers', and 'families'.")
  }
  if (!is.null(byr) && !byr %in% names(ped)) {
    stop("If byr is specified, byr must be a column in the pedigree data. If you do not want to sort by birth year, set byr = NULL.")
  }
  if (!is.null(founder_sort_var) && !founder_sort_var %in% names(ped)) {
    stop("If you set founder_sort_var, that variable must be a column in the pedigree data. If you want to sort by using the default, set founder_sort_var = NULL. The default is to sort by birth year if that's present and by personID otherwise.")
  }

  # user inputs
  ## what variables to skip
  if (is.null(skip_var)) {
    skip_var <- c(personID, famID, momID, dadID, matID, patID)
  } else {
    skip_var <- c(skip_var, personID, famID, momID, dadID, matID, patID)
  }
  ## How to sort/identify the founders
  ## If founder_sort_var is NULL, sort by byr if it's present, otherwise sort by personID
  if (is.null(founder_sort_var) && is.null(byr)) {
    founder_sort_var <- personID
  } else if (is.null(founder_sort_var) & !is.null(byr)) {
    founder_sort_var <- byr
  }

  # Build the pedigree using the provided functions
  if ("families" %in% type && !famID %in% names(ped)) {
    if (verbose) message("Counting families...")
    ped <- ped2fam(ped,
      personID = personID,
      momID = momID, dadID = dadID, famID = famID
    )
  }
  if ("mothers" %in% type && !matID %in% names(ped)) {
    if (verbose == TRUE) message("Counting mothers...")
    ped <- ped2maternal(ped,
      personID = personID,
      momID = momID, dadID = dadID, matID = matID
    )
  }
  if ("fathers" %in% type && !patID %in% names(ped)) {
    if (verbose == TRUE) message("Counting fathers...")
    ped <- ped2paternal(ped,
      personID = personID,
      momID = momID, dadID = dadID, patID = patID
    )
  }


  # Convert to data.table
  ped_dt <- data.table::as.data.table(ped)

  # Initialize...
  ## Output list
  output <- list()
  ## Size of families
  n_fathers <- n_mothers <- n_families <- NULL


  if (network_checks) {
    if (verbose == TRUE) message("Performing network validation checks...")
    output$network_validation <- checkPedigreeNetwork(
      ped,
      personID = personID,
      momID = momID,
      dadID = dadID,
      verbose = verbose
    )
  }

  # Calculate summary statistics for families, maternal lines, and paternal lines

  if ("families" %in% type) {
    if (verbose == TRUE) message("Summarizing families...")
    family_summary_dt <- calculateSummaryDT(ped_dt, famID,
      skip_var = skip_var,
      five_num_summary = five_num_summary
    )
    # Find the originating member for each line
    if (include_founder == TRUE) {
      family_summary_dt <- summarizeFounder(
        verbose = verbose, ped_dt = ped_dt,
        group_var = famID,
        sort_var = founder_sort_var,
        foo_summary_dt = family_summary_dt
      )
    }
    output$family_summary <- family_summary_dt
    n_families <- nrow(family_summary_dt)
    if (verbose == TRUE) message("Summarized ", n_families, " families.")
  }

  if ("mothers" %in% type) {
    if (verbose == TRUE) message("Summarizing maternal lines...")
    maternal_summary_dt <- calculateSummaryDT(ped_dt, matID,
      skip_var = skip_var,
      five_num_summary = five_num_summary
    )
    if (include_founder) {
      maternal_summary_dt <- summarizeFounder(
        verbose = verbose, ped_dt = ped_dt,
        group_var = matID,
        sort_var = founder_sort_var,
        foo_summary_dt = maternal_summary_dt
      )
    }
    output$maternal_summary <- maternal_summary_dt
    n_mothers <- nrow(maternal_summary_dt)
    if (verbose == TRUE) message("Summarized ", n_mothers, " maternal lines.")
  }
  if ("fathers" %in% type) {
    if (verbose == TRUE) message("Summarizing paternal lines...")
    paternal_summary_dt <- calculateSummaryDT(ped_dt, patID,
      skip_var = skip_var,
      five_num_summary = five_num_summary
    )
    if (include_founder) {
      paternal_summary_dt <- summarizeFounder(
        verbose = verbose, ped_dt = ped_dt,
        group_var = patID,
        sort_var = founder_sort_var,
        foo_summary_dt = paternal_summary_dt
      )
    }

    output$paternal_summary <- paternal_summary_dt
    n_fathers <- nrow(paternal_summary_dt)
    if (verbose == TRUE) message("Summarized ", n_fathers, " paternal lines.")
  }

  ## Check errors
  #  if (check_errors) {
  #   if (verbose) message("Checking for errors...")
  #    output$checkIDs <- checkIDs(ped,
  #                                repair = FALSE, verbose = verbose)
  #  }

  ## oldest
  if (!is.null(byr) && noldest > 0) {
    if (!is.null(n_families) && "families" %in% type) {
      if (verbose == TRUE) message("Finding oldest families...")
      output$oldest_families <- findOldest(
        foo_summary_dt = family_summary_dt,
        byr = byr,
        noldest = noldest,
        n_foo = n_families
      )
    }
    if (!is.null(n_mothers) && "mothers" %in% type) {
      if (verbose == TRUE) message("Finding oldest maternal lines...")
      output$oldest_maternal <- findOldest(
        foo_summary_dt = maternal_summary_dt,
        byr = byr,
        noldest = noldest,
        n_foo = n_mothers
      )
    }
    if (!is.null(n_fathers) && "fathers" %in% type) {
      if (verbose == TRUE) message("Finding oldest paternal lines...")
      output$oldest_paternal <- findOldest(
        foo_summary_dt = paternal_summary_dt,
        byr = byr,
        noldest = noldest,
        n_foo = n_fathers
      )
    }
  }

  # biggest lines
  if (!is.null(nbiggest) && nbiggest > 0) {
    if (!is.null(n_families) && "families" %in% type) {
      output$biggest_families <- findBiggest(
        foo_summary_dt = family_summary_dt,
        nbiggest = nbiggest,
        n_foo = n_families
      )
    }
    if (!is.null(n_mothers) && "mothers" %in% type) {
      output$biggest_maternal <- findBiggest(
        foo_summary_dt = maternal_summary_dt,
        nbiggest = nbiggest,
        n_foo = n_mothers
      )
    }
    if (!is.null(n_fathers) && "fathers" %in% type) {
      output$biggest_paternal <- findBiggest(
        foo_summary_dt = paternal_summary_dt,
        nbiggest = nbiggest,
        n_foo = n_fathers
      )
    }
  }
  return(output)
}


#' Function to calculate summary statistics for all numeric variables
#' This function calculates summary statistics for all numeric variables in a data.table. It is supposed to be used internally by the \code{summarize_pedigree} function.
#' @inheritParams summarizePedigrees
#' @param data A data.table containing the pedigree data.
#' @param group_var A character string specifying the column name of the grouping variable.
#' @return A data.table containing the summary statistics for all numeric variables.
#' @keywords internal
#'
calculateSummaryDT <- function(data, group_var, skip_var,
                               five_num_summary = FALSE) {
  # Identify numeric columns excluding the group_var and skip_var
  numeric_cols <- setdiff(
    names(data)[vapply(data, is.numeric, logical(1))],
    c(group_var, skip_var)
  )
  summary_stats <- data[,
    {
      count <- .N # Calculate count once per group
      stats_list <- lapply(numeric_cols, function(colname) {
        x <- .SD[[colname]]
        stats <- list(
          #   count = .N,
          mean = as.double(base::mean(x, na.rm = TRUE)),
          median = as.double(stats::median(x, na.rm = TRUE)),
          #  mode = as.double(stats::mode(x, na.rm = TRUE)),
          min = ifelse(all(is.na(x)), as.double(NA),
            as.double(base::min(x, na.rm = TRUE))
          ),
          max = ifelse(all(is.na(x)), as.double(NA),
            as.double(base::max(x, na.rm = TRUE))
          ),
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
    },
    by = group_var,
    .SDcols = numeric_cols
  ]
  # Flatten the nested lists
  summary_stats <- data.table::as.data.table(summary_stats[, lapply(.SD, unlist),
    by = group_var
  ])
  return(summary_stats)
}

#' Function to find the originating member for each line
#'
#' This function finds the originating member for each line in a pedigree. It is supposed to be used internally by the \code{summarize_pedigree} function.
#' @inheritParams summarizePedigrees
#' @param sort_var A character string specifying the column name to sort by.
#' @param data A data.table containing the pedigree data.
#'
#' @return A data.table containing the originating member for each line.
#' @keywords internal
#'
findFounder <- function(data, group_var, sort_var) {
  data[order(get(sort_var)), .SD[1], by = group_var]
}

#' Function to summarize the originating members for each line
#'
#' This function summarizes the originating members for each line in a pedigree.
#' It is supposed to be used internally by the \code{summarize_pedigree} function.
#'
#' @inheritParams summarizePedigrees
#' @inheritParams findFounder
#'
#' @keywords internal

summarizeFounder <- function(ped_dt, group_var, sort_var,
                             foo_summary_dt, verbose) {
  if (verbose) {
    message(paste0(
      "Finding originating members for ",
      "group_var"
    ))
  }
  originating_member_foo <- findFounder(
    data = ped_dt,
    group_var = group_var,
    sort_var = sort_var
  )
  # Merge summary statistics with originating members for additional information
  foo_summary_dt <- merge(foo_summary_dt,
    originating_member_foo,
    by = group_var, suffixes = c("", "_founder")
  )
  return(foo_summary_dt)
}
#' Summarize the maternal lines in a pedigree
#' @inheritParams summarizePedigrees
#' @seealso [summarizePedigrees ()]
#' @export
#'
summarizeMatrilines <- function(ped, famID = "famID", personID = "ID",
                                momID = "momID", dadID = "dadID",
                                matID = "matID", patID = "patID",
                                byr = NULL, include_founder = FALSE,
                                founder_sort_var = NULL,
                                nbiggest = 5, noldest = 5, skip_var = NULL,
                                five_num_summary = FALSE, verbose = FALSE) {
  # Call to wrapper function
  summarizePedigrees(
    ped = ped,
    personID = personID,
    nbiggest = nbiggest,
    noldest = noldest,
    byr = byr,
    include_founder = include_founder,
    momID = momID, dadID = dadID,
    famID = famID, matID = matID, patID = patID, skip_var = skip_var,
    type = "mothers", verbose = verbose, five_num_summary = five_num_summary,
    founder_sort_var = founder_sort_var
  )
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
                                include_founder = FALSE,
                                nbiggest = 5, noldest = 5, skip_var = NULL,
                                five_num_summary = FALSE, verbose = FALSE) {
  # Call to wrapper function
  summarizePedigrees(
    ped = ped,
    personID = personID,
    nbiggest = nbiggest,
    noldest = noldest,
    byr = byr,
    include_founder = include_founder,
    momID = momID, dadID = dadID,
    famID = famID, matID = matID, patID = patID, skip_var = skip_var,
    type = "fathers", verbose = verbose, five_num_summary = five_num_summary,
    founder_sort_var = founder_sort_var
  )
}

#' Summarize the families in a pedigree
#' @inheritParams summarizePedigrees
#' @seealso [summarizePedigrees ()]
#' @export

summarizeFamilies <- function(ped, famID = "famID", personID = "ID",
                              momID = "momID", dadID = "dadID",
                              matID = "matID", patID = "patID",
                              byr = NULL, founder_sort_var = NULL,
                              include_founder = FALSE,
                              nbiggest = 5, noldest = 5, skip_var = NULL,
                              five_num_summary = FALSE, verbose = FALSE) {
  # Call to wrapper function
  summarizePedigrees(
    ped = ped,
    personID = personID,
    nbiggest = nbiggest,
    noldest = noldest,
    byr = byr,
    include_founder = include_founder,
    momID = momID,
    dadID = dadID,
    famID = famID,
    matID = matID,
    patID = patID,
    skip_var = skip_var,
    type = "families",
    verbose = verbose,
    five_num_summary = five_num_summary,
    founder_sort_var = founder_sort_var
  )
}
# Function to find the oldest individuals in a pedigree
#' This function finds the oldest families in a pedigree. It is supposed to be used internally by the \code{summarize_pedigree} function.
#' @inheritParams summarizePedigrees
#' @param foo_summary_dt A data.table containing the summary statistics.
#' @param n_foo An integer specifying the number of individuals in the summary.
#' @returns a data.table containing the oldest families in the pedigree.

findOldest <- function(foo_summary_dt, byr, noldest, n_foo) {
  oldest_foo <- try_na(foo_summary_dt[order(get(byr))][1:min(c(noldest, n_foo),
    na.rm = TRUE
  )])
  return(oldest_foo)
}

# Function to find the biggest families in a pedigree

#' This function finds the biggest families in a pedigree. It is supposed to be
#' used internally by the \code{summarize_pedigree} function.
#' @inheritParams findOldest
#' @inheritParams summarizePedigrees
#' @returns a data.table containing the biggest families in the pedigree.


findBiggest <- function(foo_summary_dt, nbiggest, n_foo) {
  biggest_foo <- try_na(
    foo_summary_dt[order(-get("count"))][1:min(c(nbiggest, n_foo),
      na.rm = TRUE
    )]
  )
  return(biggest_foo)
}

#' @rdname summarizePedigrees
#' @export
summarisePedigrees <- summarizePedigrees

#' @rdname summarizeFamilies
#' @export
summariseFamilies <- summarizeFamilies

#' @rdname summarizeMatrilines
#' @export
summariseMatrilines <- summarizeMatrilines

#' @rdname summarizePatrilines
#' @export
summarisePatrilines <- summarizePatrilines
