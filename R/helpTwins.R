#' Determine isTwin Status
#' @param ped pedigree data frame
#' @return isTwin 'S' matrix
#' @keywords internal


isTwin <- function(ped) {
  isTwin <- apply(ped[, c("twinID")], 1, function(x) {
    !is.na(x)
  })
}


#' Find MZ twin pair_rows in a pedigree
#'
#' Identifies MZ twin pair_rows from the \code{twinID} column and returns their
#' row indices.  These indices are used later to merge the twins' columns in
#' the \code{r2} matrix before \code{tcrossprod}, which correctly produces
#' relatedness 1 between MZ co-twins with no diagonal or downstream artifacts.
#'
#' @param ped A pedigree data.frame with columns \code{ID} and \code{twinID}.
#'   Optionally a \code{zygosity} column; when present only pair_rows where both
#'   members have \code{zygosity == "MZ"} are used.
#' @param verbose logical. If TRUE, print progress messages.
#' @param returnIDs logical. If TRUE, return the IDs of the twin pair_rows instead of row indices.
#' @param returnRows logical. If TRUE, return the row indices of the twin pair_rows instead of IDs.
#' @param returnAsList logical. If TRUE, return results as a list of vectors
#'  (default). If FALSE, return results as a data.frame with separate columns for each twin's ID and row index.
#'  @param beta logical. If TRUE, use an optimized approach with O(1) lookups for large pedigrees. If FALSE (default), use a simpler approach that may be less efficient for large pedigrees.
#' @return A list of length-2 integer vectors \code{c(idx1, idx2)} giving the
#'   row indices of each MZ pair in the pedigree, or \code{NULL} if none found.
#' @keywords internal
findMZtwins <- function(ped, verbose = FALSE, returnRows = TRUE,
                        returnIDs = FALSE, returnAsList = TRUE,
                        beta = FALSE) {
  if (!"twinID" %in% colnames(ped)) {
    return(NULL)
  }

  twin_rows <- which(!is.na(ped$twinID))

  # If zygosity column exists, restrict to MZ pair_rows
  if ("zygosity" %in% colnames(ped)) {
    twin_rows <- twin_rows[!is.na(ped$zygosity[twin_rows]) &
      ped$zygosity[twin_rows] %in% c("mz", "MZ")]
  }

  if (length(twin_rows) == 0) {
    return(NULL)
  }

  # Build ID-to-row lookup for O(1) resolution instead of which() per pair
  id_to_row <- seq_len(nrow(ped))
  names(id_to_row) <- as.character(ped$ID)

  # Use environment as hash set for O(1) membership checks
  processed <- new.env(hash = TRUE, parent = emptyenv())

  pair_rows <- vector("list", length(twin_rows))
  if (returnIDs) {
    pair_ids <- vector("list", length(twin_rows))
  }
  n_pairs <- 0L

  for (idx in twin_rows) {
    twin_id <- ped$ID[idx]
    co_twin_id <- ped$twinID[idx]

    twin_id_chr <- as.character(twin_id)
    co_twin_id_chr <- as.character(co_twin_id)

    # Skip if already processed this pair (O(1) lookup)
    if (exists(twin_id_chr, envir = processed, inherits = FALSE) ||
      exists(co_twin_id_chr, envir = processed, inherits = FALSE)) {
      next
    }

    # O(1) row lookup via named vector
    idx1 <- id_to_row[twin_id_chr]
    idx2 <- id_to_row[co_twin_id_chr]

    if (is.na(idx1) || is.na(idx2)) next

    # Always put the lower index first for consistency
    if (idx1 > idx2) {
      tmp <- idx1
      idx1 <- idx2
      idx2 <- tmp
    }

    # O(1) insert into hash set
    assign(twin_id_chr, TRUE, envir = processed)
    assign(co_twin_id_chr, TRUE, envir = processed)

    n_pairs <- n_pairs + 1L
    pair_rows[[n_pairs]] <- c(idx1, idx2)
    if (returnIDs) {
      pair_ids[[n_pairs]] <- c(twin_id, co_twin_id)
    }
    if (verbose) {
      message(
        "MZ twin pair found: ", twin_id, " (row ", idx1,
        ") and ", co_twin_id, " (row ", idx2, ")"
      )
    }
  }

  # Trim pre-allocated lists to actual size
  if (n_pairs == 0L) {
    return(NULL)
  }
  pair_rows <- pair_rows[seq_len(n_pairs)]
  if (returnIDs) {
    pair_ids <- pair_ids[seq_len(n_pairs)]
  }

  if (returnIDs == TRUE && returnRows == FALSE) {
    if (returnAsList == TRUE) {
      return(pair_ids)
    } else {
      data.frame(
        twin1_id = vapply(pair_ids, `[`, numeric(1), 1L),
        twin2_id = vapply(pair_ids, `[`, numeric(1), 2L)
      )
    }
  } else if (returnRows == TRUE && returnIDs == FALSE) {
    if (returnAsList == TRUE) {
      return(pair_rows)
    } else {
      data.frame(
        twin1_row = vapply(pair_rows, `[`, integer(1), 1L),
        twin2_row = vapply(pair_rows, `[`, integer(1), 2L)
      )
    }
  } else if (returnIDs == TRUE && returnRows == TRUE) {
    if (returnAsList == TRUE) {
      return(list(pair_rows = pair_rows, pair_ids = pair_ids))
    } else {
      return(data.frame(
        twin1_id = vapply(pair_ids, `[`, numeric(1), 1L),
        twin2_id = vapply(pair_ids, `[`, numeric(1), 2L),
        twin1_row = vapply(pair_rows, `[`, integer(1), 1L),
        twin2_row = vapply(pair_rows, `[`, integer(1), 2L)
      ))
    }
  } else {
    stop("Invalid combination of returnRows and returnIDs parameters")
  }
}
# replace all MZ twin IDs with the first twin's ID in each pair so they are merged for the path tracing and all subsequent steps.  We will copy the values back to the second twin at the end.

#' Fuse MZ twin pairs in a pedigree dataset for path tracing
#' This function identifies MZ twin pairs in the pedigree dataset and merges their IDs for path tracing purposes. The second twin in each pair is made a founder (with NA parents), and all children of the second twin are redirected to the first twin. This allows for correct relatedness calculations without diagonal or downstream artifacts.
#' @param ped A pedigree data.frame with columns \code{ID}, \code{momID}, \code{dadID}, and optionally \code{twinID} and \code{zygosity}. The function will look for MZ twin pairs based on the \code{twinID} column and optionally restrict to MZ pairs if a \code{zygosity} column is present.
#' @param mz_id_pairs Optional list of length-2 character vectors specifying the IDs of MZ twin pairs to fuse. If provided, this will be used instead of automatically identifying MZ twins from the \code{twinID} column. Each element should be a character vector of length 2, e.g. \code{list(c("ID1", "ID2"), c("ID3", "ID4"))}.
#' @param mz_row_pairs Optional list of length-2 integer vectors specifying the row indices of MZ twin pairs to fuse. If provided, this will be used instead of automatically identifying MZ twins from the \code{twinID} column. Each element should be an integer vector of length 2, e.g. \code{list(c(1, 2), c(3, 4))}.
#' @param test_df_twins logical. If TRUE, return the data frame of twin pairs instead of the modified pedigree. Default is FALSE.
#' @param config A list of configuration options.
#' @return A modified version of the input pedigree data.frame with MZ twin pairs fused for path tracing. If \code{test_df_twins} is TRUE, returns the data frame of identified twin pairs instead.

fuseTwins <- function(ped,
                      mz_id_pairs = NULL,
                      mz_row_pairs = NULL,
                      config = list(verbose = FALSE),
                      test_df_twins = FALSE) {
  df_twins <- NULL

  if (is.null(mz_id_pairs) && is.null(mz_row_pairs)) {
    df_twins <- findMZtwins(ped,
      verbose = config$verbose,
      returnRows = TRUE, returnIDs = TRUE, returnAsList = FALSE
    )
    if (test_df_twins == TRUE) {
      return(df_twins)
    }
  }


  fuseattemptable <- !is.null(df_twins) || !is.null(mz_id_pairs) && length(mz_id_pairs) > 0 || !is.null(mz_row_pairs) && length(mz_row_pairs) > 0

  if (config$verbose == TRUE) {
    if (fuseattemptable == TRUE) {
      message("MZ twin pairs identified for fusion")
    } else {
      message("No MZ twin pairs identified for fusion.")
    }
  }

  if (fuseattemptable == TRUE) {
    # If df_twins is not already provided, construct it from the provided mz_id_pairs or mz_row_pairs
    if (is.null(mz_id_pairs) && !is.null(mz_row_pairs)) {
      df_twins <- lapply(mz_row_pairs, function(row) {
        twin1_id <- ped$ID[row[1]]
        twin2_id <- ped$ID[row[2]]
        data.frame(
          twin1_id = twin1_id,
          twin2_id = twin2_id,
          twin1_row = row[1],
          twin2_row = row[2]
        )
      })
      df_twins <- do.call(rbind, df_twins)
      # remove row names
      rownames(df_twins) <- NULL
      if (test_df_twins == TRUE) {
        return(df_twins)
      }
    } else if (!is.null(mz_id_pairs) && is.null(mz_row_pairs)) {
      df_twins <- lapply(mz_id_pairs, function(pair) {
        twin1_row <- which(ped$ID == pair[1])
        twin2_row <- which(ped$ID == pair[2])
        data.frame(
          twin1_id = pair[1], twin2_id = pair[2],
          twin1_row = twin1_row, twin2_row = twin2_row
        )
      })
      df_twins <- do.call(rbind, df_twins)
      rownames(df_twins) <- NULL
      if (test_df_twins == TRUE) {
        return(df_twins)
      }
    } else if (!is.null(mz_id_pairs) && !is.null(mz_row_pairs) && length(mz_id_pairs) == length(mz_row_pairs)) {
      df_twins <- lapply(1:length(mz_id_pairs), function(i) {
        twin1_id <- mz_id_pairs[[i]][1]
        twin2_id <- mz_id_pairs[[i]][2]
        twin1_row <- mz_row_pairs[[i]][1]
        twin2_row <- mz_row_pairs[[i]][2]
        data.frame(
          twin1_id = twin1_id, twin2_id = twin2_id,
          twin1_row = twin1_row, twin2_row = twin2_row
        )
      })
      df_twins <- do.call(rbind, df_twins)
      rownames(df_twins) <- NULL
      if (test_df_twins == TRUE) {
        return(df_twins)
      }
    } else if (!is.null(df_twins)) {
      # df_twins is already in the correct format
    } else {
      stop("Invalid input: must provide either mz_id_pairs, mz_row_pairs, or df_twins")
    }
    twin1s_id <- df_twins$twin1_id
    twin2s_id <- df_twins$twin2_id
    twin2s_row <- df_twins$twin2_row


    # Make twin2s founders
    ped$momID[twin2s_row] <- NA
    ped$dadID[twin2s_row] <- NA

    # Now redirect all children of twin2 to twin1
    ped$momID[ped$momID %in% twin2s_id] <- twin1s_id[match(ped$momID[ped$momID %in% twin2s_id], twin2s_id)]
    ped$dadID[ped$dadID %in% twin2s_id] <- twin1s_id[match(ped$dadID[ped$dadID %in% twin2s_id], twin2s_id)]

    if ("spouseID" %in% colnames(ped)) {
      ped$spouseID[ped$spouseID %in% twin2s_id] <- twin1s_id[match(ped$spouseID[ped$spouseID %in% twin2s_id], twin2s_id)]
    }
    if ("spID" %in% colnames(ped)) {
      ped$spID[ped$spID %in% twin2s_id] <- twin1s_id[match(ped$spID[ped$spID %in% twin2s_id], twin2s_id)]
    }

    if (config$verbose == TRUE) {
      message("Merged ", length(mz_pair_rows), " MZ twin pair(s) in pedigree dataset for path tracing")
    }
  } else {
    if (config$verbose == TRUE) {
      message("No MZ twin pair_rows found in pedigree dataset")
    }
  }

  return(ped)
}
