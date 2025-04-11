#' Convert Sparse Relationship Matrices to Kinship Links
#'
#' This function processes one or more sparse relationship components (additive, mitochondrial,
#' and common nuclear) and converts them into kinship link pairs. The resulting related pairs are
#' either returned as a data frame or written to disk in CSV format.
#'
#' @param rel_pairs_file File path to write related pairs to (CSV format).
#' @param ad_ped_matrix Matrix of additive genetic relatedness coefficients.
#' @param mit_ped_matrix Matrix of mitochondrial relatedness coefficients. Alias: \code{mt_ped_matrix}.
#' @param mt_ped_matrix Matrix of mitochondrial relatedness coefficients.
#' @param cn_ped_matrix Matrix of common nuclear relatedness coefficients.
#' @param write_buffer_size Number of related pairs to write to disk at a time.
#' @param gc Logical. If TRUE, performs garbage collection via \code{\link{gc}} to free memory.
#' @param writetodisk Logical. If TRUE, writes the related pairs to disk; if FALSE, returns a data frame.
#' @param verbose Logical. If TRUE, prints progress messages.
#' @param update_rate Numeric. Frequency (in iterations) at which progress messages are printed.
#' @param legacy Logical. If TRUE, uses the legacy branch of the function.
#' @param outcome_name Character string representing the outcome name (used in file naming).
#' @param drop_upper_triangular Logical. If TRUE, drops the upper triangular portion of the matrix.
#' @param include_all_links_1ped Logical. If TRUE, includes all links in the output. (Default is true when only one ped is provided)
#' @param ... Additional arguments to be passed to \code{\link{com2links}}
#'
#' @return A data frame of related pairs if \code{writetodisk} is FALSE; otherwise, writes the results to disk.
#' @export com2links

com2links <- function(
    rel_pairs_file = "dataRelatedPairs.csv",
    ad_ped_matrix = NULL,
    mit_ped_matrix = mt_ped_matrix,
    mt_ped_matrix = NULL,
    cn_ped_matrix = NULL,
    #  pat_ped_matrix = NULL,
    #  mat_ped_matrix = NULL,
    #  mapa_id_file = "data_mapaID.csv",
    write_buffer_size = 1000,
    update_rate = 1000,
    gc = TRUE,
    writetodisk = TRUE,
    verbose = FALSE,
    legacy = FALSE,
    outcome_name = "data",
    drop_upper_triangular = TRUE,
    include_all_links_1ped=FALSE,
    ...) {
  # --- Input Validations and Preprocessing ---

  # Ensure that at least one relationship matrix is provided.
  if (is.null(ad_ped_matrix) && is.null(mit_ped_matrix) && is.null(cn_ped_matrix)) {
    stop("At least one of 'ad_ped_matrix', 'mit_ped_matrix', or 'cn_ped_matrix' must be provided.")
  }
  # Validate and convert ad_ped_matrix to a sparse dgCMatrix if provided.
  if (!is.null(ad_ped_matrix)) {
    ad_ped_matrix <- validate_and_convert_matrix(
      mat = ad_ped_matrix,
      name = "ad_ped_matrix"
    )
  }

  # Validate and convert cn_ped_matrix to a sparse dgCMatrix if provided.
  if (!is.null(cn_ped_matrix)) {
    cn_ped_matrix <- validate_and_convert_matrix(
      mat = cn_ped_matrix,
      name = "cn_ped_matrix",
      ensure_symmetric = TRUE
    )
  }

  # Validate and process mit_ped_matrix: convert and ensure binary values.
  if (!is.null(mit_ped_matrix)) {
    mit_ped_matrix <- validate_and_convert_matrix(
      mat = mit_ped_matrix,
      name = "mit_ped_matrix", force_binary = TRUE,
      ensure_symmetric = TRUE
    )
  }

  # --- Build IDs and Prepare Matrix Pointers ---

  # Extract individual IDs from the first available matrix.
  ids <- NULL



  if (!is.null(cn_ped_matrix)) {
    ids <- as.numeric(dimnames(cn_ped_matrix)[[1]])
    nc <- ncol(cn_ped_matrix)
  } else if (!is.null(ad_ped_matrix)) {
    ids <- as.numeric(dimnames(ad_ped_matrix)[[1]])
    nc <- ncol(ad_ped_matrix)
  } else if (!is.null(mit_ped_matrix)) {
    ids <- as.numeric(dimnames(mit_ped_matrix)[[1]])
    nc <- ncol(mit_ped_matrix)
  }

  if (is.null(ids)) {
    stop("Could not extract IDs from the provided matrices.")
  }

  # --- matrix_case construction and switch dispatch ---
  matrix_case <- paste(sort(c(
    if (!is.null(ad_ped_matrix)) "ad" else NULL,
    if (!is.null(mit_ped_matrix)) "mt" else NULL,
    if (!is.null(cn_ped_matrix)) "cn" else NULL
  )), collapse = "-")

  if (verbose) {
    print(matrix_case)
  }

  switch(matrix_case,
         "ad" = process_one(
           matrix = ad_ped_matrix,
           rel_name = "addRel",
           ids = ids,
           nc = nc,
           rel_pairs_file = rel_pairs_file,
           writetodisk = writetodisk,
           write_buffer_size = write_buffer_size,
           drop_upper_triangular = drop_upper_triangular,
           update_rate = update_rate,
           verbose = verbose,
           gc = gc,
           include_all_links = include_all_links_1ped,
           ...
         ),
         "mt" = process_one(
           matrix = mit_ped_matrix,
           rel_name = "mitRel",
           ids = ids,
           nc = nc,
           rel_pairs_file = rel_pairs_file,
           writetodisk = writetodisk,
           write_buffer_size = write_buffer_size,
           drop_upper_triangular = drop_upper_triangular,
           update_rate = update_rate,
           verbose = verbose,
           gc = gc,
           include_all_links = include_all_links_1ped,
           ...
         ),
         "cn" = process_one(
           matrix = cn_ped_matrix,
           rel_name = "cnuRel",
           ids = ids,
           nc = nc,
           rel_pairs_file = rel_pairs_file,
           writetodisk = writetodisk,
           write_buffer_size = write_buffer_size,
           drop_upper_triangular = drop_upper_triangular,
           update_rate = update_rate,
           verbose = verbose,
           gc = gc,
           include_all_links = include_all_links_1ped,
           ...
         ),
         "ad-mt" = process_two(
           matrix1 = ad_ped_matrix,
           name1 = "addRel",
           matrix2 = mit_ped_matrix,
           name2 = "mitRel",
           ids = ids,
           nc = nc,
           rel_pairs_file = rel_pairs_file,
           writetodisk = writetodisk,
           write_buffer_size = write_buffer_size,
           drop_upper_triangular = drop_upper_triangular,
           update_rate = update_rate,
           verbose = verbose,
           gc = gc,
           ...
         ),
         "ad-cn" = process_two(
           matrix1 = ad_ped_matrix,
           name1 = "addRel",
           matrix2 = cn_ped_matrix,
           name2 = "cnuRel",
           ids = ids,
           nc = nc,
           rel_pairs_file = rel_pairs_file,
           writetodisk = writetodisk,
           write_buffer_size = write_buffer_size,
           drop_upper_triangular = drop_upper_triangular,
           update_rate = update_rate,
           verbose = verbose,
           gc = gc,
           ...
         ),
         "cn-mt" = process_two(
           matrix1 = cn_ped_matrix,
           name1 = "cnuRel",
           matrix2 = mit_ped_matrix,
           name2 = "mitRel",
           ids = ids,
           nc = nc,
           rel_pairs_file = rel_pairs_file,
           writetodisk = writetodisk,
           write_buffer_size = write_buffer_size,
           drop_upper_triangular = drop_upper_triangular,
           update_rate = update_rate,
           verbose = verbose,
           gc = gc,
           ...
         ),
         "ad-cn-mt" = process_all_three(
           mat1 = ad_ped_matrix,
           name1 = "addRel",
           mat2 = mit_ped_matrix,
           name2 = "mitRel",
           mat3 = cn_ped_matrix,
           name3 = "cnuRel",
           ids = ids,
           nc = nc,
           rel_pairs_file = rel_pairs_file,
           writetodisk = writetodisk,
           write_buffer_size = write_buffer_size,
           drop_upper_triangular = drop_upper_triangular,
           update_rate = update_rate,
           verbose = verbose,
           gc = gc,
           ...
         ),
         stop("Unsupported matrix combination")
  )
}
#' Convert Sparse Relationship Matrices to Kinship Links for one Matrix
#' @inheritParams com2links
#' @param include_all_links Logical. If TRUE, all links are included in the output.
#' @keywords internal



process_one <- function(matrix, rel_name, ids, nc, rel_pairs_file, writetodisk,
                        write_buffer_size, drop_upper_triangular, update_rate, verbose, gc,
                       include_all_links=TRUE, ...) {
  if (include_all_links == FALSE) {
  # Extract pointers and indices from the matrix.
  newColPos <- matrix@p + 1L
  iss <- matrix@i + 1L
  x <- matrix@x

  # Initialize the related pairs file with headers.
  df_relpairs <- initialize_empty_df(relNames = rel_name)

  if (writetodisk == TRUE) {
    utils::write.table(
      df_relpairs,
      file = rel_pairs_file, sep = ",", append = FALSE, row.names = FALSE
    )

    # Prepare an empty buffer for batching writes.
    write_buffer <- list()
    remove(df_relpairs)
  }

  # Process each column in the matrix.
  for (j in 1L:nc) {

      ID2 <- ids[j]

    # Extract column indices
    ncp <- newColPos[j]
    ncpp <- newColPos[j + 1L]
    cond <- ncp < ncpp
    if (cond) {
      vv <- ncp:(ncpp - 1L)
      issvv <- iss[vv]
    }

    # Create a unique set of row indices.
    u <- sort(issvv)

    # If any relationships exist for this individual, build the related pairs.
    if (cond) {
        # Create a data frame with unique pairs.
        ID1 <- ids[u]
        tds <- data.frame(ID1 = ID1, ID2 = ID2)
        tds[[rel_name]] <- 0
      if (cond) {
        tds[u %in% issvv, rel_name] <- x[vv]
      }
      if (drop_upper_triangular == TRUE) {
        tds <- tds[tds$ID1 <= tds$ID2, ] # or < if you want strictly lower triangle
      }

      # Write the batch to disk or accumulate in the data frame.
      if (nrow(tds) > 0) {
        if (writetodisk == TRUE) {
          write_buffer[[length(write_buffer) + 1]] <- tds

          if (length(write_buffer) >= write_buffer_size) { # Write in batches
            utils::write.table(do.call(rbind, write_buffer),
              file = rel_pairs_file,
              row.names = FALSE, col.names = FALSE, append = TRUE, sep = ","
            )
            write_buffer <- list()
          }
        } else {
          df_relpairs <- rbind(df_relpairs, tds)
        }
      }
    }
    if (verbose && (j %% update_rate == 0L)) {
      cat("Done with", j, "of", nc, "\n")
    }
  }
  # If not writing to disk, return the accumulated data frame.
  if (writetodisk == FALSE) {
    return(df_relpairs)
  } else {
    # Write any remaining buffered rows.
    if (length(write_buffer) > 0) {
      utils::write.table(do.call(rbind, write_buffer),
        file = rel_pairs_file,
        row.names = FALSE, col.names = FALSE, append = TRUE, sep = ","
      )
    }
  }
  if (gc == TRUE) {
    remove(newColPos, iss, x)
  }
  }else{
    matrix2=  matrix(rep(1,length(ids)^2),
              nrow = length(ids),
              dimnames = list(ids, ids))
    process_two(matrix2=matrix, name2=rel_name,
                  matrix1=methods::as(matrix2,"CsparseMatrix"),
                  name1="phantom",
                    ids=ids,
                  nc = nc,
                    rel_pairs_file = rel_pairs_file,
                    writetodisk = writetodisk,
                    write_buffer_size = write_buffer_size,
                    drop_upper_triangular = drop_upper_triangular,
                    update_rate = update_rate,
                    verbose = verbose,
                    gc = gc)



  }
}

process_all_three <- function(
    mat1, name1,
    mat2, name2,
    mat3, name3,
    ids, nc,
    rel_pairs_file,
    writetodisk,
    write_buffer_size,
    drop_upper_triangular,
    update_rate,
    verbose,
    gc,
    ...) {
  # Extract matrix slots
  p1 <- mat1@p + 1L
  i1 <- mat1@i + 1L
  x1 <- mat1@x
  p2 <- mat2@p + 1L
  i2 <- mat2@i + 1L
  x2 <- mat2@x
  p3 <- mat3@p + 1L
  i3 <- mat3@i + 1L
  x3 <- mat3@x

  relNames <- c(name1, name2, name3)
  df_relpairs <- initialize_empty_df(relNames)

  if (writetodisk) {
    utils::write.table(df_relpairs, file = rel_pairs_file, sep = ",", append = FALSE, row.names = FALSE)
    write_buffer <- list()
    rm(df_relpairs)
  }

  for (j in seq_len(nc)) {
    ID2 <- ids[j]

    # Get index spans
    v1 <- if (p1[j] < p1[j + 1L]) {
      idx <- p1[j]:(p1[j + 1L] - 1L)
      list(i = i1[idx], x = x1[idx])
    } else {
      NULL
    }
    v2 <- if (p2[j] < p2[j + 1L]) {
      idx <- p2[j]:(p2[j + 1L] - 1L)
      list(i = i2[idx], x = x2[idx])
    } else {
      NULL
    }
    v3 <- if (p3[j] < p3[j + 1L]) {
      idx <- p3[j]:(p3[j + 1L] - 1L)
      list(i = i3[idx], x = x3[idx])
    } else {
      NULL
    }

    # Union of index positions
    u <- sort(unique(c(
      if (!is.null(v1)) v1$i else NULL,
      if (!is.null(v2)) v2$i else NULL,
      if (!is.null(v3)) v3$i else NULL
    )))
    if (length(u) > 0) {
      ID1 <- ids[u]
      tds <- data.frame(ID1 = ID1, ID2 = ID2)
      tds[[name1]] <- if (!is.null(v1)) ifelse(u %in% v1$i, v1$x[match(u, v1$i)], 0) else 0
      tds[[name2]] <- if (!is.null(v2)) ifelse(u %in% v2$i, v2$x[match(u, v2$i)], 0) else 0
      tds[[name3]] <- if (!is.null(v3)) ifelse(u %in% v3$i, v3$x[match(u, v3$i)], 0) else 0

      if (drop_upper_triangular) {
        tds <- tds[tds$ID1 <= tds$ID2, ]
      }

      if (nrow(tds) > 0) {
        if (writetodisk) {
          write_buffer[[length(write_buffer) + 1L]] <- tds
          if (length(write_buffer) >= write_buffer_size) {
            utils::write.table(do.call(rbind, write_buffer),
              file = rel_pairs_file, row.names = FALSE,
              col.names = FALSE, append = TRUE, sep = ","
            )
            write_buffer <- list()
          }
        } else {
          df_relpairs <- rbind(df_relpairs, tds)
        }
      }
    }

    if (verbose && (j %% update_rate == 0L)) {
      cat("Done with", j, "of", nc, "\n")
    }
  }

  if (!writetodisk) {
    return(df_relpairs)
  } else if (length(write_buffer) > 0) {
    utils::write.table(do.call(rbind, write_buffer),
      file = rel_pairs_file, row.names = FALSE,
      col.names = FALSE, append = TRUE, sep = ","
    )
  }

  invisible(NULL)
}

process_two <- function(
    matrix1, name1,
    matrix2, name2,
    ids, nc,
    rel_pairs_file,
    writetodisk,
    write_buffer_size,
    drop_upper_triangular,
    update_rate,
    verbose,
    gc,
    ...) {
  # Extract internal slots
  p1 <- matrix1@p + 1L
  i1 <- matrix1@i + 1L
  x1 <- matrix1@x
  p2 <- matrix2@p + 1L
  i2 <- matrix2@i + 1L
  x2 <- matrix2@x

  relNames <- c(name1, name2)
  df_relpairs <- initialize_empty_df(relNames)

  if (writetodisk) {
    utils::write.table(df_relpairs, file = rel_pairs_file, sep = ",", append = FALSE, row.names = FALSE)
    write_buffer <- list()
    rm(df_relpairs)
  }

  for (j in seq_len(nc)) {
    ID2 <- ids[j]

    # Get index/value slices
    v1 <- if (p1[j] < p1[j + 1L]) {
      idx <- p1[j]:(p1[j + 1L] - 1L)
      list(i = i1[idx], x = x1[idx])
    } else {
      NULL
    }
    v2 <- if (p2[j] < p2[j + 1L]) {
      idx <- p2[j]:(p2[j + 1L] - 1L)
      list(i = i2[idx], x = x2[idx])
    } else {
      NULL
    }

    # Union of indices from both matrices
    u <- sort(unique(c(
      if (!is.null(v1)) v1$i else NULL,
      if (!is.null(v2)) v2$i else NULL
    )))

    if (length(u) > 0) {
      ID1 <- ids[u]
      tds <- data.frame(ID1 = ID1, ID2 = ID2)
      tds[[name1]] <- if (!is.null(v1)) ifelse(u %in% v1$i, v1$x[match(u, v1$i)], 0) else 0
      tds[[name2]] <- if (!is.null(v2)) ifelse(u %in% v2$i, v2$x[match(u, v2$i)], 0) else 0

      if (drop_upper_triangular) {
        tds <- tds[tds$ID1 <= tds$ID2, ]
      }

      if (nrow(tds) > 0) {
        if (writetodisk) {
          write_buffer[[length(write_buffer) + 1L]] <- tds
          if (length(write_buffer) >= write_buffer_size) {
            utils::write.table(do.call(rbind, write_buffer),
              file = rel_pairs_file, row.names = FALSE,
              col.names = FALSE, append = TRUE, sep = ","
            )
            write_buffer <- list()
          }
        } else {
          df_relpairs <- rbind(df_relpairs, tds)
        }
      }
    }

    if (verbose && (j %% update_rate == 0L)) {
      cat("Done with", j, "of", nc, "\n")
    }
  }

  if (!writetodisk) {
    return(df_relpairs)
  } else if (length(write_buffer) > 0) {
    utils::write.table(do.call(rbind, write_buffer),
      file = rel_pairs_file, row.names = FALSE,
      col.names = FALSE, append = TRUE, sep = ","
    )
  }

  invisible(NULL)
}


#' @title validate_and_convert_matrix
#' @description
#' This function validates and converts a matrix to a specific format.
#'
#' @param mat The matrix to be validated and converted.
#' @param name The name of the matrix for error messages.
#' @param ensure_symmetric Logical indicating whether to ensure the matrix is symmetric.
#' @param force_binary Logical indicating whether to force the matrix to be binary.
#'
#' @return The validated and converted matrix.
validate_and_convert_matrix <- function(mat, name, ensure_symmetric = FALSE, force_binary = FALSE) {
  if (!inherits(mat, c("matrix", "dgCMatrix", "dsCMatrix","generalMatrix",
                       "symmetricMatrix", "triangularMatrix", "dsyMatrix", "dspMatrix", "dsyMatrix",'CsparseMatrix'))) {
    stop(paste0("The '", name, "' must be a matrix or generalMatrix"))
  }
  if (!inherits(mat, "generalMatrix")) {
    mat <- methods::as(mat, if (ensure_symmetric) "symmetricMatrix" else "generalMatrix")
  }
  if (force_binary) {
    mat@x[mat@x > 0] <- 1
  }
  return(mat)
}

#' @title initialize_empty_df
#' @description
#' This function initializes an empty data frame with specified column names.
#'
#' @param relNames A vector of column names to be included in the data frame.
#'
#' @return An empty data frame with specified column names.
#' @keywords internal

initialize_empty_df <- function(relNames) {
  df <- data.frame(ID1 = numeric(0), ID2 = numeric(0))
  for (r in relNames) {
    df[[r]] <- numeric(0)
  }
  return(df)
}
