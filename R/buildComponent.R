#' Take a pedigree and turn it into a relatedness matrix
#' @param ped a pedigree dataset.  Needs ID, momID, and dadID columns
#' @param component character.  Which component of the pedigree to return.  See Details.
#' @param max_gen the maximum number of iterations that the adjacency matrix is multiplied to get the relatedness matrix. `Inf` uses as many iterations as there are in the data. Defaults to 25.
#' @param sparse logical.  If TRUE, use and return sparse matrices from Matrix package
#' @param verbose logical.  If TRUE, print progress through stages of algorithm
#' @param update_rate numeric. The rate at which to print progress
#' @param gc logical. If TRUE, do frequent garbage collection via \code{\link{gc}} to save memory
#' @param saveable logical. If TRUE, save the intermediate results to disk
#' @param save_rate numeric. The rate at which to save the intermediate results
#' @param save_rate_gen  numeric. The rate at which to save the intermediate results by generation. If NULL, defaults to save_rate
#' @param save_rate_parlist numeric. The rate at which to save the intermediate results by parent list. If NULL, defaults to save_rate*1000
#' @param resume logical. If TRUE, resume from a checkpoint
#' @param save_path character. The path to save the checkpoint files
#' @param flatten_diag logical. If TRUE, overwrite the diagonal of the final relatedness matrix with ones
#' @param standardize_colnames logical. If TRUE, standardize the column names of the pedigree dataset
#' @param transpose_method character. The method to use for computing the transpose.  Options are "tcrossprod", "crossprod", "star", or "chunked"
#' @param chunk_size numeric. If greater than 1 is Number of rows per chunk when \code{transpose_method = "chunked"}. Defaults to 1000. If less than or equal to 1, the entire matrix is processed in a single chunk.
#' @param keep_ids character vector of IDs to retain in the final relatedness matrix. When supplied, only the rows of \code{r2} corresponding to these IDs are used in the tcrossprod, so the result is a \code{length(keep_ids) x length(keep_ids)} matrix. All columns of \code{r2} are retained during the multiplication so relatedness values remain correct. IDs not found in the pedigree are silently dropped with a warning.
#' @param adjacency_method character. The method to use for computing the adjacency matrix.  Options are "loop", "indexed", direct or beta
#' @param isChild_method character. The method to use for computing the isChild matrix.  Options are "classic" or "partialparent"
#' @param adjBeta_method numeric The method to use for computing the building the adjacency_method matrix when using the "beta" build
#' @param compress logical. If TRUE, use compression when saving the checkpoint files.  Defaults to TRUE.
#' @param mz_twins logical. If TRUE, merge MZ co-twin columns in the r2 matrix before tcrossprod so that MZ twins are coded with relatedness 1 instead of 0.5. Twin pairs are identified from the \code{twinID} column. When a \code{zygosity} column is also present, only pairs where both members have \code{zygosity == "MZ"} are used; otherwise all \code{twinID} pairs are assumed to be MZ. Defaults to TRUE.
#' @param mz_method character. The method to handle MZ twins.  Options are "merging" (default) or "addtwins".  "addtwins" adds the twin2 column to the twin1 column before tcrossprod so that all relatedness flows through a single source, then leaves the twin2 column as zero and relies on the fact that the row/col names are the same to copy the values back to twin2 after tcrossprod.  "merging" merges the twin2 column into the twin1 column before tcrossprod and then copies the values back to twin2 after tcrossprod so that both twins appear in the final matrix.
#' @param force_symmetric logical. If TRUE, force the final relatedness matrix to be symmetric. This can help mitigate any numerical asymmetry introduced by the transpose method, especially when using sparse matrices. Defaults to TRUE.
#' @param beta logical. Used for benchmarking
#' @param ... additional arguments to be passed to \code{\link{ped2com}}
#' @details The algorithms and methodologies used in this function are further discussed and exemplified in the vignette titled "examplePedigreeFunctions". For more advanced scenarios and detailed explanations, consult this vignette.
#' @export
#'
ped2com <- function(ped, component,
                    max_gen = 25,
                    sparse = TRUE,
                    verbose = FALSE,
                    gc = FALSE,
                    flatten_diag = FALSE,
                    standardize_colnames = TRUE,
                    transpose_method = "tcrossprod",
                    chunk_size = 1000L,
                    keep_ids = NULL,
                    adjacency_method = "direct",
                    isChild_method = "partialparent",
                    saveable = FALSE,
                    resume = FALSE,
                    save_rate = 5,
                    save_rate_gen = save_rate,
                    save_rate_parlist = 100000 * save_rate,
                    update_rate = 100,
                    save_path = "checkpoint/",
                    adjBeta_method = NULL,
                    compress = TRUE,
                    mz_twins = TRUE,
                    mz_method = "addtwins",
                    beta = FALSE,
                    force_symmetric = FALSE,
                    ...) {
  #------
  # Check inputs
  #------

  config <- list(
    verbose = verbose,
    saveable = saveable,
    resume = resume,
    save_path = save_path,
    max_gen = max_gen,
    sparse = sparse,
    flatten_diag = flatten_diag,
    standardize_colnames = standardize_colnames,
    transpose_method = transpose_method,
    adjacency_method = adjacency_method,
    isChild_method = isChild_method,
    save_rate = save_rate,
    save_rate_gen = save_rate_gen,
    save_rate_parlist = save_rate_parlist,
    update_rate = update_rate,
    gc = gc,
    component = component,
    adjBeta_method = adjBeta_method,
    nr = nrow(ped),
    compress = compress,
    keep_ids = keep_ids,
    mz_method = mz_method,
    mz_twins = mz_twins,
    force_symmetric = force_symmetric
  )


  #------
  # Checkpointing
  #------
  if (config$saveable == TRUE || config$resume == TRUE) { # prepare checkpointing
    if (config$verbose == TRUE) message("Preparing checkpointing...\n")
    # initialize checkpoint files
    checkpoint_files <- initializeCheckpoint(config)
  }
  #------
  # Validation/Preparation
  #------

  # Validate the 'component' argument and match it against predefined choices
  config$component <- match.arg(tolower(config$component),
    choices = c(
      "generation",
      "additive",
      "common nuclear",
      "mitochondrial",
      "mtdna", "mitochondria",
      "distance"
    )
  )

  transpose_method_options <- c(
    "tcrossprod", "crossprod", "star",
    "tcross.alt.crossprod", "tcross.alt.star",
    "chunked"
  )

  if (!config$transpose_method %in% transpose_method_options) {
    stop(paste0(
      "Invalid method specified. Choose from ",
      paste(transpose_method_options, collapse = ", "), "."
    ))
  }

  # Validate the 'adjacency_method' argument
  adjacency_method_options <- c("indexed", "loop", "direct", "beta")
  if (!config$adjacency_method %in% adjacency_method_options
  ) {
    stop(paste0(
      "Invalid method specified. Choose from ",
      paste(adjacency_method_options, collapse = ", "), "."
    ))
  }

  # standardize colnames
  if (config$standardize_colnames == TRUE) {
    ped <- standardizeColnames(ped, verbose = config$verbose)
  }

  mz_row_pairs <- NULL
  mz_id_pairs <- NULL

  if (config$mz_twins == TRUE && "twinID" %in% colnames(ped)) {
    df_mz <- findMZtwins(ped,
      verbose = config$verbose,
      returnIDs = TRUE,
      returnRows = TRUE,
      returnAsList = TRUE,
      beta = beta
    )
    mz_row_pairs <- df_mz$pair_rows
    mz_id_pairs <- df_mz$pair_ids
  }


  # Load final result if computation was completed
  if (config$resume == TRUE && file.exists(checkpoint_files$final_matrix)) {
    if (config$verbose == TRUE) cat("Loading final computed matrix...\n")
    return(readRDS(checkpoint_files$final_matrix))
  }

  if (config$mz_method %in% c("merging") && config$mz_twins == TRUE && !is.null(mz_row_pairs) && length(mz_row_pairs) > 0 &&
    config$component %in% c("additive")) {
    # replace all MZ twin IDs with the first twin's ID in each pair so they are merged for the path tracing and all subsequent steps.  We will copy the values back to the second twin at the end.
    ped <- fuseTwins(ped = ped, mz_row_pairs = mz_row_pairs, mz_id_pairs = mz_id_pairs, config = config, beta = beta)
    if (config$verbose == TRUE) {
      message("Merged ", length(mz_row_pairs), " MZ twin pair(s) in pedigree dataset for path tracing")
    }
  }


  #------
  # Algorithm
  #------

  # Get the number of rows in the pedigree dataset,
  # representing the size of the family
  #  nr <- nrow(ped)

  # Print the family size if verbose is TRUE
  if (config$verbose == TRUE) {
    cat(paste0("Family Size = ", config$nr, "\n"))
  }

  #
  # Step 1: Construct parent-child adjacency matrix

  ## A. Resume from Checkpoint if Needed
  ## Initialize variables
  list_of_adjacencies <- .loadOrComputeParList(
    checkpoint_files = checkpoint_files,
    ped = ped,
    config = config
  )


  ## B. Resume loop from the next uncomputed index


  # Construct sparse matrix
  # Garbage collection if gc is TRUE
  if (config$gc == TRUE) {
    gc()
  }

  # Assign parent values based on the component type
  parVal <- .assignParentValue(component = config$component)

  # Construct sparse matrix
  # Initialize adjacency matrix for parent-child relationships
  isPar <- .loadOrComputeIsPar(
    iss = list_of_adjacencies$iss,
    jss = list_of_adjacencies$jss,
    parVal = parVal,
    ped = ped,
    checkpoint_files = checkpoint_files,
    config = config
  )
  if (config$verbose == TRUE) {
    cat("Completed first degree relatives (adjacency)\n")
  }

  # isPar is the adjacency matrix.  'A' matrix from RAM
  if (config$component %in% c("common nuclear")) {
    Matrix::diag(isPar) <- 1

    if (!is.null(config$keep_ids)) {
      isPar <- .subsetKeepIds(
        component = isPar,
        keep_ids = keep_ids,
        available_ids = rownames(isPar),
        config = config,
        drop = FALSE,
        verbose_message = "Subsetting adjacency matrix to %d target individuals\n"
      ) # also need to drop columns here because the adjacency matrix is used in the path tracing and we want to make sure the paths are correct for the target individuals.  We will keep all columns for the path tracing but subset to the target rows so that the relatedness values are correct for the target individuals.

      if (length(rownames(isPar)) > 1) {
        isPar <- isPar[, rownames(isPar), drop = FALSE]
      } # else {
      #      isPar <- isPar[rownames(isPar)]
      #    }
      #  isPar <- isPar[, rownames(isPar)] #
    }
    if (config$sparse == FALSE) {
      isPar <- as.matrix(isPar)
    }
    return(isPar)
  }

  # isChild is the 'S' matrix from RAM
  isChild <- .loadOrComputeIsChild(
    ped = ped,
    checkpoint_files = checkpoint_files,
    config = config,
    compress = config$compress
  )

  # TODO merge twin columns
  # --- Step 2: Compute Relatedness Matrix ---


  if (config$resume == TRUE && file.exists(checkpoint_files$ram_checkpoint)) {
    if (config$verbose == TRUE) cat("Resuming: Loading completed RAM matrix...\n")
    r <- readRDS(checkpoint_files$ram_checkpoint)
    gen <- rep(0, config$nr)
    mtSum <- 0
    count <- 0
  } else if (config$resume == TRUE && file.exists(checkpoint_files$r_checkpoint) &&
    file.exists(checkpoint_files$gen_checkpoint) &&
    file.exists(checkpoint_files$mtSum_checkpoint) &&
    file.exists(checkpoint_files$newIsPar_checkpoint) &&
    file.exists(checkpoint_files$count_checkpoint)
  ) {
    if (config$verbose == TRUE) cat("Resuming: Loading previous computation...\n")
    r <- readRDS(checkpoint_files$r_checkpoint)
    gen <- readRDS(checkpoint_files$gen_checkpoint)
    mtSum <- readRDS(checkpoint_files$mtSum_checkpoint)
    newIsPar <- readRDS(checkpoint_files$newIsPar_checkpoint)
    count <- readRDS(checkpoint_files$count_checkpoint)
  } else {
    r <- Matrix::Diagonal(x = 1, n = config$nr)
    gen <- rep(1, config$nr)
    mtSum <- sum(r, na.rm = TRUE)
    newIsPar <- isPar
    count <- 0
  }
  maxCount <- config$max_gen + 1

  # Ancestor distance matrix: ancDist[i, j] = min parent-child steps from i
  # up to ancestor j; NA = j is not an ancestor of i; diagonal = 0 (self).
  # Only allocated for the "distance" component; other components ignore it.
  if (config$component == "distance") {
    ancDist <- matrix(NA_integer_,
      nrow = config$nr, ncol = config$nr,
      dimnames = list(ped$ID, ped$ID)
    )
    diag(ancDist) <- 0L
  }

  if (config$verbose == TRUE) {
    cat("About to do RAM path tracing\n")
  }

  # r is I + A + A^2 + ... = (I-A)^-1 from RAM
  # could trim, here
  ## it keeps going until it explains all of the relatedness with themselves (i.e., mtSum == 0)
  # some of this precision is artificial because we literally get to the point that the condon is eaither there or not. probabiliticy

  # how much percision do we need to get unbiased estimates

  # big matrix still happens here because the network is built. just less percise on inbreeding

  # bias-precision tradeoff. how much percision do we need to get unbiased estimates? not a lot
  while (mtSum != 0 && count < maxCount) {
    r <- r + newIsPar
    gen <- gen + (Matrix::rowSums(newIsPar) > 0)

    # Record first-hit ancestor distances.  At this point newIsPar = A^(count+1),
    # so the step count for these entries is count + 1.
    if (config$component == "distance") {
      Ak_t <- methods::as(newIsPar, "TsparseMatrix")
      ri <- Ak_t@i + 1L # 0-based → 1-based row (child)
      ci <- Ak_t@j + 1L # 0-based → 1-based col (ancestor)
      if (length(ri) > 0L) {
        fresh <- is.na(ancDist[cbind(ri, ci)])
        if (any(fresh)) {
          ancDist[cbind(ri[fresh], ci[fresh])] <- count + 1L
        }
      }
    }

    newIsPar <- newIsPar %*% isPar
    mtSum <- sum(newIsPar)
    count <- count + 1
    if (config$verbose == TRUE) {
      cat(paste0("Completed ", count - 1, " degree relatives\n"))
    }
    # Save progress every save_rate iterations
    if (config$saveable == TRUE && (count %% save_rate_gen == 0)) {
      saveRDS(r, file = checkpoint_files$r_checkpoint, compress = config$compress)
      saveRDS(gen, file = checkpoint_files$gen_checkpoint, compress = config$compress)
      saveRDS(newIsPar, file = checkpoint_files$newIsPar_checkpoint, compress = config$compress)
      saveRDS(mtSum, file = checkpoint_files$mtSum_checkpoint, compress = config$compress)
      saveRDS(count, file = checkpoint_files$count_checkpoint, compress = config$compress)
    }
    if (config$gc == TRUE && config$nr > 1000000) {
      gc()
    } # extra gc if large
  }
  # compute rsq <- r %*% sqrt(diag(isChild))
  # compute rel <- tcrossprod(rsq)
  if (config$gc == TRUE) {
    rm(isPar, newIsPar)
    gc()
  }
  if (config$saveable == TRUE) {
    saveRDS(r, file = checkpoint_files$ram_checkpoint, compress = config$compress)
  }

  if (config$component == "generation") { # no need to do the rest
    gen <- .subsetKeepIds(
      component = gen,
      keep_ids = config$keep_ids,
      available_ids = rownames(r),
      config = config,
      verbose_message = "Subsetting generation component to %d target individuals\n"
    )
    return(gen)
  } else if (config$component == "distance") { # no need to do the rest
    if (!is.null(config$keep_ids)) {
      keep_idx <- match(as.character(config$keep_ids), rownames(ancDist))
      keep_idx <- keep_idx[!is.na(keep_idx)]
      ancDist <- ancDist[keep_idx, , drop = FALSE]
    }
    return(ancDist)
  } else {
    if (config$verbose == TRUE) {
      cat("Completed RAM path tracing\n")
    }
  }

  # --- Step 3: I-A inverse times diagonal multiplication ---
  r2 <- .loadOrComputeInverseDiagonal(
    r = r,
    isChild = isChild,
    checkpoint_files = checkpoint_files,
    config = config,
    compress = config$compress
  )

  if (config$mz_method == "addtwins" && config$mz_twins == TRUE && !is.null(mz_row_pairs) && length(mz_row_pairs) > 0) {
    if (config$verbose == TRUE) {
      message("MZ twin merging enabled: Will merge MZ twin columns in r2 before tcrossprod")
    }

    # --- Step 3b: Add  ---
    # MZ twins share the same genetic source.  We absorb twin2's column into
    # twin1's before tcrossprod so all path-traced relatedness flows through a
    # single source.  After tcrossprod we copy twin1's row/col back to twin2.
    if (!is.null(mz_row_pairs) && length(mz_row_pairs) > 0 && config$component %in% c("additive")) {
      # Extract all indices at once for batch operations
      pairs_mat <- do.call(rbind, mz_row_pairs)
      idx1_all <- pairs_mat[, 1]
      idx2_all <- pairs_mat[, 2]
      # Batch: absorb all twin2 columns into twin1 columns, then zero twin2
      r2[, idx1_all] <- r2[, idx1_all, drop = FALSE] + r2[, idx2_all, drop = FALSE]
      r2[, idx2_all] <- 0

      if (config$verbose == TRUE) {
        message("Added ", length(mz_row_pairs), " MZ twin pair column(s) in r2")
      }
    }
  }
  # --- Step 4: T crossproduct  ---

  # Subset rows of r2 to target individuals if requested.
  # All columns are kept so dot products use the full ancestry paths.
  if (!is.null(config$keep_ids)) {
    r2 <- .subsetKeepIds(
      component = r2,
      keep_ids = config$keep_ids,
      available_ids = rownames(r2),
      config = config,
      verbose_message = "Subsetting r2 to %d target individuals before tcrossprod\n",
      drop = FALSE
    )
  }

  use_tcrossprod_checkpoint <- FALSE
  if (config$resume == TRUE && file.exists(checkpoint_files$tcrossprod_checkpoint) &&
    config$component != "generation") {
    saved_keep_ids <- if (file.exists(checkpoint_files$tcrossprod_ids)) {
      readRDS(checkpoint_files$tcrossprod_ids)
    } else {
      saved_keep_ids <- NULL
      if (config$verbose == TRUE) {
        message("No saved keep_ids found for tcrossprod checkpoint.  Expected at: ", checkpoint_files$tcrossprod_ids, "\n")
      }
    }
    if (identical(saved_keep_ids, config$keep_ids)) {
      if (config$verbose == TRUE) message("Resuming: Loading tcrossprod...\n")
      r <- readRDS(checkpoint_files$tcrossprod_checkpoint)
      use_tcrossprod_checkpoint <- TRUE
    } else {
      use_tcrossprod_checkpoint <- FALSE
      warning(
        "tcrossprod checkpoint keep_ids do not match \u2014 recomputing.\n",
        "  saved:    ", if (is.null(saved_keep_ids)) "NULL (full pedigree)" else paste(length(saved_keep_ids), "IDs"), "\n",
        "  expected: ", if (is.null(config$keep_ids)) "NULL (full pedigree)" else paste(length(config$keep_ids), "IDs")
      )
    }
  }

  if (use_tcrossprod_checkpoint == FALSE) {
    if (config$saveable == TRUE) {
      saveRDS(config$keep_ids, file = checkpoint_files$tcrossprod_ids, compress = config$compress)
    }
    r <- .computeTranspose(
      r2 = r2,
      transpose_method = transpose_method,
      chunk_size = chunk_size,
      verbose = config$verbose,
      force_symmetric = config$force_symmetric,
      config = config,
      checkpoint_files = if (exists("checkpoint_files")) checkpoint_files else NULL
    )
    if (config$saveable == TRUE) {
      saveRDS(r, file = checkpoint_files$tcrossprod_checkpoint, compress = config$compress)
    }
  }

  if (config$mz_method %in% c("merging", "addtwins") && config$mz_twins == TRUE && config$component %in% c("additive") && !is.null(mz_row_pairs) && length(mz_row_pairs) > 0) {
    # --- Step 4b: Restore MZ twins ---
    # Copy twin1's row/col to twin2 so both twins appear in the final matrix.
    if (config$sparse == FALSE) {
      r <- as.matrix(r)
      rnames <- rownames(r)
      ids_mat <- do.call(rbind, mz_id_pairs)
      idx1_all <- match(ids_mat[, 1], rnames)
      idx2_all <- match(ids_mat[, 2], rnames)
      # Batch copy: twin1 rows/cols -> twin2 rows/cols
      r[idx2_all, ] <- r[idx1_all, ]

      r[, idx2_all] <- r[, idx1_all]
    } else {
      # TODO this is really slow.  Can we do it without coercing to dense?  Maybe by doing row/col replacement on the sparse matrix directly?  Or by constructing a sparse matrix with the twin2 values and adding it to r?
      #  r <- df_add

      rnames <- r@Dimnames[[1]]

      ids_mat <- do.call(rbind, mz_id_pairs)
      # needs to use sparse indexing to avoid coercion to dense
      idx1_all <- match(ids_mat[, 1], rnames)
      idx2_all <- match(ids_mat[, 2], rnames)

      twin1_rows <- r[idx1_all, , drop = FALSE]
      twin1_cols <- r[, idx1_all, drop = FALSE]
      twin1_rows@Dimnames[[1]] <- rnames[idx2_all]
      twin1_cols@Dimnames[[2]] <- rnames[idx2_all]
      twin1_self <- r[idx1_all, idx1_all, drop = FALSE]
      twin1_self@Dimnames[[1]] <- rnames[idx2_all]

      r[idx2_all, ] <- twin1_rows
      r[, idx2_all] <- twin1_cols
      r[idx2_all, idx2_all] <- twin1_self

      # Batch copy: twin1 rows/cols -> twin2 rows/cols

      # Row/column replacement on a dsCMatrix (symmetric) causes Matrix to
      # coerce to dgCMatrix (general), doubling stored entries.  Convert back

      r <- Matrix::drop0(r)

      # so both mz_method paths return the same sparse class.
      if (methods::is(r, "CsparseMatrix") && !methods::is(r, "symmetricMatrix")) {
        r <- Matrix::forceSymmetric(r)
      }
    }
    if (config$verbose == TRUE) {
      message("Restored ", length(mz_row_pairs), " MZ twin pair(s) in relatedness matrix")
    }
  }


  if (config$component %in% c("mitochondrial", "mtdna", "mitochondria")) {
    r@x <- rep(1, length(r@x))
    # Assign 1 to all nonzero elements for mitochondrial component
  }

  # Remove explicit zeros so that both mz_method paths produce
  # structurally identical sparse matrices

  if (config$sparse == FALSE && !methods::is(r, "matrix")) {
    r <- as.matrix(r)
  }
  # flattens diagonal if you don't want to deal with inbreeding
  if (config$flatten_diag == TRUE) {
    diag(r) <- 1
  }
  if (config$saveable == TRUE) {
    saveRDS(r, file = checkpoint_files$final_matrix, compress = config$compress)
  }
  r
}

#' Compute the transpose multiplication for the relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @param r2 a relatedness matrix
#' @param force_symmetric logical. If TRUE, forces the output matrix to be symmetric using Matrix::forceSymmetric.  This can be helpful when using chunked multiplication to ensure the final result is exactly symmetric despite potential numerical issues.  Defaults to TRUE.
#' @param checkpoint_files list of checkpoint file paths for saving intermediate results when using chunked multiplication. Should contain at least 'tcrossprod_checkpoint' for saving the chunks and 'tcrossprod_ids' for saving the keep_ids used in the tcrossprod checkpoint.
#' @param config list of configuration parameters, used for checkpointing and verbose output
.computeTranspose <- function(r2, transpose_method = "tcrossprod", chunk_size = 1000L,
                              verbose = FALSE, force_symmetric = FALSE, config = NULL,
                              checkpoint_files = NULL) {
  valid_methods <- c(
    "tcrossprod", "crossprod", "star",
    "tcross.alt.crossprod", "tcross.alt.star",
    "chunked"
  )
  if (!transpose_method %in% valid_methods) {
    stop("Invalid method specified. Choose from
         'tcrossprod', 'crossprod', 'star', 'tcross.alt.crossprod',
         'tcross.alt.star', or 'chunked'.")
  }

  # Map aliases to core methods
  alias_map <- c(
    "tcross.alt.crossprod" = "crossprod",
    "tcross.alt.star" = "star"
  )

  if (chunk_size > 1) {
    chunk_size_method <- "lines"
  } else if (chunk_size > 0 && chunk_size <= 1) {
    chunk_size_method <- "proportion"
  } else {
    chunk_size_method <- "full"
  }


  if (transpose_method %in% names(alias_map)) {
    method_normalized <- alias_map[[transpose_method]]
  } else {
    method_normalized <- transpose_method
  }

  result <- switch(method_normalized,
    "tcrossprod" = {
      if (verbose == TRUE) cat("Doing tcrossprod\n")
      Matrix::tcrossprod(r2)
    },
    "crossprod" = {
      if (verbose == TRUE) cat("Doing tcrossprod using crossprod(t(.))\n")
      crossprod(t(as.matrix(r2)))
    },
    "star" = {
      if (verbose == TRUE) cat("Doing tcrossprod using %*% t(.)\n")
      r2 %*% t(as.matrix(r2))
    },
    "chunked" = {
      n <- nrow(r2)
      if (chunk_size_method == "proportion") {
        chunk_size <- ceiling(n * chunk_size)
      }
      n_chunks <- ceiling(n / chunk_size)
      if (verbose == TRUE) {
        cat(sprintf(
          "Doing chunked tcrossprod: %d rows, chunk size %d (%d chunks)\n",
          n, chunk_size, n_chunks
        ))
      }
      blocks <- vector("list", n_chunks)
      for (i in seq_len(n_chunks)) {
        row_start <- (i - 1L) * chunk_size + 1L
        row_end <- min(i * chunk_size, n)
        if (verbose == TRUE) {
          cat(sprintf("  chunk %d/%d (rows %d-%d)\n", i, n_chunks, row_start, row_end))
        }

        if (config$resume == TRUE && file.exists(paste0(checkpoint_files$tcrossprod_checkpoint, "_chunk_", i, ".rds"))) {
          if (verbose == TRUE) cat("    Resuming: Loading chunk from checkpoint...\n")
          blocks[[i]] <- readRDS(paste0(checkpoint_files$tcrossprod_checkpoint, "_chunk_", i, ".rds"))
          next
        }
        blocks[[i]] <- Matrix::tcrossprod(r2[row_start:row_end, , drop = FALSE], r2)
        gc()

        if (config$saveable == TRUE) {
          saveRDS(blocks[[i]], file = paste0(checkpoint_files$tcrossprod_checkpoint, "_chunk_", i, ".rds"), compress = config$compress)
        }
      }


      if (force_symmetric == TRUE) {
        Matrix::forceSymmetric(do.call(rbind, blocks))
      } else {
        do.call(rbind, blocks)
      }
    }
  )

  result
}

#' Initialize checkpoint files
#' @inheritParams ped2com
#' @keywords internal

initializeCheckpoint <- function(config = list(
                                   verbose = FALSE,
                                   saveable = FALSE,
                                   resume = FALSE,
                                   save_path = "checkpoint/"
                                 )) {
  # Define checkpoint files
  # Ensure save path exists
  if (config$saveable == TRUE && !dir.exists(config$save_path)) {
    if (config$verbose == TRUE) cat("Creating save path...\n")
    dir.create(config$save_path, recursive = TRUE)
  } else if (config$resume == TRUE && !dir.exists(config$save_path)) {
    stop("Cannot resume from checkpoint. Save path does not exist.")
  }

  checkpoint_files <- list(
    parList = file.path(config$save_path, "parList.rds"),
    lens = file.path(config$save_path, "lens.rds"),
    isPar = file.path(config$save_path, "isPar.rds"),
    iss = file.path(config$save_path, "iss.rds"),
    jss = file.path(config$save_path, "jss.rds"),
    isChild = file.path(config$save_path, "isChild.rds"),
    r_checkpoint = file.path(config$save_path, "r_checkpoint.rds"),
    gen_checkpoint = file.path(config$save_path, "gen_checkpoint.rds"),
    newIsPar_checkpoint = file.path(
      config$save_path,
      "newIsPar_checkpoint.rds"
    ),
    mtSum_checkpoint = file.path(config$save_path, "mtSum_checkpoint.rds"),
    ram_checkpoint = file.path(config$save_path, "ram_checkpoint.rds"),
    r2_checkpoint = file.path(config$save_path, "r2_checkpoint.rds"),
    tcrossprod_checkpoint = file.path(
      config$save_path,
      "tcrossprod_checkpoint.rds"
    ),
    tcrossprod_ids = file.path(config$save_path, "tcrossprod_ids.rds"),
    count_checkpoint = file.path(config$save_path, "count_checkpoint.rds"),
    final_matrix = file.path(config$save_path, "final_matrix.rds")
  )

  checkpoint_files
}

#' Assign parent values based on component type
#' @inheritParams ped2com
.assignParentValue <- function(component) {
  # Set parent values depending on the component type
  if (component %in% c("generation", "additive", "distance")) {
    parVal <- .5
  } else if (component %in%
    c("common nuclear", "mitochondrial", "mtdna", "mitochondria")) {
    parVal <- 1
  } else {
    stop("Don't know how to set parental value")
  }
  parVal
}

#' Load or compute a checkpoint
#' @param file The file path to load the checkpoint from.
#' @param compute_fn The function to compute the checkpoint if it doesn't exist.
#' @param config A list containing configuration parameters such as `resume`, `verbose`, and `saveable`.
#' @param message_resume Optional message to display when resuming from a checkpoint.
#' @param message_compute Optional message to display when computing the checkpoint.
#' @param compress a logical specifying whether saving to a named file is to use "gzip" compression, or one of "gzip", "bzip2", "xz" or "zstd" to indicate the type of compression to be used. Ignored if file is a connection.
#' @return The loaded or computed checkpoint.
#' @keywords internal
loadOrComputeCheckpoint <- function(file, compute_fn,
                                    config, message_resume = NULL,
                                    message_compute = NULL,
                                    compress = TRUE) {
  if (config$resume == TRUE && file.exists(file)) {
    if (config$verbose == TRUE && !is.null(message_resume)) cat(message_resume)
    readRDS(file)
  } else {
    if (config$verbose == TRUE && !is.null(message_compute)) cat(message_compute)
    result <- compute_fn()
    if (config$saveable == TRUE) saveRDS(result, file = file, compress = compress)
    result
  }
}

#' Load or compute the isPar matrix
#' @inheritParams loadOrComputeCheckpoint
#' @inheritParams ped2com
#' @param iss The row indices of the sparse matrix.
#' @param jss The column indices of the sparse matrix.
#' @param parVal The value to assign to the non-zero elements of the sparse matrix.
#' @param ped The pedigree dataset.
#' @param checkpoint_files A list of checkpoint file paths.
#'
#' @keywords internal
#' @importFrom Matrix sparseMatrix
.loadOrComputeIsPar <- function(iss, jss, parVal, ped, checkpoint_files, config,
                                compress = TRUE) {
  isPar <- loadOrComputeCheckpoint(
    file = checkpoint_files$isPar,
    compute_fn = function() {
      Matrix::sparseMatrix(
        i = iss, j = jss, x = parVal,
        dims = c(config$nr, config$nr),
        dimnames = list(ped$ID, ped$ID)
      )
    },
    config = config,
    message_resume = "Resuming: Loading adjacency matrix...\n",
    message_compute = "Initializing adjacency matrix...\n",
    compress = compress
  )

  isPar
}

#' Load or compute the isChild matrix
#' @inheritParams loadOrComputeCheckpoint
#' @inheritParams ped2com
#' @param checkpoint_files A list of checkpoint file paths.
#'
#'  @keywords internal

.loadOrComputeIsChild <- function(ped, checkpoint_files, config, compress = TRUE) {
  isChild <- loadOrComputeCheckpoint(
    file = checkpoint_files$isChild,
    compute_fn = function() isChild(isChild_method = config$isChild_method, ped = ped),
    config = config,
    message_resume = "Resuming: Loading isChild matrix...\n",
    message_compute = "Computing isChild matrix...\n",
    compress = compress
  )

  isChild
}

#' Load or compute the inverse diagonal matrix
#' @inheritParams loadOrComputeCheckpoint
#' @inheritParams ped2com
#' @param r The relatedness matrix.
#' @importFrom Matrix Diagonal
#' @keywords internal
#' @return The computed inverse diagonal matrix.


.loadOrComputeInverseDiagonal <- function(r, isChild,
                                          checkpoint_files, config,
                                          compress = TRUE) {
  r2 <- loadOrComputeCheckpoint(
    file = checkpoint_files$r2_checkpoint,
    compute_fn = function() {
      r %*% Matrix::Diagonal(x = sqrt(isChild), n = config$nr)
    },
    config = config,
    message_resume = "Resuming: Loading I-A inverse...\n",
    message_compute = "Doing I-A inverse times diagonal multiplication\n",
    compress = compress
  )
  if (config$gc == TRUE) {
    rm(r, isChild)
    gc()
  }
  r2
}


#' parent-child adjacency data
#' @inheritParams loadOrComputeCheckpoint
#' @inheritParams ped2com
#' @param checkpoint_files A list of checkpoint file paths.
#' @param config A list containing configuration parameters such as `resume`, `verbose`, and `saveable`.
#' @param parList A list of parent-child adjacency data.
#' @param lens A vector of lengths for each parent-child relationship.
#' @keywords internal

#' @return A list containing the parent-child adjacency data either
#'         loaded from a checkpoint or initialized.
#'

.loadOrComputeParList <- function(checkpoint_files, config,
                                  ped = NULL,
                                  parList = NULL, lens = NULL,
                                  compress = TRUE) {
  if (config$resume == TRUE &&
    file.exists(checkpoint_files$parList) &&
    file.exists(checkpoint_files$lens)) {
    if (config$verbose == TRUE) {
      message("Resuming: Loading parent-child adjacency data...\n")
    }
    parList <- readRDS(checkpoint_files$parList)
    lens <- readRDS(checkpoint_files$lens)
    computed_indices <- which(!vapply(parList, is.null, logical(1)))
    lastComputed <- if (length(computed_indices) > 0) {
      max(computed_indices)
    } else {
      0
    }
    if (config$verbose == TRUE) message("Resuming from iteration", lastComputed + 1, "\n")
  } else {
    ## Initialize variables
    parList <- vector("list", config$nr)
    lens <- integer(config$nr)
    lastComputed <- 0
    if (config$verbose == TRUE) cat("Building parent adjacency matrix...\n")
  }

  if (config$resume == TRUE &&
    file.exists(checkpoint_files$iss) &&
    file.exists(checkpoint_files$jss)) { # fix to check actual
    if (config$verbose == TRUE) message("Resuming: Constructed matrix...\n")
    jss <- readRDS(checkpoint_files$jss)
    iss <- readRDS(checkpoint_files$iss)
    list_of_adjacencies <- list(iss = iss, jss = jss)
  } else {
    if (config$verbose == TRUE) message("Computing parent-child adjacency matrix...\n")
    list_of_adjacencies <- computeParentAdjacency(
      ped = ped,
      save_rate_parlist = config$save_rate_parlist,
      checkpoint_files = checkpoint_files,
      component = config$component,
      adjacency_method = config$adjacency_method, # adjacency_method,
      saveable = config$saveable,
      resume = config$resume,
      save_path = config$save_path,
      update_rate = config$update_rate,
      verbose = config$verbose,
      lastComputed = lastComputed,
      config = config,
      parList = parList,
      lens = lens,
      adjBeta_method = config$adjBeta_method
    )

    # Construct sparse matrix


    if (config$verbose == TRUE) {
      message("Constructed sparse matrix\n")
    }
    if (config$saveable == TRUE) {
      saveRDS(list_of_adjacencies$jss, file = checkpoint_files$jss, compress = compress)
      saveRDS(list_of_adjacencies$iss, file = checkpoint_files$iss, compress = compress)
    }
  }
  list_of_adjacencies
}


#' Subset output to requested IDs
#' @inheritParams ped2com
#' @param component A component to subset.
#' @param keep_ids Character vector of IDs to retain.
#' @param available_ids Character vector of IDs available in \code{x}.
#' @param verbose_message Character. Message prefix to print when \code{config$verbose == TRUE}.
#' @param drop logical. Passed to \code{[} when subsetting matrices.
#' @keywords internal
.subsetKeepIds <- function(component, keep_ids = NULL, available_ids, config,
                           verbose_message = "Subsetting to %d target individuals\n",
                           drop = FALSE) {
  if (is.null(keep_ids)) {
    return(component)
  }

  idx <- match(keep_ids, available_ids)
  missing <- keep_ids[is.na(idx)]

  if (length(missing) > 0) {
    warning(
      length(missing), " keep_ids not found in pedigree and will be dropped: ",
      paste(Matrix::head(missing, 5), collapse = ", "),
      if (length(missing) > 5) " ..." else ""
    )
  }

  idx <- idx[!is.na(idx)]

  if (config$verbose == TRUE) {
    cat(sprintf(verbose_message, length(idx)))
  }
  # consequence is missing data
  if (is.matrix(component) || methods::is(component, "Matrix")) {
    component <- component[idx, , drop = drop]
  } else {
    component <- component[idx]
  }

  component
}
