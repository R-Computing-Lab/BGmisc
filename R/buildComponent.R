#' Take a pedigree and turn it into a relatedness matrix
#' @param ped a pedigree dataset.  Needs ID, momID, and dadID columns
#' @param component character.  Which component of the pedigree to return.  See Details.
#' @param max_gen the maximum number of generations to compute
#'  (e.g., only up to 4th degree relatives).  The default is 25. However it can be set to infinity.
#'   `Inf` uses as many generations as there are in the data.
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
#' @param transpose_method character. The method to use for computing the transpose.  Options are "tcrossprod", "crossprod", or "star"
#' @param adjacency_method character. The method to use for computing the adjacency matrix.  Options are "loop", "indexed", direct or beta
#' @param isChild_method character. The method to use for computing the isChild matrix.  Options are "classic" or "partialparent"
#' @param adjBeta_method numeric The method to use for computing the building the adjacency_method matrix when using the "beta" build
#' @param compress logical. If TRUE, use compression when saving the checkpoint files.  Defaults to TRUE.
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
                    adjacency_method = "direct",
                    isChild_method = "classic",
                    saveable = FALSE,
                    resume = FALSE,
                    save_rate = 5,
                    save_rate_gen = save_rate,
                    save_rate_parlist = 100000 * save_rate,
                    update_rate = 100,
                    save_path = "checkpoint/",
                    adjBeta_method = NULL,
                    compress = TRUE,
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
    compress = compress
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
      "mtdna", "mitochondria"
    )
  )

  transpose_method_options <- c(
    "tcrossprod", "crossprod", "star",
    "tcross.alt.crossprod", "tcross.alt.star"
  )
  if (!config$transpose_method %in% transpose_method_options) {
    stop(paste0(
      "Invalid method specified. Choose from ",
      paste(transpose_method_options, collapse = ", "), "."
    ))
  }


  if (!config$adjacency_method %in%
    c("indexed", "loop", "direct", "beta")) {
    stop("Invalid method specified. Choose from 'indexed', 'loop', 'direct', or 'beta'.")
  }

  # standardize colnames
  if (config$standardize_colnames == TRUE) {
    ped <- standardizeColnames(ped, verbose = config$verbose)
  }

  # Load final result if computation was completed
  if (config$resume == TRUE && file.exists(checkpoint_files$final_matrix)) {
    if (config$verbose == TRUE) cat("Loading final computed matrix...\n")
    return(readRDS(checkpoint_files$final_matrix))
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
  # --- Step 2: Compute Relatedness Matrix ---


  if (config$resume == TRUE && file.exists(checkpoint_files$r_checkpoint) &&
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
  if (config$verbose == TRUE) {
    cat("About to do RAM path tracing\n")
  }

  # r is I + A + A^2 + ... = (I-A)^-1 from RAM
  # could trim, here
  while (mtSum != 0 && count < maxCount) {
    r <- r + newIsPar
    gen <- gen + (Matrix::rowSums(newIsPar) > 0)
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
    return(gen)
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

  # --- Step 4: T crossproduct  ---

  if (config$resume == TRUE && file.exists(checkpoint_files$tcrossprod_checkpoint) &&
    config$component != "generation") {
    if (config$verbose == TRUE) message("Resuming: Loading tcrossprod...\n")
    r <- readRDS(checkpoint_files$tcrossprod_checkpoint)
  } else {
    r <- .computeTranspose(
      r2 = r2, transpose_method = transpose_method,
      verbose = config$verbose
    )
    if (config$saveable == TRUE) {
      saveRDS(r,
        file = checkpoint_files$tcrossprod_checkpoint,
        compress = config$compress
      )
    }
  }

  if (config$component %in% c("mitochondrial", "mtdna", "mitochondria")) {
    r@x <- rep(1, length(r@x))
    # Assign 1 to all nonzero elements for mitochondrial component
  }

  if (config$sparse == FALSE) {
    r <- as.matrix(r)
  }
  # flattens diagonal if you don't want to deal with inbreeding
  if (config$flatten_diag == TRUE) {
    diag(r) <- 1
  }
  if (config$saveable == TRUE) {
    saveRDS(r, file = checkpoint_files$final_matrix, compress = config$compress)
  }
  return(r)
}

#' Take a pedigree and turn it into an additive genetics relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2add <- function(ped, max_gen = 25, sparse = TRUE, verbose = FALSE,
                    gc = FALSE,
                    flatten_diag = FALSE, standardize_colnames = TRUE,
                    transpose_method = "tcrossprod",
                    adjacency_method = "direct",
                    saveable = FALSE,
                    resume = FALSE,
                    save_rate = 5,
                    save_rate_gen = save_rate,
                    save_rate_parlist = 100000 * save_rate,
                    save_path = "checkpoint/",
                    compress = TRUE,
                    ...) {
  ped2com(
    ped = ped,
    max_gen = max_gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "additive",
    flatten_diag = flatten_diag,
    standardize_colnames = standardize_colnames,
    transpose_method = transpose_method,
    adjacency_method = adjacency_method,
    saveable = saveable,
    resume = resume,
    save_rate_gen = save_rate_gen,
    save_rate_parlist = save_rate_parlist,
    save_path = save_path,
    compress = compress,
    ...
  )
}

#' Take a pedigree and turn it into a mitochondrial relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#' @aliases ped2mt
#'
ped2mit <- ped2mt <- function(ped, max_gen = 25,
                              sparse = TRUE,
                              verbose = FALSE, gc = FALSE,
                              flatten_diag = FALSE,
                              standardize_colnames = TRUE,
                              transpose_method = "tcrossprod",
                              adjacency_method = "direct",
                              saveable = FALSE,
                              resume = FALSE,
                              save_rate = 5,
                              save_rate_gen = save_rate,
                              save_rate_parlist = 100000 * save_rate,
                              save_path = "checkpoint/",
                              compress = TRUE,
                              ...) {
  ped2com(
    ped = ped,
    max_gen = max_gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "mitochondrial",
    flatten_diag = flatten_diag,
    standardize_colnames = standardize_colnames,
    transpose_method = transpose_method,
    adjacency_method = adjacency_method,
    saveable = saveable,
    resume = resume,
    save_rate_gen = save_rate_gen,
    save_rate_parlist = save_rate_parlist,
    save_path = save_path,
    compress = compress,
    ...
  )
}

#' Take a pedigree and turn it into a common nuclear environmental  matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2cn <- function(ped, max_gen = 25, sparse = TRUE, verbose = FALSE,
                   gc = FALSE, flatten_diag = FALSE,
                   standardize_colnames = TRUE,
                   transpose_method = "tcrossprod",
                   saveable = FALSE,
                   resume = FALSE,
                   save_rate = 5,
                   adjacency_method = "direct",
                   save_rate_gen = save_rate,
                   save_rate_parlist = 1000 * save_rate,
                   save_path = "checkpoint/",
                   compress = TRUE,
                   ...) {
  ped2com(
    ped = ped,
    max_gen = max_gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "common nuclear",
    adjacency_method = adjacency_method,
    flatten_diag = flatten_diag,
    standardize_colnames = standardize_colnames,
    transpose_method = transpose_method,
    saveable = saveable,
    resume = resume,
    save_rate_gen = save_rate_gen,
    save_rate_parlist = save_rate_parlist,
    save_path = save_path,
    compress = compress,
    ...
  )
}
#' Take a pedigree and turn it into a generation relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2gen <- function(ped, max_gen = 25, sparse = TRUE, verbose = FALSE,
                    gc = FALSE, flatten_diag = FALSE,
                    standardize_colnames = TRUE,
                    transpose_method = "tcrossprod",
                    saveable = FALSE,
                    resume = FALSE,
                    save_rate = 5,
                    adjacency_method = "direct",
                    save_rate_gen = save_rate,
                    save_rate_parlist = 1000 * save_rate,
                    save_path = "checkpoint/",
                    compress = TRUE,
                    ...) {
  ped2com(
    ped = ped,
    max_gen = max_gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "generation",
    adjacency_method = adjacency_method,
    flatten_diag = flatten_diag,
    standardize_colnames = standardize_colnames,
    transpose_method = transpose_method,
    saveable = saveable,
    resume = resume,
    save_rate_gen = save_rate_gen,
    save_rate_parlist = save_rate_parlist,
    save_path = save_path,
    compress = compress,
    ...
  )
}


#' Take a pedigree and turn it into an extended environmental relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2ce <- function(ped, ...) {
  matrix(1, nrow = nrow(ped), ncol = nrow(ped), dimnames = list(ped$ID, ped$ID))
}


#' Compute the transpose multiplication for the relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @param r2 a relatedness matrix
#'
.computeTranspose <- function(r2, transpose_method = "tcrossprod", verbose = FALSE) {
  valid_methods <- c(
    "tcrossprod", "crossprod", "star",
    "tcross.alt.crossprod", "tcross.alt.star"
  )
  if (!transpose_method %in% valid_methods) {
    stop("Invalid method specified. Choose from
         'tcrossprod', 'crossprod', 'star', 'tcross.alt.crossprod',
         or 'tcross.alt.star'.")
  }

  # Map aliases to core methods
  alias_map <- c(
    "tcross.alt.crossprod" = "crossprod",
    "tcross.alt.star" = "star"
  )

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
    }
  )

  return(result)
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
    count_checkpoint = file.path(config$save_path, "count_checkpoint.rds"),
    final_matrix = file.path(config$save_path, "final_matrix.rds")
  )

  return(checkpoint_files)
}

#' Assign parent values based on component type
#' @inheritParams ped2com
.assignParentValue <- function(component) {
  # Set parent values depending on the component type
  if (component %in% c("generation", "additive")) {
    parVal <- .5
  } else if (component %in%
    c("common nuclear", "mitochondrial", "mtdna", "mitochondria")) {
    parVal <- 1
  } else {
    stop("Don't know how to set parental value")
  }
  return(parVal)
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
    return(readRDS(file))
  } else {
    if (config$verbose == TRUE && !is.null(message_compute)) cat(message_compute)
    result <- compute_fn()
    if (config$saveable == TRUE) saveRDS(result, file = file, compress = compress)
    return(result)
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

  return(isPar)
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

  return(isChild)
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
  return(r2)
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
    computed_indices <- which(!vapply(parList, is.null))
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
  return(list_of_adjacencies)
}
