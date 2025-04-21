#' Take a pedigree and turn it into a relatedness matrix
#' @param ped a pedigree dataset.  Needs ID, momID, and dadID columns
#' @param component character.  Which component of the pedigree to return.  See Details.
#' @param max.gen the maximum number of generations to compute
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
#' @param flatten.diag logical. If TRUE, overwrite the diagonal of the final relatedness matrix with ones
#' @param standardize.colnames logical. If TRUE, standardize the column names of the pedigree dataset
#' @param transpose_method character. The method to use for computing the transpose.  Options are "tcrossprod", "crossprod", or "star"
#' @param adjacency_method character. The method to use for computing the adjacency matrix.  Options are "loop", "indexed", direct or beta
#' @param isChild_method character. The method to use for computing the isChild matrix.  Options are "classic" or "partialparent"
#' @param adjBeta_method numeric The method to use for computing the building the adjacency_method matrix when using the "beta" build
#' @param ... additional arguments to be passed to \code{\link{ped2com}}
#' @details The algorithms and methodologies used in this function are further discussed and exemplified in the vignette titled "examplePedigreeFunctions". For more advanced scenarios and detailed explanations, consult this vignette.
#' @export
#'
ped2com <- function(ped, component,
                    max.gen = 25,
                    sparse = TRUE,
                    verbose = FALSE,
                    gc = FALSE,
                    flatten.diag = FALSE,
                    standardize.colnames = TRUE,
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
                    ...) {
  #------
  # Checkpointing
  #------
  if (saveable || resume) { # prepare checkpointing
    if (verbose) cat("Preparing checkpointing...\n")
    # Ensure save path exists
    if (saveable && !dir.exists(save_path)) {
      if (verbose) cat("Creating save path...\n")
      dir.create(save_path, recursive = TRUE)
    } else if (resume && !dir.exists(save_path)) {
      stop("Cannot resume from checkpoint. Save path does not exist.")
    }

    # Define checkpoint files
    checkpoint_files <- list(
      parList = file.path(save_path, "parList.rds"),
      lens = file.path(save_path, "lens.rds"),
      isPar = file.path(save_path, "isPar.rds"),
      iss = file.path(save_path, "iss.rds"),
      jss = file.path(save_path, "jss.rds"),
      isChild = file.path(save_path, "isChild.rds"),
      r_checkpoint = file.path(save_path, "r_checkpoint.rds"),
      gen_checkpoint = file.path(save_path, "gen_checkpoint.rds"),
      newIsPar_checkpoint = file.path(save_path, "newIsPar_checkpoint.rds"),
      mtSum_checkpoint = file.path(save_path, "mtSum_checkpoint.rds"),
      r2_checkpoint = file.path(save_path, "r2_checkpoint.rds"),
      tcrossprod_checkpoint = file.path(save_path, "tcrossprod_checkpoint.rds"),
      count_checkpoint = file.path(save_path, "count_checkpoint.rds"),
      final_matrix = file.path(save_path, "final_matrix.rds")
    )
  }
  #------
  # Validation/Preparation
  #------

  # Validate the 'component' argument and match it against predefined choices
  component <- match.arg(tolower(component),
    choices = c(
      "generation",
      "additive",
      "common nuclear",
      "mitochondrial"
    )
  )

  if (!transpose_method %in% c("tcrossprod", "crossprod", "star", "tcross.alt.crossprod", "tcross.alt.star")) {
    stop("Invalid method specified. Choose from 'tcrossprod', 'crossprod', or 'star' or 'tcross.alt.crossprod' or 'tcross.alt.star'.")
  }
  if (!adjacency_method %in% c("indexed", "loop", "direct", "beta")) {
    stop("Invalid method specified. Choose from 'indexed', 'loop', 'direct', or 'beta'.")
  }

  # standardize colnames
  if (standardize.colnames) {
    ped <- standardizeColnames(ped, verbose = verbose)
  }

  # Load final result if computation was completed
  if (resume && file.exists(checkpoint_files$final_matrix)) {
    if (verbose) cat("Loading final computed matrix...\n")
    return(readRDS(checkpoint_files$final_matrix))
  }


  #------
  # Algorithm
  #------

  # Get the number of rows in the pedigree dataset, representing the size of the family
  nr <- nrow(ped)

  # Print the family size if verbose is TRUE
  if (verbose) {
    cat(paste0("Family Size = ", nr, "\n"))
  }

  # Step 1: Construct parent-child adjacency matrix
  ## A. Resume from Checkpoint if Needed
  if (resume && file.exists(checkpoint_files$parList) && file.exists(checkpoint_files$lens)) {
    if (verbose) cat("Resuming: Loading parent-child adjacency data...\n")
    parList <- readRDS(checkpoint_files$parList)
    lens <- readRDS(checkpoint_files$lens)
    computed_indices <- which(!sapply(parList, is.null))
    lastComputed <- if (length(computed_indices) > 0) max(computed_indices) else 0
    if (verbose) cat("Resuming from iteration", lastComputed + 1, "\n")
  } else {
    ## Initialize variables
    parList <- vector("list", nr)
    lens <- integer(nr)
    lastComputed <- 0

    if (verbose) cat("Building parent adjacency matrix...\n")
  }


  ## B. Resume loop from the next uncomputed index

  if (verbose) cat("Computing parent-child adjacency matrix...\n")
  # Construct sparse matrix
  if (resume && file.exists(checkpoint_files$iss) && file.exists(checkpoint_files$jss)) { # fix to check actual
    if (verbose) cat("Resuming: Constructed matrix...\n")
    jss <- readRDS(checkpoint_files$jss)
    iss <- readRDS(checkpoint_files$iss)
    list_of_adjacencies <- list(iss = iss, jss = jss)
  } else {
    list_of_adjacencies <- computeParentAdjacency(
      ped = ped,
      save_rate_parlist = save_rate_parlist,
      checkpoint_files = checkpoint_files,
      component = component,
      adjacency_method = adjacency_method, # adjacency_method,
      saveable = saveable,
      resume = resume,
      save_path = save_path,
      update_rate = update_rate,
      verbose = verbose,
      lastComputed = lastComputed,
      nr = nr,
      parList = parList,
      lens = lens,
      adjBeta_method = adjBeta_method
    )

    # Construct sparse matrix
    iss <- list_of_adjacencies$iss
    jss <- list_of_adjacencies$jss

    if (verbose) {
      cat("Constructed sparse matrix\n")
    }
    if (saveable) {
      saveRDS(jss, file = checkpoint_files$jss)
      saveRDS(iss, file = checkpoint_files$iss)
    }
    # Garbage collection if gc is TRUE
    if (gc) {
      rm(parList, lens, list_of_adjacencies)
      gc()
    }
  }
  # Set parent values depending on the component type
  if (component %in% c("generation", "additive")) {
    parVal <- .5
  } else if (component %in% c("common nuclear", "mitochondrial")) {
    parVal <- 1
  } else {
    stop("Don't know how to set parental value")
  }
  # Construct sparse matrix
  if (resume && file.exists(checkpoint_files$isPar)) {
    if (verbose) cat("Resuming: Loading adjacency matrix...\n")
    isPar <- readRDS(checkpoint_files$isPar)
  } else {
    # Initialize adjacency matrix for parent-child relationships
    isPar <- Matrix::sparseMatrix(
      i = iss,
      j = jss,
      x = parVal,
      dims = c(nr, nr),
      dimnames = list(ped$ID, ped$ID)
    )
    if (verbose) {
      cat("Completed first degree relatives (adjacency)\n")
    }
    if (saveable) {
      saveRDS(isPar, file = checkpoint_files$isPar)
    }
  }

  # isPar is the adjacency matrix.  'A' matrix from RAM
  if (component %in% c("common nuclear")) {
    Matrix::diag(isPar) <- 1
    if (sparse == FALSE) {
      isPar <- as.matrix(isPar)
    }
    return(isPar)
  }

  if (resume && file.exists(checkpoint_files$isChild)) {
    if (verbose) cat("Resuming: Loading isChild matrix...\n")
    isChild <- readRDS(checkpoint_files$isChild)
  } else {
    # isChild is the 'S' matrix from RAM

    isChild <- isChild(isChild_method = isChild_method, ped = ped)

    if (saveable) {
      saveRDS(isChild, file = checkpoint_files$isChild)
    }
  }
  # --- Step 2: Compute Relatedness Matrix ---
  if (resume && file.exists(checkpoint_files$r_checkpoint) && file.exists(checkpoint_files$gen_checkpoint) && file.exists(checkpoint_files$mtSum_checkpoint) && file.exists(checkpoint_files$newIsPar_checkpoint) &&
    file.exists(checkpoint_files$count_checkpoint)
  ) {
    if (verbose) cat("Resuming: Loading previous computation...\n")
    r <- readRDS(checkpoint_files$r_checkpoint)
    gen <- readRDS(checkpoint_files$gen_checkpoint)
    mtSum <- readRDS(checkpoint_files$mtSum_checkpoint)
    newIsPar <- readRDS(checkpoint_files$newIsPar_checkpoint)
    count <- readRDS(checkpoint_files$count_checkpoint)
  } else {
    r <- Matrix::Diagonal(x = 1, n = nr)
    gen <- rep(1, nr)
    mtSum <- sum(r, na.rm = TRUE)
    newIsPar <- isPar
    count <- 0
  }
  maxCount <- max.gen + 1
  if (verbose) {
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
    if (verbose) {
      cat(paste0("Completed ", count - 1, " degree relatives\n"))
    }
    # Save progress every save_rate iterations
    if (saveable && (count %% save_rate_gen == 0)) {
      saveRDS(r, file = checkpoint_files$r_checkpoint)
      saveRDS(gen, file = checkpoint_files$gen_checkpoint)
      saveRDS(newIsPar, file = checkpoint_files$newIsPar_checkpoint)
      saveRDS(mtSum, file = checkpoint_files$mtSum_checkpoint)
      saveRDS(count, file = checkpoint_files$count_checkpoint)
    }
  }
  # compute rsq <- r %*% sqrt(diag(isChild))
  # compute rel <- tcrossprod(rsq)
  if (gc) {
    rm(isPar, newIsPar)
    gc()
  }

  if (component == "generation") { # no need to do the rest
    return(gen)
  } else {
    if (verbose) {
      cat("Completed RAM path tracing\n")
    }
  }

  # --- Step 3: I-A inverse times diagonal multiplication ---
  if (resume && file.exists(checkpoint_files$r2_checkpoint)) {
    if (verbose) cat("Resuming: Loading I-A inverse...\n")
    r2 <- readRDS(checkpoint_files$r2_checkpoint)
  } else {
    if (verbose) {
      cat("Doing I-A inverse times diagonal multiplication\n")
    }
    r2 <- r %*% Matrix::Diagonal(x = sqrt(isChild), n = nr)
    if (gc) {
      rm(r, isChild)
      gc()
    }
    if (saveable) {
      saveRDS(r2, file = checkpoint_files$r2_checkpoint)
    }
  }

  # --- Step 4: T crossproduct  ---

  if (resume && file.exists(checkpoint_files$tcrossprod_checkpoint) && component != "generation") {
    if (verbose) cat("Resuming: Loading tcrossprod...\n")
    r <- readRDS(checkpoint_files$tcrossprod_checkpoint)
  } else {
    r <- .computeTranspose(r2 = r2, transpose_method = transpose_method, verbose = verbose)
    if (saveable) {
      saveRDS(r, file = checkpoint_files$tcrossprod_checkpoint)
    }
  }


  if (component == "mitochondrial") {
    r@x <- rep(1, length(r@x))
    # Assign 1 to all nonzero elements for mitochondrial component
  }

  if (sparse == FALSE) {
    r <- as.matrix(r)
  }
  if (flatten.diag) { # flattens diagonal if you don't want to deal with inbreeding
    diag(r) <- 1
  }
  if (saveable) {
    saveRDS(r, file = checkpoint_files$final_matrix)
  }
  return(r)
}

#' Take a pedigree and turn it into an additive genetics relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2add <- function(ped, max.gen = 25, sparse = TRUE, verbose = FALSE,
                    gc = FALSE,
                    flatten.diag = FALSE, standardize.colnames = TRUE,
                    transpose_method = "tcrossprod",
                    adjacency_method = "direct",
                    saveable = FALSE,
                    resume = FALSE,
                    save_rate = 5,
                    save_rate_gen = save_rate,
                    save_rate_parlist = 1000 * save_rate,
                    save_path = "checkpoint/",
                    ...) {
  ped2com(
    ped = ped,
    max.gen = max.gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "additive",
    flatten.diag = flatten.diag,
    standardize.colnames = standardize.colnames,
    transpose_method = transpose_method,
    adjacency_method = adjacency_method,
    saveable = saveable,
    resume = resume,
    save_rate_gen = save_rate_gen,
    save_rate_parlist = save_rate_parlist,
    save_path = save_path
  )
}

#' Take a pedigree and turn it into a mitochondrial relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#' @aliases ped2mt
#'
ped2mit <- ped2mt <- function(ped, max.gen = 25,
                              sparse = TRUE,
                              verbose = FALSE, gc = FALSE,
                              flatten.diag = FALSE,
                              standardize.colnames = TRUE,
                              transpose_method = "tcrossprod",
                              adjacency_method = "direct",
                              saveable = FALSE,
                              resume = FALSE,
                              save_rate = 5,
                              save_rate_gen = save_rate,
                              save_rate_parlist = 1000 * save_rate,
                              save_path = "checkpoint/",
                              ...) {
  ped2com(
    ped = ped,
    max.gen = max.gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "mitochondrial",
    flatten.diag = flatten.diag,
    standardize.colnames = standardize.colnames,
    transpose_method = transpose_method,
    adjacency_method = adjacency_method,
    saveable = saveable,
    resume = resume,
    save_rate_gen = save_rate_gen,
    save_rate_parlist = save_rate_parlist,
    save_path = save_path,
    ...
  )
}

#' Take a pedigree and turn it into a common nuclear environmental relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2cn <- function(ped, max.gen = 25, sparse = TRUE, verbose = FALSE,
                   gc = FALSE, flatten.diag = FALSE,
                   standardize.colnames = TRUE,
                   transpose_method = "tcrossprod",
                   saveable = FALSE,
                   resume = FALSE,
                   save_rate = 5,
                   adjacency_method = "direct",
                   save_rate_gen = save_rate,
                   save_rate_parlist = 1000 * save_rate,
                   save_path = "checkpoint/",
                   ...) {
  ped2com(
    ped = ped,
    max.gen = max.gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "common nuclear",
    adjacency_method = adjacency_method,
    flatten.diag = flatten.diag,
    standardize.colnames = standardize.colnames,
    transpose_method = transpose_method,
    saveable = saveable,
    resume = resume,
    save_rate_gen = save_rate_gen,
    save_rate_parlist = save_rate_parlist,
    save_path = save_path,
    ...
  )
}
#' Take a pedigree and turn it into an extended environmental relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2ce <- function(ped,
                   ...) {
  matrix(1, nrow = nrow(ped), ncol = nrow(ped), dimnames = list(ped$ID, ped$ID))
}


#' Compute the transpose multiplication for the relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @param r2 a relatedness matrix
#'
.computeTranspose <- function(r2, transpose_method = "tcrossprod", verbose = FALSE) {
  valid_methods <- c("tcrossprod", "crossprod", "star",
                     "tcross.alt.crossprod", "tcross.alt.star")
  if (!transpose_method %in% valid_methods) {
    stop("Invalid method specified. Choose from 'tcrossprod', 'crossprod', 'star', 'tcross.alt.crossprod', or 'tcross.alt.star'.")
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
                     if (verbose) cat("Doing tcrossprod\n")
                     Matrix::tcrossprod(r2)
                   },
                   "crossprod" = {
                     if (verbose) cat("Doing tcrossprod using crossprod(t(.))\n")
                     crossprod(t(as.matrix(r2)))
                   },
                   "star" = {
                     if (verbose) cat("Doing tcrossprod using %*% t(.)\n")
                     r2 %*% t(as.matrix(r2))
                   }
  )

  return(result)
}
