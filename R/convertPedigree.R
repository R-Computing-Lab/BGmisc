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
#' @param tcross.alt.crossprod logical. If TRUE, use alternative method of using Crossprod function for computing the transpose
#' @param tcross.alt.star logical. If TRUE, use alternative method of using \%\*\% for computing the transpose
#' @param ... additional arguments to be passed to \code{\link{ped2com}}
#' @details The algorithms and methodologies used in this function are further discussed and exemplified in the vignette titled "examplePedigreeFunctions". For more advanced scenarios and detailed explanations, consult this vignette.
#' @export
#'
ped2com <- function(ped, component,
                    max.gen = 25,
                    sparse = FALSE,
                    verbose = FALSE,
                    gc = FALSE,
                    flatten.diag = FALSE,
                    standardize.colnames = TRUE,
                    tcross.alt.crossprod = FALSE,
                    tcross.alt.star = FALSE,
                    saveable = FALSE,
                    resume = FALSE,
                    save_rate = 5,
                    save_rate_gen = save_rate,
                    save_rate_parlist = 1000*save_rate,
                    update_rate = 100,
                    save_path = "checkpoint/",
                    ...) {
  #------
  # Checkpointing
  #------
  if (saveable | resume) { # prepare checkpointing
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

  # standardize colnames
  if (standardize.colnames) {
    ped <- standardizeColnames(ped)
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
  }

  # Resume loop from the next uncomputed index
  if (lastComputed < nr) {
    # Loop through each individual in the pedigree build the adjacency matrix for parent-child relationships
    # Is person in column j the parent of the person in row i? .5 for yes, 0 for no.

    ped$momID <- as.numeric(ped$momID)
    ped$dadID <- as.numeric(ped$dadID)
    ped$ID <- as.numeric(ped$ID)

    for (i in (lastComputed + 1):nr) {
      x <- ped[i, , drop = FALSE]

      # Handle parentage according to the 'component' specified
      if (component %in% c("generation", "additive")) {
        # Code for 'generation' and 'additive' components
        # Checks if is mom of ID or is dad of ID
        # do once
        # xID <- as.numeric(x["ID"])
        # sMom <- (xID == as.numeric(ped$momID))
        # sDad <- (xID == as.numeric(ped$dadID))
        # val <- sMom | sDad
        # val[is.na(val)] <- FALSE
        # reduces computations
        xID <- as.numeric(x["ID"])
        sMom <- (xID == ped$momID)
        sDad <- (xID == ped$dadID)
        val <- sMom | sDad
        val[is.na(val)] <- FALSE
      } else if (component %in% c("common nuclear")) {
        # Code for 'common nuclear' component
        # IDs have the Same mom and Same dad
        sMom <- (as.numeric(x["momID"]) == ped$momID)
        sMom[is.na(sMom)] <- FALSE
        sDad <- (as.numeric(x["dadID"]) == ped$dadID)
        sDad[is.na(sDad)] <- FALSE
        val <- sMom & sDad
      } else if (component %in% c("mitochondrial")) {
        # Code for 'mitochondrial' component
        #  sMom <- (as.numeric(x["ID"]) == as.numeric(ped$momID))
        #  sDad <- TRUE
        # val <- sMom & sDad
        # val[is.na(val)] <- FALSE

        # reduces computations
        val <- (as.numeric(x["ID"]) == ped$momID)
        val[is.na(val)] <- FALSE
      } else {
        stop("Unknown relatedness component requested")
      }
      # Storing the indices of the parent-child relationships
      # keep track of indices only, and then initialize a single sparse matrix
      wv <- which(val)
      parList[[i]] <- wv
      lens[i] <- length(wv)
      # Print progress if verbose is TRUE
      if (verbose && (i %% update_rate == 0)) {
        cat(paste0("Done with ", i, " of ", nr, "\n"))
      }
      # Checkpointing every save_rate iterations
      if (saveable && (i %% save_rate_parlist == 0)) {
        saveRDS(parList, file = checkpoint_files$parList)
        saveRDS(lens, file = checkpoint_files$lens)
        if (verbose) cat("Checkpointed parlist saved at iteration", i, "\n")
      }
    }
    if (saveable) {
      saveRDS(parList, file = checkpoint_files$parList)
      saveRDS(lens, file = checkpoint_files$lens)
      if (verbose) cat("parList saved\n")
    }
  }

  # Construct sparse matrix
  if (resume && file.exists(checkpoint_files$isPar)) { # fix to check actual
    if (verbose) cat("Resuming: Constructed matrix...\n")
    jss <- readRDS(checkpoint_files$jss)
    iss <- readRDS(checkpoint_files$iss)
  } else {
    # Construct sparse matrix
    jss <- rep(1L:nr, times = lens)
    iss <- unlist(parList)

    if (verbose) {
      cat("Constructed sparse matrix\n")
    }
    if (saveable) {
      saveRDS(jss, file = checkpoint_files$jss)
      saveRDS(iss, file = checkpoint_files$iss)
    }
    # Garbage collection if gc is TRUE
    if (gc) {
      rm(parList, lens)
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
    if (!sparse) {
      isPar <- as.matrix(isPar)
    }
    return(isPar)
  }

  if (resume && file.exists(checkpoint_files$isChild)) {
    if (verbose) cat("Resuming: Loading isChild matrix...\n")
    isChild <- readRDS(checkpoint_files$isChild)
  } else {
    # isChild is the 'S' matrix from RAM
    isChild <- apply(ped[, c("momID", "dadID")], 1, function(x) {
      2^(-!all(is.na(x)))
    })
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

  while (mtSum != 0 & count < maxCount) {
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
  # --- Step 3: I-A inverse times diagonal multiplication ---
  if (resume && file.exists(checkpoint_files$final_matrix)) {
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
    if (tcross.alt.crossprod) {
      if (verbose) {
        cat("Doing alt tcrossprod crossprod t \n")
      }
      r <- crossprod(t(as.matrix(r2)))
    } else if (tcross.alt.star) {
      if (verbose) {
        cat("Doing alt tcrossprod %*% t \n")
      }
      r <- r2 %*% t(as.matrix(r2))
    } else {
      if (verbose) {
        cat("Doing tcrossprod\n")
      }
      # failed here
      r <- Matrix::tcrossprod(r2)
    }
    if (saveable) {
      saveRDS(r, file = checkpoint_files$tcrossprod_checkpoint)
    }
  }

  if (component == "generation") {
    return(gen)
  } else {
    if (component == "mitochondrial") {
      r@x <- rep(1, length(r@x))
      # Assign 1 to all nonzero elements for mitochondrial component
    }

    if (!sparse) {
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
}

#' Take a pedigree and turn it into an additive genetics relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2add <- function(ped, max.gen = 25, sparse = FALSE, verbose = FALSE,
                    gc = FALSE,
                    flatten.diag = FALSE, standardize.colnames = TRUE,
                    tcross.alt.crossprod = FALSE, tcross.alt.star = FALSE,
                    saveable = FALSE,
                    resume = FALSE,
                    save_rate = 5,
                    save_rate_gen = save_rate,
                    save_rate_parlist = 1000*save_rate,
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
    tcross.alt.crossprod = tcross.alt.crossprod,
    tcross.alt.star = tcross.alt.star,
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
                              sparse = FALSE,
                              verbose = FALSE, gc = FALSE,
                              flatten.diag = FALSE,
                              standardize.colnames = TRUE,
                              tcross.alt.crossprod = FALSE, tcross.alt.star = FALSE,
                              saveable = FALSE,
                              resume = FALSE,
                              save_rate = 5,
                              save_rate_gen = save_rate_gen,
                              save_rate_parlist = 1000*save_rate,
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
    tcross.alt.crossprod = tcross.alt.crossprod,
    tcross.alt.star = tcross.alt.star,
    saveable = saveable,
    resume = resume,
    save_rate_gen = save_rate_gen,
    save_rate_parlist = save_rate_parlist,
    save_path = save_path
  )
}

#' Take a pedigree and turn it into a common nuclear environmental relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2cn <- function(ped, max.gen = 25, sparse = FALSE, verbose = FALSE,
                   gc = FALSE, flatten.diag = FALSE,
                   standardize.colnames = TRUE,
                   tcross.alt.crossprod = FALSE, tcross.alt.star = FALSE,
                   saveable = FALSE,
                   resume = FALSE,
                   save_rate = 5,
                   save_rate_gen = save_rate,
                   save_rate_parlist = 1000*save_rate,
                   save_path = "checkpoint/",
                   ...) {
  ped2com(
    ped = ped,
    max.gen = max.gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "common nuclear",
    flatten.diag = flatten.diag,
    standardize.colnames = standardize.colnames,
    tcross.alt.crossprod = tcross.alt.crossprod,
    tcross.alt.star = tcross.alt.star,
    saveable = saveable,
    resume = resume,
    save_rate_gen = save_rate_gen,
    save_rate_parlist = save_rate_parlist,
    save_path = save_path
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
