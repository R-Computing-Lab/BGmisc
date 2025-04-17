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
    list_of_adjacencies <- compute_parent_adjacency(
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
  if (!transpose_method %in% c("tcrossprod", "crossprod", "star", "tcross.alt.crossprod", "tcross.alt.star")) {
    stop("Invalid method specified. Choose from 'tcrossprod', 'crossprod', or 'star'.")
  }
  if (transpose_method %in% c("crossprod", "tcross.alt.crossprod")) {
    if (verbose) cat("Doing alt tcrossprod crossprod t \n")
    return(crossprod(t(as.matrix(r2))))
  } else if (transpose_method %in% c("star", "tcross.alt.star")) {
    if (verbose) cat("Doing alt tcrossprod %*% t \n")
    return(r2 %*% t(as.matrix(r2)))
  } else {
    if (verbose) cat("Doing tcrossprod\n")
    return(Matrix::tcrossprod(r2))
  }
}

.adjLoop <- function(ped, component, saveable, resume,
                     save_path, verbose, lastComputed,
                     nr, checkpoint_files, update_rate,
                     parList, lens, save_rate_parlist,
                     ...) {
  # Loop through each individual in the pedigree
  # Build the adjacency matrix for parent-child relationships
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
      val <- (as.numeric(x["ID"]) == ped$momID)
      val[is.na(val)] <- FALSE
    } else {
      stop("Unknown relatedness component requested")
    }
    # Storing the indices of the parent-child relationships
    # Keep track of indices only, and then initialize a single sparse matrix
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
  jss <- rep(1L:nr, times = lens)
  iss <- unlist(parList)
  list_of_adjacency <- list(iss = iss, jss = jss)
  return(list_of_adjacency)
}

.adjIndexed <- function(ped, component, saveable, resume,
                        save_path, verbose, lastComputed,
                        nr, checkpoint_files, update_rate,
                        parList, lens, save_rate_parlist) {
  # Loop through each individual in the pedigree
  # Build the adjacency matrix for parent-child relationships
  # Is person in column j the parent of the person in row i? .5 for yes, 0 for no.

  # Convert IDs
  ped$ID <- as.numeric(ped$ID)
  ped$momID <- as.numeric(ped$momID)
  ped$dadID <- as.numeric(ped$dadID)

  # parent-child lookup
  mom_index <- match(ped$momID, ped$ID, nomatch = 0)
  dad_index <- match(ped$dadID, ped$ID, nomatch = 0)

  for (i in (lastComputed + 1):nr) {
    if (component %in% c("generation", "additive")) {
      sMom <- (mom_index == i)
      sDad <- (dad_index == i)
      val <- sMom | sDad
    } else if (component %in% c("common nuclear")) {
      # Code for 'common nuclear' component
      # IDs have the Same mom and Same dad
      sMom <- (ped$momID[i] == ped$momID)
      sMom[is.na(sMom)] <- FALSE
      sDad <- (ped$dadID[i] == ped$dadID)
      sDad[is.na(sDad)] <- FALSE
      val <- sMom & sDad
    } else if (component %in% c("mitochondrial")) {
      val <- (mom_index == i)
    } else {
      stop("Unknown relatedness component requested")
    }

    val[is.na(val)] <- FALSE
    parList[[i]] <- which(val)
    lens[i] <- length(parList[[i]])

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
  jss <- rep(1L:nr, times = lens)
  iss <- unlist(parList)
  list_of_adjacency <- list(iss = iss, jss = jss)
  return(list_of_adjacency)
}

.adjDirect <- function(ped, component, saveable, resume,
                       save_path, verbose, lastComputed,
                       nr, checkpoint_files, update_rate,
                       parList, lens, save_rate_parlist, adjBeta_method,
                       ...) {
  # Loop through each individual in the pedigree
  # Build the adjacency matrix for parent-child relationships
  # Is person in column j the parent of the person in row i? .5 for yes, 0 for no.
  uniID <- ped$ID # live dangerously without sort(unique(ped$ID))
  ped$ID <- as.numeric(factor(ped$ID, levels = uniID))
  ped$momID <- as.numeric(factor(ped$momID, levels = uniID))
  ped$dadID <- as.numeric(factor(ped$dadID, levels = uniID))

  if (component %in% c("generation", "additive")) {
    mIDs <- stats::na.omit(data.frame(rID = ped$ID, cID = ped$momID))
    dIDs <- stats::na.omit(data.frame(rID = ped$ID, cID = ped$dadID))
    iss <- c(mIDs$rID, dIDs$rID)
    jss <- c(mIDs$cID, dIDs$cID)
  } else if (component %in% c("common nuclear")) {
    #  message("Common Nuclear component is not yet implemented for direct method.  Using index method.\n")

    # 1) Create a logical mask for only known parents
    mask <- !is.na(ped$momID) & !is.na(ped$dadID)

    # 2) Create a single hash label for each known (momID, dadID) pair
    base <- max(ped$ID, na.rm = TRUE) + 1L
    pairCode <- ped$momID[mask] + base * ped$dadID[mask]

    # 3) Factor that label => each row with the same (mom,dad) gets the same integer code
    childVec <- which(mask)

    # 4) Group children by pairCode, so each group is "all children with that (mom,dad)"
    groupList <- split(childVec, pairCode)

    # 5) For each group with >1 children, form pairwise adjacency (i->j) for i != j
    iss_list <- vector("list", length(groupList))
    jss_list <- vector("list", length(groupList))
    counter <- 1

    for (g in groupList) {
      k <- length(g)
      if (k > 1) {
        # We'll form all k^2 combos, then remove the diagonal i=j
        # rep() calls faster than expand.grid

        # v = each child repeated k times
        # w = entire group repeated once for each child
        v <- rep(g, each = k) # row index
        w <- rep(g, times = k) # col index

        keep <- (v != w) # remove diagonal where v == w
        iss_list[[counter]] <- v[keep]
        jss_list[[counter]] <- w[keep]
        counter <- counter + 1
      }
    }


    iss <- unlist(iss_list, use.names = FALSE)
    jss <- unlist(jss_list, use.names = FALSE)

    #   list_of_adjacency <-    .adjBeta(ped=ped,adjBeta_method=adjBeta_method,
    #                                      component = component,
    #                                     saveable = saveable, resume = resume,
    #                                     save_path = save_path, verbose = verbose,
    #                                     lastComputed = lastComputed, nr = nr,
    #                                     checkpoint_files = checkpoint_files,
    #                                     update_rate = update_rate,
    #                                     parList = parList,
    #                                     lens = lens, save_rate_parlist = save_rate_parlist,
    #                                     ...)

    #   return(list_of_adjacency)
  } else if (component %in% c("mitochondrial")) {
    mIDs <- stats::na.omit(data.frame(rID = ped$ID, cID = ped$momID))
    iss <- c(mIDs$rID)
    jss <- c(mIDs$cID)
  } else {
    stop("Unknown relatedness component requested")
  }
  list_of_adjacency <- list(
    iss = iss,
    jss = jss
  )
  return(list_of_adjacency)
}

#' Compute Parent Adjacency Matrix with Multiple Approaches
#' @inheritParams ped2com
#' @inherit ped2com details
#' @param nr the number of rows in the pedigree dataset
#' @param lastComputed the last computed index
#' @param parList a list of parent-child relationships
#' @param lens a vector of the lengths of the parent-child relationships
#' @param checkpoint_files a list of checkpoint files

compute_parent_adjacency <- function(ped, component,
                                     adjacency_method = "direct",
                                     saveable, resume,
                                     save_path, verbose = FALSE,
                                     lastComputed = 0, nr, checkpoint_files, update_rate,
                                     parList, lens, save_rate_parlist, adjBeta_method = NULL,
                                     ...) {
  if (adjacency_method == "loop") {
    if (lastComputed < nr) { # Original version
      list_of_adjacency <- .adjLoop(
        ped = ped,
        component = component,
        saveable = saveable,
        resume = resume,
        save_path = save_path,
        verbose = verbose,
        lastComputed = lastComputed,
        nr = nr,
        checkpoint_files = checkpoint_files,
        update_rate = update_rate,
        parList = parList,
        lens = lens,
        save_rate_parlist = save_rate_parlist,
        ...
      )
    }
  } else if (adjacency_method == "indexed") { # Garrison version
    if (lastComputed < nr) {
      list_of_adjacency <- .adjIndexed(
        ped = ped,
        component = component,
        saveable = saveable,
        resume = resume,
        save_path = save_path,
        verbose = verbose,
        lastComputed = lastComputed,
        nr = nr,
        checkpoint_files = checkpoint_files,
        update_rate = update_rate,
        parList = parList,
        lens = lens,
        save_rate_parlist = save_rate_parlist,
        ...
      )
    }
  } else if (adjacency_method == "direct") { # Hunter version
    if (lastComputed < nr) {
      list_of_adjacency <- .adjDirect(
        ped = ped,
        component = component,
        saveable = saveable,
        resume = resume,
        save_path = save_path,
        verbose = verbose,
        lastComputed = lastComputed,
        nr = nr,
        checkpoint_files = checkpoint_files,
        update_rate = update_rate,
        parList = parList,
        lens = lens,
        save_rate_parlist = save_rate_parlist,
        ...
      )
    }
  } else if (adjacency_method == "beta") {
    list_of_adjacency <- .adjBeta(
      ped = ped,
      adjBeta_method = adjBeta_method,
      component = component,
      saveable = saveable,
      resume = resume,
      save_path = save_path,
      verbose = verbose,
      lastComputed = lastComputed,
      nr = nr,
      checkpoint_files = checkpoint_files,
      update_rate = update_rate,
      parList = parList,
      lens = lens,
      save_rate_parlist = save_rate_parlist,
      ...
    )
  } else {
    stop("Invalid method specified. Choose from 'loop', 'direct', 'indexed', or beta")
  }
  if (saveable) {
    saveRDS(parList, file = checkpoint_files$parList)
    saveRDS(lens, file = checkpoint_files$lens)
    if (verbose) {
      cat("Final checkpoint saved for adjacency matrix.\n")
    }
  }
  return(list_of_adjacency)
}


#' Determine isChild Status, isChild is the 'S' matrix from RAM
#' @param isChild_method method to determine isChild status
#' @param ped pedigree data frame
#' @return isChild 'S' matrix
#'

isChild <- function(isChild_method, ped) {
  if (isChild_method == "partialparent") {
    isChild <- apply(ped[, c("momID", "dadID")], 1, function(x) {
      .5 + .25 * sum(is.na(x)) # 2 parents -> .5, 1 parent -> .75, 0 parents -> 1
    })
  } else {
    isChild <- apply(ped[, c("momID", "dadID")], 1, function(x) {
      2^(-!all(is.na(x)))
    })
  }
}


.adjBeta <- function(ped, component,
                     adjBeta_method = 5,
                     parList = NULL,
                     lastComputed = 0,
                     nr = NULL,
                     lens = NULL,
                     saveable = FALSE,
                     resume = FALSE,
                     save_path = NULL,
                     verbose = FALSE,
                     save_rate_parlist = NULL,
                     update_rate = NULL,
                     checkpoint_files = NULL,
                     ...) { # 1) Pairwise compare mother IDs
  if (adjBeta_method == 1) {
    # gets slow when data are bigger. much slower than indexed
    momMatch <- outer(ped$momID, ped$momID, FUN = "==")
    momMatch[is.na(momMatch)] <- FALSE

    # 2) Pairwise compare father IDs
    dadMatch <- outer(ped$dadID, ped$dadID, FUN = "==")
    dadMatch[is.na(dadMatch)] <- FALSE

    # 3) Sibling adjacency if both mom & dad match
    adj <- momMatch & dadMatch

    # 4) Extract indices where adj[i,j] is TRUE
    w <- which(adj, arr.ind = TRUE)
    # iss <- w[, 1]
    #  jss <- w[, 2]
    #
    list_of_adjacency <- list(
      iss = w[, 1],
      jss = w[, 2]
    )
  } else if (adjBeta_method == 2) {
    # 1) Create a logical mask for known parents
    mask <- !is.na(ped$momID) & !is.na(ped$dadID)

    # 2) Create a single string label for each known (momID, dadID) pair
    pairLabel <- paste0(ped$momID[mask], "_", ped$dadID[mask])

    # 3) Factor that label => each row with the same (mom,dad) gets the same integer code
    #    This is "creating a new ID" for each unique parent pair
    pairCode <- match(pairLabel, unique(pairLabel))

    # childVec are the row indices in 'ped' that have known parents
    childVec <- which(mask) # length(childVec) = sum(mask)

    # 4) Group children by pairCode, so each group is "all children with that (mom,dad)"
    groupList <- split(childVec, pairCode)

    # 5) For each group with >1 children, form pairwise adjacency i->j
    iss_list <- list()
    jss_list <- list()
    counter <- 1

    for (g in groupList) {
      if (length(g) > 1) {
        combos <- expand.grid(g, g, KEEP.OUT.ATTRS = FALSE)
        combos <- combos[combos[, 1] != combos[, 2], , drop = FALSE]
        iss_list[[counter]] <- combos[, 1]
        jss_list[[counter]] <- combos[, 2]
        counter <- counter + 1
      }
    }
    # iss <- unlist(iss_list, use.names = FALSE)
    #  jss <- unlist(jss_list, use.names = FALSE)

    list_of_adjacency <- list(
      iss = unlist(iss_list, use.names = FALSE),
      jss = unlist(jss_list, use.names = FALSE)
    )
  } else if (adjBeta_method == 3) {
    nr <- nrow(ped)
    # terrible
    # Define a scalar-checking function:
    f_check <- function(i, j) {
      # i, j are each single integers
      # Return one boolean: do they share both parents?
      !is.na(ped$momID[i]) && !is.na(ped$dadID[i]) &&
        !is.na(ped$momID[j]) && !is.na(ped$dadID[j]) &&
        (ped$momID[i] == ped$momID[j]) &&
        (ped$dadID[i] == ped$dadID[j])
    }

    # Vectorize it so outer() will produce an nr x nr matrix
    vf_check <- Vectorize(f_check)

    # Now outer() calls vf_check(...) in a way that yields scalar results
    adj <- outer(seq_len(nr), seq_len(nr), FUN = vf_check)

    # Extract which cells of adj are TRUE
    w <- which(adj, arr.ind = TRUE)
    #  iss <- w[, 1]
    #   jss <- w[, 2]

    list_of_adjacency <- list(
      iss = iss <- w[, 1],
      jss = jss <- w[, 2]
    )
  } else if (adjBeta_method == 4) {
    # 1) Create a logical mask for known parents
    mask <- !is.na(ped$momID) & !is.na(ped$dadID)

    # 2) Create a single string label for each known (momID, dadID) pair
    pairLabel <- paste0(ped$momID[mask], "_", ped$dadID[mask])

    # 3) Factor that label => each row with the same (mom,dad) gets the same integer code
    pairCode <- match(pairLabel, unique(pairLabel))

    # childVec are the row indices in 'ped' that have known parents
    childVec <- which(mask)

    # 4) Group children by pairCode, so each group is "all children with that (mom,dad)"
    groupList <- split(childVec, pairCode)

    # 5) For each group with >1 children, form pairwise adjacency (i->j) for i != j
    iss_list <- vector("list", length(groupList))
    jss_list <- vector("list", length(groupList))
    counter <- 1

    for (g in groupList) {
      k <- length(g)
      if (k > 1) {
        # We'll form all k^2 combos, then remove the diagonal i=j
        # Instead of expand.grid, do rep() calls:

        # v = each child repeated k times
        # w = entire group repeated once for each child
        v <- rep(g, each = k) # row index
        w <- rep(g, times = k) # col index

        keep <- (v != w) # remove diagonal where v == w
        iss_list[[counter]] <- v[keep]
        jss_list[[counter]] <- w[keep]
        counter <- counter + 1
      }
    }

    list_of_adjacency <- list(
      iss = unlist(iss_list, use.names = FALSE),
      jss = unlist(jss_list, use.names = FALSE)
    )
  } else if (adjBeta_method == 5) {
    # 1) Create a logical mask for known parents
    mask <- !is.na(ped$momID) & !is.na(ped$dadID)

    # 2) Create a single hash label for each known (momID, dadID) pair
    # pairLabel <- paste0(ped$momID[mask], "_", ped$dadID[mask])
    base <- max(ped$ID, na.rm = TRUE) + 1L
    pairCode <- ped$momID[mask] + base * ped$dadID[mask]

    # 3) Factor that label => each row with the same (mom,dad) gets the same integer code
    childVec <- which(mask)

    # 4) Group children by pairCode, so each group is "all children with that (mom,dad)"
    groupList <- split(childVec, pairCode)

    # 5) For each group with >1 children, form pairwise adjacency (i->j) for i != j
    iss_list <- vector("list", length(groupList))
    jss_list <- vector("list", length(groupList))
    counter <- 1

    for (g in groupList) {
      k <- length(g)
      if (k > 1) {
        # We'll form all k^2 combos, then remove the diagonal i=j
        # Instead of expand.grid, do rep() calls:

        # v = each child repeated k times
        # w = entire group repeated once for each child
        v <- rep(g, each = k) # row index
        w <- rep(g, times = k) # col index

        keep <- (v != w) # remove diagonal where v == w
        iss_list[[counter]] <- v[keep]
        jss_list[[counter]] <- w[keep]
        counter <- counter + 1
      }
    }

    list_of_adjacency <- list(
      iss = unlist(iss_list, use.names = FALSE),
      jss = unlist(jss_list, use.names = FALSE)
    )
  } else {
    list_of_adjacency <- .adjIndexed(
      ped = ped, component = component,
      saveable = saveable, resume = resume,
      save_path = save_path, verbose = verbose,
      lastComputed = lastComputed, nr = nr,
      checkpoint_files = checkpoint_files,
      update_rate = update_rate, parList = parList,
      lens = lens, save_rate_parlist = save_rate_parlist
    )
  }
  return(list_of_adjacency)
}
