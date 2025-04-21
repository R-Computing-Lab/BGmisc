
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





#' Compute Parent Adjacency Matrix with Multiple Approaches
#' @inheritParams ped2com
#' @inherit ped2com details
#' @param nr the number of rows in the pedigree dataset
#' @param lastComputed the last computed index
#' @param parList a list of parent-child relationships
#' @param lens a vector of the lengths of the parent-child relationships
#' @param checkpoint_files a list of checkpoint files
#' @param update_rate the rate at which to update the progress
#'
#' @export
computeParentAdjacency <- function(ped, component,
                                     adjacency_method = "direct",
                                     saveable, resume,
                                     save_path,
                                   verbose = FALSE,
                                     lastComputed = 0, nr,
                                   checkpoint_files,
                                   update_rate,
                                     parList, lens, save_rate_parlist,
                                   adjBeta_method = NULL,
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
