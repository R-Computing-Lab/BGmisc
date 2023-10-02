#' Take a pedigree and turn it into a relatedness matrix
#' @param ped a pedigree dataset.  Needs ID, momID, and dadID columns
#' @param component character.  Which component of the pedigree to return.  See Details.
#' @param max.gen the maximum number of generations to compute
#'  (e.g., only up to 4th degree relatives).  The default of Inf uses as many
#'  generations as there are in the data.
#' @param sparse logical.  If TRUE, use and return sparse matrices from Matrix package
#' @param verbose logical  If TRUE, print progress through stages of algorithm
#' @param gc logical. If TRUE, do frequent garbage collection via \code{\link{gc}} to save memory
#' @param flatten_diag Logical. The default is FALSE. If TRUE, overwrites the diagonal of the final relatedness matrix with ones.
#' @details source examplePedigreeFunctions
#' @export
#'
ped2com <- function(ped, component,
                    max.gen = Inf,
                    sparse = FALSE,
                    verbose = FALSE,
                    gc = FALSE,
                    flatten_diag = FALSE) {
  component <- match.arg(tolower(component), choices = c("generation", "additive", "common nuclear", "mitochondrial"))
  nr <- nrow(ped)
  if (verbose) {
    cat(paste0("Family Size = ", nr, "\n"))
  }
  parList <- list()
  lens <- integer(nr)
  # Is person in column j the parent of the person in row i? .5 for yes, 0 for no.
  for (i in 1:nr) {
    x <- ped[i, , drop = FALSE]
    if (component %in% c("generation", "additive")) {
      # iS mom of ID or iS dad of ID
      sMom <- (as.numeric(x["ID"]) == as.numeric(ped$momID))
      sDad <- (as.numeric(x["ID"]) == as.numeric(ped$dadID))
      val <- sMom | sDad
      val[is.na(val)] <- FALSE
    } else if (component %in% c("common nuclear")) {
      # IDs have the Same mom and Same dad
      sMom <- (as.numeric(x["momID"]) == as.numeric(ped$momID))
      sMom[is.na(sMom)] <- FALSE
      sDad <- (as.numeric(x["dadID"]) == as.numeric(ped$dadID))
      sDad[is.na(sDad)] <- FALSE
      val <- sMom & sDad
    } else if (component %in% c("mitochondrial")) {
      sMom <- (as.numeric(x["ID"]) == as.numeric(ped$momID))
      sDad <- TRUE
      val <- sMom & sDad
      val[is.na(val)] <- FALSE
    } else {
      stop("Unknown relatedness component requested")
    }
    # keep track of indices only, and then initialize a single sparse matrix
    wv <- which(val)
    parList[[i]] <- wv
    lens[i] <- length(wv)
    if (verbose && !(i %% 100)) {
      cat(paste0("Done with ", i, " of ", nr, "\n"))
    }
  }
  jss <- rep(1L:nr, times = lens)
  iss <- unlist(parList)
  if (gc) {
    rm(parList, lens)
  }
  if (gc) {
    gc()
  }
  if (component %in% c("generation", "additive")) {
    parVal <- .5
  } else if (component %in% c("common nuclear", "mitochondrial")) {
    parVal <- 1
  } else {
    stop("Don't know how to set parental value")
  }
  isPar <- Matrix::sparseMatrix(i = iss, j = jss, x = parVal, dims = c(nr, nr), dimnames = list(ped$ID, ped$ID))
  if (verbose) {
    cat("Completed first degree relatives (adjacency)\n")
  }
  # isPar is the adjacency matrix.  'A' matrix from RAM
  if (component %in% c("common nuclear")) {
    Matrix::diag(isPar) <- 1
    if (!sparse) {
      isPar <- as.matrix(isPar)
    }
    return(isPar)
  }
  isChild <- apply(ped[, c("momID", "dadID")], 1, function(x) {
    2^(-!all(is.na(x)))
  })
  # isChild is the 'S' matrix from RAM
  r <- Matrix::Diagonal(x = 1, n = nr)
  gen <- rep(1, nr)
  mtSum <- sum(r, na.rm = TRUE)
  newIsPar <- isPar
  count <- 0
  maxCount <- max.gen + 1
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
  }
  if (verbose) {
    cat("About to do RAM path tracing\n")
  }
  # compute rsq <- r %*% sqrt(diag(isChild))
  # compute rel <- tcrossprod(rsq)
  if (gc) {
    rm(isPar, newIsPar)
  }
  if (gc) {
    gc()
  }
  if (verbose) {
    cat("Doing I-A inverse times diagonal multiplication\n")
  }
  r2 <- r %*% Matrix::Diagonal(x = sqrt(isChild), n = nr)
  if (gc) {
    rm(r, isChild)
  }
  if (gc) {
    gc()
  }
  if (verbose) {
    cat("Doing tcrossprod\n")
  }
  r <- Matrix::tcrossprod(r2)
  if (component == "generation") {
    return(gen)
  } else {
    if (!sparse) {
      r <- as.matrix(r)
    }
    if (component == "mitochondrial") {
      r[r != 0] <- 1 # for mitochondrial component, set all nonzero values to 1
    }
    if (flatten_diag) { # flattens diagonal if you don't want to deal with inbreeding
      diag(r) <- 1
    }
    return(r)
  }
}

#' Take a pedigree and turn it into an additive genetics relatedness matrix
#' @inheritParams ped2com
#' @details The algorithms and methodologies used in this function are further discussed and exemplified in the vignette titled "examplePedigreeFunctions".
#' For more advanced scenarios and detailed explanations, consult this vignette.

#' @export
#'
ped2add <- function(ped, max.gen = Inf, sparse = FALSE, verbose = FALSE, gc = FALSE, flatten_diag = FALSE) {
  ped2com(
    ped = ped,
    max.gen = max.gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "additive",
    flatten_diag = flatten_diag
  )
}

#' Take a pedigree and turn it into a mitochondrial relatedness matrix
#' @inheritParams ped2com
#' @details source examplePedigreeFunctions
#' @export
#'
ped2mit <- function(ped, max.gen = Inf, sparse = FALSE, verbose = FALSE, gc = FALSE, flatten_diag = FALSE) {
  ped2com(
    ped = ped,
    max.gen = max.gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "mitochondrial",
    flatten.diag = flatten.diag
  )
}

#' Take a pedigree and turn it into a common nuclear environmental relatedness matrix
#' @inheritParams ped2com
#' @details source examplePedigreeFunctions
#' @export
#'
ped2cn <- function(ped, max.gen = Inf, sparse = FALSE, verbose = FALSE, gc = FALSE, flatten_diag = FALSE) {
ped2cn <- function(ped, max.gen = Inf, sparse = FALSE, verbose = FALSE, gc = FALSE, flatten.diag = FALSE) {
  ped2com(
    ped = ped,
    max.gen = max.gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "common nuclear",
    flatten_diag = flatten_diag
  )
}

#' Take a pedigree and turn it into an extended environmental relatedness matrix
#' @inheritParams ped2com
#' @details source examplePedigreeFunctions
#' @export
#'
ped2ce <- function(ped) {
  matrix(1, nrow = nrow(ped), ncol = nrow(ped), dimnames = list(ped$ID, ped$ID))
}
