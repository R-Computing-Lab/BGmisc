#' Reduce an ancestor distance matrix to a pairwise generational distance matrix
#'
#' Given the \code{n x n} ancestor distance matrix returned by
#' \code{ped2com(component = "distance")}, computes a pairwise distance matrix
#' \code{D} where \code{D[i, j]} summarizes the relationship between individuals
#' \code{i} and \code{j} through their common ancestors.
#'
#' For each potential common ancestor column \code{c}, the combined step count
#' \code{ancDist[i, c] + ancDist[j, c]} is computed for all pairs simultaneously
#' via \code{outer}, then collapsed across ancestors with \code{pmin}/\code{pmax}.
#'
#' @param ancDist  Ancestor distance matrix from
#'   \code{ped2com(component = "distance")}: \code{ancDist[i, j]} is the
#'   minimum number of parent-child steps from individual \code{i} up to
#'   ancestor \code{j}; \code{NA} if \code{j} is not an ancestor of \code{i};
#'   diagonal = 0.
#' @param method   One of \code{"path"}, \code{"mrca_min"}, \code{"mrca_max"},
#'   or \code{"mrca_all"}.
#' @return A symmetric numeric \code{n x n} matrix; \code{NA} for unrelated
#'   pairs.
#' @keywords internal
.pairDistFromAnc <- function(ancDist, method) {
  n   <- nrow(ancDist)
  ids <- rownames(ancDist)

  if (method %in% c("path", "mrca_min")) {
    D       <- matrix(Inf, n, n, dimnames = list(ids, ids))
    diag(D) <- 0

    for (c in seq_len(n)) {
      d_c     <- ancDist[, c]
      has_anc <- which(!is.na(d_c))
      if (length(has_anc) < 2L) next
      sub     <- as.numeric(d_c[has_anc])
      D[has_anc, has_anc] <- pmin(
        D[has_anc, has_anc],
        outer(sub, sub, "+")
      )
    }
    D[D == Inf] <- NA_real_

  } else if (method == "mrca_max") {
    D       <- matrix(-Inf, n, n, dimnames = list(ids, ids))
    diag(D) <- 0
    any_com <- matrix(FALSE, n, n)

    for (c in seq_len(n)) {
      d_c     <- ancDist[, c]
      has_anc <- which(!is.na(d_c))
      if (length(has_anc) < 2L) next
      sub     <- as.numeric(d_c[has_anc])
      D[has_anc, has_anc] <- pmax(
        D[has_anc, has_anc],
        outer(sub, sub, "+")
      )
      any_com[has_anc, has_anc] <- TRUE
    }
    diag(any_com) <- TRUE
    D[!any_com]   <- NA_real_
    D[D == -Inf]  <- NA_real_

  } else {
    # mrca_all - aggregation strategy across ALL common ancestors is
    # domain-specific (sum? mean? something else?).
    # TODO: define aggregation and replace this stop().
    # Options to consider:
    #   sum(total_steps)  - total path weight through every shared ancestor
    #   mean(total_steps) - average distance across all shared ancestors
    # Your domain knowledge of what "all paths" should mean for pedigree
    # analysis goes here. For inbred pedigrees this will differ substantially.
    stop("mrca_all aggregation not yet implemented - please define the aggregation strategy.")
  }

  D
}

#' Compute the generational distance between two individuals
#'
#' @description
#' Returns a single numeric value representing how many generations apart two
#' individuals are, according to the chosen method.
#'
#' @param ped      A pedigree data frame.
#' @param id1,id2  IDs of the two individuals to compare.
#' @param method   Distance method. One of:
#'   \describe{
#'     \item{\code{"rank"}}{Absolute difference in generation numbers from
#'       \code{\link{ped2gen}}. Founders = 1, their children = 2, etc.}
#'     \item{\code{"path"}}{Minimum number of parent-child steps between the
#'       two individuals through any shared ancestor (undirected path length).}
#'     \item{\code{"mrca_min"}}{Total steps via the most recent common ancestor
#'       (fewest combined steps to reach a shared ancestor).}
#'     \item{\code{"mrca_max"}}{Total steps via the most distant common ancestor
#'       (most combined steps to reach a shared ancestor).}
#'     \item{\code{"mrca_all"}}{Aggregated distance across all common ancestors}
#'   }
#' @param personID Character. ID column name. Default \code{"ID"}.
#' @param momID    Character. Mother ID column name. Default \code{"momID"}.
#' @param dadID    Character. Father ID column name. Default \code{"dadID"}.
#' @param max_gen  Integer. Maximum generations to traverse. Default \code{25}.
#' @param ...      Additional arguments passed to \code{\link{ped2com}}
#'   (e.g. \code{adjacency_method}, \code{verbose}, \code{sparse}).
#'
#' @return A single numeric value, or \code{NA} if no genealogical path exists.
#' @seealso \code{\link{ped2genDist}}, \code{\link{ped2genDistFocal}}
#' @export
getGenDist <- function(ped, id1, id2,
                       method   = c("rank", "path", "mrca_min", "mrca_max", "mrca_all"),
                       personID = "ID",
                       momID    = "momID",
                       dadID    = "dadID",
                       max_gen  = 25L,
                       ...) {
  method <- match.arg(method)

  for (id in c(id1, id2)) {
    if (!id %in% ped[[personID]]) {
      stop("ID '", id, "' not found in column '", personID, "' of ped.")
    }
  }

  if (method == "rank") {
    gen <- ped2gen(ped, sparse = FALSE)
    gen <- setNames(gen, as.character(ped[[personID]]))
    return(abs(gen[as.character(id1)] - gen[as.character(id2)]))
  }

  ancDist <- ped2com(ped, component = "distance", max_gen = max_gen, sparse = FALSE, ...)
  D       <- .pairDistFromAnc(ancDist, method)
  D[as.character(id1), as.character(id2)]
}

#' Compute generational distances from a focal individual to all others
#'
#' @description
#' Appends a new column to \code{ped} containing the generational distance
#' between each individual and the focal person.  Unrelated individuals
#' receive \code{NA}.
#'
#' @param ped      A pedigree data frame.
#' @param focal_id ID of the target individual.
#' @param method   Distance method; see \code{\link{getGenDist}}.
#' @param col_name Name of the new column. Defaults to
#'   \code{paste0("genDist_", method, "_", focal_id)}.
#' @inheritParams getGenDist
#'
#' @return The input \code{ped} with one additional column.
#' @seealso \code{\link{getGenDist}}, \code{\link{ped2genDist}}
#' @export
ped2genDistFocal <- function(ped, focal_id,
                              method   = c("rank", "path", "mrca_min", "mrca_max", "mrca_all"),
                              col_name = NULL,
                              personID = "ID",
                              momID    = "momID",
                              dadID    = "dadID",
                              max_gen  = 25L,
                              ...) {
  method <- match.arg(method)

  if (!focal_id %in% ped[[personID]]) {
    stop("focal_id '", focal_id, "' not found in column '", personID, "' of ped.")
  }

  if (is.null(col_name)) {
    col_name <- paste0("genDist_", method, "_", focal_id)
  }

  if (method == "rank") {
    gen             <- ped2gen(ped, sparse = FALSE)
    gen             <- setNames(gen, as.character(ped[[personID]]))
    focal_key       <- as.character(focal_id)
    ped[[col_name]] <- abs(gen - gen[focal_key])
    return(ped)
  }

  ancDist         <- ped2com(ped, component = "distance", max_gen = max_gen, sparse = FALSE, ...)
  D               <- .pairDistFromAnc(ancDist, method)
  focal_key       <- as.character(focal_id)
  ped_ids         <- as.character(ped[[personID]])
  ped[[col_name]] <- D[ped_ids, focal_key]

  ped
}

#' Compute a full pairwise generational distance matrix
#'
#' @description
#' Returns an n x n matrix of generational distances between all pairs of
#' individuals in \code{ped}.  Pairs with no genealogical path receive
#' \code{NA}.
#'
#' Uses \code{\link{ped2com}} with \code{component = "distance"} to build the
#' ancestor distance matrix via the same adjacency construction and power
#' iteration used by all other pedigree components, then reduces it to pairwise
#' distances with vectorised \code{outer} + \code{pmin}/\code{pmax}.
#'
#' @param ped    A pedigree data frame.
#' @param method Distance method; see \code{\link{getGenDist}}.
#' @inheritParams getGenDist
#'
#' @return A numeric matrix with row and column names set to individual IDs.
#' @seealso \code{\link{getGenDist}}, \code{\link{ped2genDistFocal}}
#' @export
ped2genDist <- function(ped,
                        method   = c("rank", "path", "mrca_min", "mrca_max", "mrca_all"),
                        personID = "ID",
                        momID    = "momID",
                        dadID    = "dadID",
                        max_gen  = 25L,
                        ...) {
  method  <- match.arg(method)
  ped_ids <- as.character(ped[[personID]])

  if (method == "rank") {
    gen <- ped2gen(ped, sparse = FALSE)
    gen <- setNames(as.numeric(gen), ped_ids)
    mat <- abs(outer(gen, gen, "-"))
    dimnames(mat) <- list(ped_ids, ped_ids)
    return(mat)
  }

  ancDist <- ped2com(ped, component = "distance", max_gen = max_gen, sparse = FALSE, ...)
  .pairDistFromAnc(ancDist, method)
}
