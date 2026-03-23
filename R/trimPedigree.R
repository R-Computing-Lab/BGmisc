#' Find Leaf Nodes in a Pedigree
#'
#' Identifies individuals who are structural "leaves" in the pedigree network ---
#' those who can potentially be removed without substantially altering the
#' connectivity of the larger tree.
#'
#' Two types of leaves are identified:
#' \itemize{
#'   \item \strong{Terminal nodes}: individuals with outdegree 0, meaning they
#'     have no children recorded in the pedigree. Controlled by
#'     \code{include_terminal}.
#'   \item \strong{Founder singletons}: individuals with indegree 0 \emph{and}
#'     outdegree 1, meaning they are founders (no recorded parents) who appear
#'     as a parent of exactly one child. Controlled by
#'     \code{include_founder_singletons}.
#' }
#'
#' In the directed pedigree graph used by \code{\link{ped2graph}}, edges run
#' from \strong{parent to child}. Consequently, indegree reflects the number of
#' recorded parents and outdegree reflects the number of recorded children.
#'
#' @inheritParams ped2fam
#' @param include_terminal Logical. If \code{TRUE} (default), flag individuals
#'   with no children (outdegree 0) as leaves.
#' @param include_founder_singletons Logical. If \code{TRUE} (default), also
#'   flag founders with exactly one child (indegree 0, outdegree 1) as leaves.
#' @param keep_var Character. Optional column name of a phenotypic variable.
#'   When supplied, individuals are protected from removal based on their value
#'   in this column (see \code{keep_vals}).
#' @param keep_vals Optional vector of values in \code{keep_var} that protect an
#'   individual from being flagged as a leaf. If \code{NULL} (default) and
#'   \code{keep_var} is supplied, any individual with a \emph{non-missing} value
#'   is protected. To protect individuals with missing data instead, pass
#'   \code{keep_vals = NA}.
#' @param verbose Logical. If \code{TRUE}, print counts of each leaf type.
#'
#' @return A character vector of person IDs that are leaf nodes.
#'
#' @seealso \code{\link{trimPedigree}} to iteratively remove the identified leaves.
#'
#' @examples
#' \dontrun{
#' ped <- data.frame(
#'   ID    = 1:6,
#'   dadID = c(NA, NA, 1, 1, 3, NA),
#'   momID = c(NA, NA, 2, 2, 4, NA)
#' )
#' findLeaves(ped)
#' }
#' @export
findLeaves <- function(ped,
                       personID = "ID",
                       momID = "momID",
                       dadID = "dadID",
                       include_terminal = TRUE,
                       include_founder_singletons = TRUE,
                       keep_var = NULL,
                       keep_vals = NULL,
                       verbose = FALSE) {
  if (!include_terminal && !include_founder_singletons) {
    stop("At least one of include_terminal or include_founder_singletons must be TRUE.")
  }

  if (!is.null(keep_var) && !keep_var %in% names(ped)) {
    stop("keep_var '", keep_var, "' not found in pedigree column names.")
  }

  pg <- ped2graph(ped, personID = personID, momID = momID, dadID = dadID)

  indeg  <- igraph::degree(pg, mode = "in")
  outdeg <- igraph::degree(pg, mode = "out")

  terminal_ids          <- if (include_terminal) names(outdeg)[outdeg == 0] else character(0)
  founder_singleton_ids <- if (include_founder_singletons) names(indeg)[indeg == 0 & outdeg == 1] else character(0)

  leaf_ids <- union(terminal_ids, founder_singleton_ids)

  # Restrict to IDs that are actual rows in the pedigree (ped2graph may add
  # phantom nodes for parents listed in momID/dadID but absent as rows)
  leaf_ids <- leaf_ids[leaf_ids %in% as.character(ped[[personID]])]

  # Protect individuals based on phenotype values
  if (!is.null(keep_var)) {
    phenotype_vals <- ped[[keep_var]]
    names(phenotype_vals) <- as.character(ped[[personID]])

    if (is.null(keep_vals)) {
      # Protect anyone with non-missing phenotype data
      protected_ids <- names(phenotype_vals)[!is.na(phenotype_vals)]
    } else if (anyNA(keep_vals)) {
      # Protect anyone with missing phenotype data (and any other keep_vals)
      non_na_vals   <- keep_vals[!is.na(keep_vals)]
      protected_ids <- names(phenotype_vals)[
        is.na(phenotype_vals) | phenotype_vals %in% non_na_vals
      ]
    } else {
      # Protect anyone whose phenotype matches keep_vals
      protected_ids <- names(phenotype_vals)[phenotype_vals %in% keep_vals]
    }

    n_before  <- length(leaf_ids)
    leaf_ids  <- leaf_ids[!leaf_ids %in% protected_ids]

    if (verbose == TRUE) {
      message(n_before - length(leaf_ids), " leaf node(s) protected by keep_var '", keep_var, "'.")
    }
  }

  if (verbose == TRUE) {
    if (include_terminal) {
      message(length(terminal_ids), " terminal nodes (outdegree == 0).")
    }
    if (include_founder_singletons) {
      message(length(founder_singleton_ids), " founder singletons (indegree == 0 & outdegree == 1).")
    }
    message(length(leaf_ids), " total leaf nodes identified.")
  }

  return(leaf_ids)
}


#' Iteratively Trim Leaf Nodes from a Pedigree
#'
#' Repeatedly removes structural leaf nodes from a pedigree until no further
#' trimming is possible or a stopping condition is reached. After each removal
#' pass, parent ID columns are updated so that references to removed individuals
#' are set to \code{NA}.
#'
#' The trimming process peels the pedigree from the outside in: first removing
#' the outermost leaves, then re-evaluating the remaining structure so that
#' newly exposed leaves can be removed in subsequent iterations.
#'
#' Iteration stops when any of the following conditions is met:
#' \itemize{
#'   \item No leaf nodes remain.
#'   \item The number of iterations reaches \code{max_iter}.
#'   \item Removing the next batch of leaves would reduce the pedigree below
#'     \code{min_size} rows.
#' }
#'
#' @inheritParams findLeaves
#' @param max_iter Integer or \code{Inf}. Maximum number of trimming iterations.
#'   Defaults to \code{Inf}, which trims until no other stopping condition applies.
#' @param min_size Integer. Minimum number of individuals to retain. Trimming
#'   stops before any removal that would reduce the pedigree below this size.
#'   Defaults to \code{0L}.
#' @param remove_ids Character vector of additional individual IDs to remove
#'   before any leaf-based trimming. Defaults to \code{NULL}.
#' @param keep_var Character. Optional column name of a phenotypic variable.
#'   Passed to \code{\link{findLeaves}} at every iteration so that individuals
#'   with protected phenotype values are never removed.
#' @param keep_vals Optional vector of phenotype values that protect an
#'   individual from removal. See \code{\link{findLeaves}} for full details.
#'
#' @return A trimmed pedigree \code{data.frame} with the same columns as the
#'   input. Parent ID columns (\code{momID}, \code{dadID}) are updated to
#'   \code{NA} for any references to removed individuals.
#'
#' @seealso \code{\link{findLeaves}} to preview which individuals would be removed.
#'
#' @examples
#' \dontrun{
#' ped <- data.frame(
#'   ID    = 1:6,
#'   dadID = c(NA, NA, 1, 1, 3, NA),
#'   momID = c(NA, NA, 2, 2, 4, NA)
#' )
#' trimPedigree(ped, verbose = TRUE)
#' trimPedigree(ped, min_size = 2, verbose = TRUE)
#' }
#' @export
trimPedigree <- function(ped,
                         personID = "ID",
                         momID = "momID",
                         dadID = "dadID",
                         include_terminal = TRUE,
                         include_founder_singletons = TRUE,
                         max_iter = Inf,
                         min_size = 0L,
                         remove_ids = NULL,
                         keep_var = NULL,
                         keep_vals = NULL,
                         verbose = FALSE) {
  # Apply any user-supplied forced removals first
  if (!is.null(remove_ids)) {
    ped <- ped[!as.character(ped[[personID]]) %in% as.character(remove_ids), ]
    if (verbose == TRUE) {
      message("Removed ", length(remove_ids), " user-specified IDs. ", nrow(ped), " individuals remain.")
    }
  }

  iter <- 0L

  repeat {
    leaf_ids <- findLeaves(
      ped,
      personID = personID,
      momID    = momID,
      dadID    = dadID,
      include_terminal           = include_terminal,
      include_founder_singletons = include_founder_singletons,
      keep_var  = keep_var,
      keep_vals = keep_vals,
      verbose   = FALSE
    )

    if (length(leaf_ids) == 0L) break

    # Enforce min_size: do not remove if doing so would drop below the threshold
    if (nrow(ped) - length(leaf_ids) < min_size) {
      if (verbose == TRUE) {
        message("Stopping: removing ", length(leaf_ids), " leaf node(s) would drop below min_size (", min_size, ").")
      }
      break
    }

    ped  <- ped[!as.character(ped[[personID]]) %in% leaf_ids, ]
    iter <- iter + 1L

    if (verbose == TRUE) {
      message(
        "Iteration ", iter, ": removed ", length(leaf_ids),
        " leaf node(s). ", nrow(ped), " individuals remain."
      )
    }

    if (iter >= max_iter) break
  }

  # Nullify dangling parent references introduced by removals
  if (momID %in% names(ped)) {
    ped[[momID]][!as.character(ped[[momID]]) %in% as.character(ped[[personID]])] <- NA
  }
  if (dadID %in% names(ped)) {
    ped[[dadID]][!as.character(ped[[dadID]]) %in% as.character(ped[[personID]])] <- NA
  }

  return(ped)
}
