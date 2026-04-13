#' Normalize ID Columns in a Pedigree Dataframe
#'
#' Creates a standardized numeric ID system for a pedigree while preserving
#' the original IDs. This is the recommended entry point for preparing
#' pedigree data for analysis functions that require numeric IDs (e.g.,
#' relatedness matrices, adjacency matrices).
#'
#' The function:
#' \enumerate{
#'   \item Builds a mapping between original IDs (any type) and new
#'     sequential integer IDs.
#'   \item Replaces all ID columns with the new integer IDs.
#'   \item Stores the mapping as an attribute so results can be converted
#'     back to original IDs via \code{\link{restoreIDs}}.
#' }
#'
#' @param ped A data.frame (or object inheriting from data.frame) containing
#'   pedigree information.
#' @param id_cols A character vector of column names to normalize. Columns that
#'   do not exist in \code{ped} are silently skipped. Defaults to
#'   \code{c("ID", "momID", "dadID", "famID")}.
#' @param remap Logical. If \code{TRUE} (default), replace original IDs with
#'   new sequential integers. If \code{FALSE}, only attach the mapping as an
#'   attribute without modifying the ID values.
#' @param verbose Logical. If \code{TRUE}, messages are printed describing
#'   what normalization was performed.
#'
#' @return A data.frame with integer ID columns (if \code{remap = TRUE}) and
#'   an \code{"id_map"} attribute containing the mapping data.frame. The
#'   mapping has columns \code{original_id} and \code{numeric_id}, plus
#'   a \code{"source_classes"} attribute recording the original column types.
#'
#' @examples
#' ped <- data.frame(
#'   ID = c("A", "B", "C", "D"),
#'   momID = c(NA, NA, "A", "A"),
#'   dadID = c(NA, NA, "B", "B"),
#'   famID = c(1, 1, 1, 1)
#' )
#'
#' # Create numeric IDs for analysis
#' ped_num <- normalizeIDs(ped)
#' ped_num
#'
#' # Get the mapping
#' attr(ped_num, "id_map")
#'
#' # Convert back to original IDs
#' restoreIDs(ped_num)
#'
#' @seealso \code{\link{restoreIDs}} to convert back, \code{\link{idMap}} to
#'   build a mapping without modifying the pedigree.
#' @export
normalizeIDs <- function(ped,
                         id_cols = c("ID", "momID", "dadID", "famID"),
                         remap = TRUE,
                         verbose = FALSE) {
  if (!inherits(ped, "data.frame")) {
    stop("ped must be a data.frame or inherit from data.frame")
  }

  # Keep only columns that actually exist
  present_cols <- id_cols[id_cols %in% names(ped)]
  if (length(present_cols) == 0L) {
    if (verbose) message("No ID columns found in ped; returning unchanged.")
    return(ped)
  }

  # Build the mapping from original IDs to sequential integers
  map <- idMap(ped, present_cols)

  if (verbose) {
    message(
      "ID mapping: ", nrow(map), " unique IDs -> integers 1:", nrow(map)
    )
  }

  # Attach mapping as attribute
  attr(ped, "id_map") <- map
  attr(ped, "id_cols") <- present_cols

  # Record original classes so restoreIDs can faithfully reconstruct
  orig_classes <- vapply(present_cols, function(col) class(ped[[col]])[1L],
    character(1L)
  )
  attr(map, "source_classes") <- orig_classes
  attr(ped, "id_map") <- map

  if (remap) {
    ped <- applyIDMap(ped, map, present_cols, direction = "to_numeric")
    if (verbose) {
      message("Replaced ID columns with sequential integers.")
    }
  }

  ped
}


#' Restore Original IDs from a Normalized Pedigree
#'
#' Reverses the mapping applied by \code{\link{normalizeIDs}}, converting
#' numeric surrogate IDs back to the original ID values and types.
#'
#' @param ped A data.frame that was previously processed by
#'   \code{\link{normalizeIDs}}.
#' @param id_map Optional. An ID mapping data.frame (as produced by
#'   \code{\link{idMap}}). If \code{NULL}, the function uses the
#'   \code{"id_map"} attribute attached by \code{\link{normalizeIDs}}.
#' @param id_cols Optional. Character vector of columns to restore. If
#'   \code{NULL}, uses the \code{"id_cols"} attribute.
#' @param verbose Logical.
#'
#' @return A data.frame with original ID values restored.
#'
#' @examples
#' ped <- data.frame(
#'   ID = c("A", "B", "C"),
#'   momID = c(NA, NA, "A"),
#'   dadID = c(NA, NA, "B")
#' )
#' ped_num <- normalizeIDs(ped)
#' restoreIDs(ped_num)
#'
#' @seealso \code{\link{normalizeIDs}}
#' @export
restoreIDs <- function(ped, id_map = NULL, id_cols = NULL, verbose = FALSE) {
  if (is.null(id_map)) {
    id_map <- attr(ped, "id_map")
  }
  if (is.null(id_map)) {
    stop("No id_map found. Provide one or use a ped from normalizeIDs().")
  }
  if (is.null(id_cols)) {
    id_cols <- attr(ped, "id_cols")
  }
  if (is.null(id_cols)) {
    id_cols <- c("ID", "momID", "dadID", "famID")
  }

  present_cols <- id_cols[id_cols %in% names(ped)]
  ped <- applyIDMap(ped, id_map, present_cols, direction = "to_original")

  # Restore original types
  source_classes <- attr(id_map, "source_classes")
  if (!is.null(source_classes)) {
    for (col in present_cols) {
      if (col %in% names(source_classes)) {
        orig_class <- source_classes[[col]]
        if (orig_class == "integer") {
          ped[[col]] <- as.integer(ped[[col]])
        } else if (orig_class %in% c("numeric", "double")) {
          ped[[col]] <- as.numeric(ped[[col]])
        }
        # character stays as-is from the restore
      }
    }
  }

  # Clean up attributes
  attr(ped, "id_map") <- NULL
  attr(ped, "id_cols") <- NULL

  if (verbose) message("Restored original IDs.")
  ped
}


#' Build an ID Mapping Table
#'
#' Collects all unique non-NA ID values across the specified columns and
#' assigns each a sequential integer. This is the core building block used
#' by \code{\link{normalizeIDs}}, but can also be called directly when you
#' need the mapping without modifying the pedigree.
#'
#' @param ped A data.frame.
#' @param id_cols Character vector of column names containing IDs.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{original_id}{The original ID value, stored as character.}
#'     \item{numeric_id}{Sequential integer from 1 to N.}
#'   }
#'
#' @examples
#' ped <- data.frame(
#'   ID = c("A", "B", "C"),
#'   momID = c(NA, NA, "A"),
#'   dadID = c(NA, NA, "B")
#' )
#' idMap(ped, c("ID", "momID", "dadID"))
#'
#' @export
idMap <- function(ped, id_cols = c("ID", "momID", "dadID", "famID")) {
  # Collect all non-NA values across the requested columns
  present_cols <- id_cols[id_cols %in% names(ped)]
  all_ids <- unlist(lapply(present_cols, function(col) ped[[col]]), use.names = FALSE)
  all_ids <- as.character(all_ids)
  unique_ids <- unique(all_ids[!is.na(all_ids)])

  data.frame(
    original_id = unique_ids,
    numeric_id = seq_along(unique_ids),
    stringsAsFactors = FALSE
  )
}


#' Apply an ID Mapping to Pedigree Columns
#'
#' Translates ID columns between original and numeric representations
#' using a mapping table.
#'
#' @param ped A data.frame.
#' @param id_map A mapping data.frame from \code{\link{idMap}}.
#' @param id_cols Character vector of columns to translate.
#' @param direction Either \code{"to_numeric"} or \code{"to_original"}.
#'
#' @return The data.frame with translated ID columns.
#' @keywords internal
applyIDMap <- function(ped, id_map, id_cols, direction = "to_numeric") {
  if (direction == "to_numeric") {
    lookup <- stats::setNames(id_map$numeric_id, id_map$original_id)
    for (col in id_cols) {
      orig_vals <- as.character(ped[[col]])
      ped[[col]] <- lookup[orig_vals]
      # NAs stay as NA (lookup returns NA for NA keys)
      ped[[col]] <- as.integer(ped[[col]])
    }
  } else {
    lookup <- stats::setNames(id_map$original_id, as.character(id_map$numeric_id))
    for (col in id_cols) {
      num_vals <- as.character(ped[[col]])
      ped[[col]] <- lookup[num_vals]
      # Remove names from the lookup result
      ped[[col]] <- unname(ped[[col]])
    }
  }
  ped
}


#' Relabel Matrix Dimnames Using an ID Map
#'
#' Translates the row and column names of a matrix (or Matrix) from numeric
#' surrogate IDs back to original IDs using a mapping table. This is used
#' internally to label relatedness and adjacency matrices with the user's
#' original IDs after computation is done with integer surrogates.
#'
#' @param mat A matrix or Matrix object whose dimnames are (character
#'   representations of) numeric surrogate IDs.
#' @param id_map A mapping data.frame from \code{\link{idMap}}, with columns
#'   \code{original_id} and \code{numeric_id}.
#'
#' @return The same matrix with dimnames replaced by original IDs.
#'
#' @examples
#' m <- matrix(1:9, 3, 3, dimnames = list(1:3, 1:3))
#' map <- data.frame(original_id = c("A", "B", "C"), numeric_id = 1:3)
#' relabelMatrix(m, map)
#'
#' @keywords internal
relabelMatrix <- function(mat, id_map) {
  lookup <- stats::setNames(id_map$original_id, as.character(id_map$numeric_id))
  rn <- rownames(mat)
  cn <- colnames(mat)
  if (!is.null(rn)) {
    rownames(mat) <- unname(lookup[as.character(rn)])
  }
  if (!is.null(cn)) {
    colnames(mat) <- unname(lookup[as.character(cn)])
  }
  mat
}
