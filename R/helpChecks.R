#' Drop Identical Duplicate IDs from Pedigree Data Frame
#'
#' #' This function identifies and removes duplicate entries in a pedigree data frame
#' based on a list of specified IDs. If multiple rows share the same ID and are
#' identical, only one instance is retained. The function returns the modified pedigree
#' data frame along with a log of changes made.
#' @param ped A data frame representing the pedigree.
#' @param ids A vector of IDs to check for duplicates in the pedigree.
#' @param changes An optional list to log changes made during the process.
dropIdenticalDuplicateIDs <- function(ped, ids, changes = NULL
) {
  if (!is.data.frame(ped)) {
    stop("ped must be a data frame")
  }
  if (is.null(changes)) {
    changes <- list()
  } else if (!is.list(changes)) {
    stop("changes must be a list or NULL")
  }

  out <- ped

  if (!is.null(ids) && length(ids) > 0) {
    for (id in ids) {
      rows_with_id <- out[out$ID == id, , drop = FALSE]
      if (nrow(unique(rows_with_id)) == 1) {
        changes[[paste0("ID", id)]] <- "Removed duplicates"
        out <- out[-which(out$ID == id)[-1], , drop = FALSE]
      } else {
        changes[[paste0("ID", id)]] <- "Kept duplicates"
      }
    }
  }

  list(ped = out, changes = changes)
}

#' Helper function to conditionally add elements to a list
#' @param validation A list to which elements may be added.
#' @param name A character string representing the name of the element to add.
#' @param value The value to add to the list if it is not NULL or empty
#' @return The updated list with the new element added if applicable.
#' @keywords internal
addIfAny <- function(validation, name, value) {
  if (!is.null(value) && length(value) > 0) validation[[name]] <- value
  validation
}
