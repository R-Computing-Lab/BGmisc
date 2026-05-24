#' Compute relatedness between all individuals and a focal person
#'
#' @description
#' Builds a relatedness matrix for the pedigree and extracts the column
#' corresponding to a single focal individual, appending it as a new column
#' on the pedigree data frame. The full matrix is computed so that relatedness
#' is propagated correctly through all ancestors; only the focal column is
#' retained in the output.
#'
#' Individuals included in the matrix (all individuals when no
#' \code{keep_ids} filter is applied, or those listed in \code{keep_ids})
#' receive their computed relatedness value; genuine zeros remain \code{0}.
#' Individuals excluded by \code{keep_ids} are coded as \code{NA}. The focal
#' person's own self-relatedness (the matrix diagonal) is preserved as-is.
#'
#' @param ped A pedigree data frame with at minimum columns for individual ID,
#'   mother ID, and father ID.
#' @param component Character. Which relatedness component to compute. Passed
#'   directly to \code{\link{ped2com}}. One of \code{"additive"},
#'   \code{"mitochondrial"}, \code{"common nuclear"}, or \code{"generation"}.
#' @param focal_id The ID of the target individual. Must match a value in the
#'   \code{personID} column of \code{ped}.
#' @param personID Character. Name of the individual ID column. Default
#'   \code{"ID"}.
#' @param col_name Character. Name of the new column added to \code{ped}.
#'   Defaults to \code{paste0(component, "Rel_", focal_id)}.
#' @inheritParams ped2com
#'
#' @return The input \code{ped} data frame with one additional column giving
#'   the relatedness of each individual to \code{focal_id} for the requested
#'   component.
#'
#' @seealso \code{\link{ped2com}}, \code{\link{ped2addFocal}}
#' @export
ped2focal <- function(
  ped,
  component,
  focal_id,
  personID = "ID",
  col_name = NULL,
  max_gen = 25,
  sparse = TRUE,
  verbose = FALSE,
  gc = FALSE,
  flatten_diag = FALSE,
  standardize_colnames = TRUE,
  transpose_method = "tcrossprod",
  chunk_size = 1000L,
  keep_ids = NULL,
  adjacency_method = "direct",
  saveable = FALSE,
  resume = FALSE,
  save_rate = 5,
  save_rate_gen = save_rate,
  save_rate_parlist = 100000 * save_rate,
  save_path = "checkpoint/",
  compress = TRUE,
  mz_twins = FALSE,
  mz_method = "addtwins",
  force_symmetric = TRUE,
  ...
) {
  if (!focal_id %in% ped[[personID]]) {
    stop("focal_id '", focal_id, "' not found in column '", personID, "' of ped.")
  }

  if (is.null(col_name)) {
    # can we remove spaces and special characters from component to make cleaner column names? Yes, we can use gsub to replace non-alphanumeric characters with underscores.
    clean_component <- gsub("[^[:alnum:]]+", "_", component)
    col_name <- paste0(clean_component, "Rel_", focal_id)
  }

  mat <- ped2com(
    ped                  = ped,
    component            = component,
    max_gen              = max_gen,
    sparse               = sparse,
    verbose              = verbose,
    gc                   = gc,
    flatten_diag         = flatten_diag,
    standardize_colnames = standardize_colnames,
    transpose_method     = transpose_method,
    chunk_size           = chunk_size,
    keep_ids             = keep_ids,
    adjacency_method     = adjacency_method,
    saveable             = saveable,
    resume               = resume,
    save_rate_gen        = save_rate_gen,
    save_rate_parlist    = save_rate_parlist,
    save_path            = save_path,
    compress             = compress,
    mz_twins             = mz_twins,
    mz_method            = mz_method,
    force_symmetric      = force_symmetric,
    ...
  )

  focal_key <- as.character(focal_id)
  focal_key_pres <- focal_key %in% colnames(mat) || focal_key %in% names(mat)


  if (!focal_key_pres) {
    stop(
      "focal_id '", focal_id, "' was not found in the computed relatedness matrix. ",
      "Check that standardize_colnames and personID are consistent."
    )
  }
  ped_ids <- as.character(ped[[personID]])
  if(is.matrix(mat)|| # or is sparse matrix
     class(mat) %in% c("dgCMatrix", "dsCMatrix", "dgTMatrix", "dsTMatrix")){
  focal_col <- mat[, focal_key]
  idx <- match(ped_ids, rownames(mat))
  } else if (is.list(mat)) {
    focal_col <- mat[[focal_key]]
    idx <- match(ped_ids, rownames(mat))
  } else if(is.vector(mat)) {
    focal_col <- mat
    idx <- match(ped_ids, names(mat))
  } else {
    stop("Unexpected type for relatedness matrix: ", class(mat))
  }

  ped[[col_name]] <- unname(focal_col[idx])

  # Step 1: fill all NAs with 0 — individuals in the pedigree but not in the
  # matrix have no relatedness to the focal person
  ped[[col_name]][is.na(ped[[col_name]])] <- 0

  # Step 2: if keep_ids was supplied, set excluded individuals back to NA
  if (!is.null(keep_ids)) {
    ped[[col_name]][!ped_ids %in% as.character(keep_ids)] <- NA
  }

  ped
}
