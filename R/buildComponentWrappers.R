#' Take a pedigree and turn it into an additive genetics relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2add <- function(ped, max_gen = 25, sparse = TRUE, verbose = FALSE,
                    gc = FALSE,
                    flatten_diag = FALSE, standardize_colnames = TRUE,
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
                    ...) {
  ped2com(
    ped = ped,
    max_gen = max_gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "additive",
    flatten_diag = flatten_diag,
    standardize_colnames = standardize_colnames,
    transpose_method = transpose_method,
    chunk_size = chunk_size,
    keep_ids = keep_ids,
    adjacency_method = adjacency_method,
    saveable = saveable,
    resume = resume,
    save_rate_gen = save_rate_gen,
    save_rate_parlist = save_rate_parlist,
    save_path = save_path,
    compress = compress,
    mz_twins = mz_twins,
    mz_method = mz_method,
    force_symmetric = force_symmetric,
    ...
  )
}

#' Take a pedigree and turn it into a mitochondrial relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#' @aliases ped2mt
#'
ped2mit <- ped2mt <- function(ped, max_gen = 25,
                              sparse = TRUE,
                              verbose = FALSE, gc = FALSE,
                              flatten_diag = FALSE,
                              standardize_colnames = TRUE,
                              transpose_method = "tcrossprod",
                              keep_ids = NULL,
                              adjacency_method = "direct",
                              saveable = FALSE,
                              resume = FALSE,
                              save_rate = 5,
                              save_rate_gen = save_rate,
                              save_rate_parlist = 100000 * save_rate,
                              save_path = "checkpoint/",
                              compress = TRUE,
                              force_symmetric = FALSE,
                              ...) {
  ped2com(
    ped = ped,
    max_gen = max_gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "mitochondrial",
    flatten_diag = flatten_diag,
    standardize_colnames = standardize_colnames,
    transpose_method = transpose_method,
    keep_ids = keep_ids,
    adjacency_method = adjacency_method,
    saveable = saveable,
    resume = resume,
    save_rate_gen = save_rate_gen,
    save_rate_parlist = save_rate_parlist,
    save_path = save_path,
    compress = compress,
    force_symmetric = force_symmetric,
    ...
  )
}

#' Take a pedigree and turn it into a common nuclear environmental  matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2cn <- function(ped, max_gen = 25, sparse = TRUE, verbose = FALSE,
                   gc = FALSE, flatten_diag = FALSE,
                   standardize_colnames = TRUE,
                   transpose_method = "tcrossprod",
                   keep_ids = NULL,
                   saveable = FALSE,
                   resume = FALSE,
                   save_rate = 5,
                   adjacency_method = "direct",
                   save_rate_gen = save_rate,
                   save_rate_parlist = 1000 * save_rate,
                   save_path = "checkpoint/",
                   compress = TRUE,
                   force_symmetric = FALSE,
                   ...) {
  ped2com(
    ped = ped,
    max_gen = max_gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "common nuclear",
    adjacency_method = adjacency_method,
    flatten_diag = flatten_diag,
    standardize_colnames = standardize_colnames,
    transpose_method = transpose_method,
    keep_ids = keep_ids,
    saveable = saveable,
    resume = resume,
    save_rate_gen = save_rate_gen,
    save_rate_parlist = save_rate_parlist,
    save_path = save_path,
    compress = compress,
    force_symmetric = force_symmetric,
    ...
  )
}
#' Take a pedigree and turn it into a generation relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2gen <- function(ped, max_gen = 25, sparse = TRUE, verbose = FALSE,
                    gc = FALSE, flatten_diag = FALSE,
                    standardize_colnames = TRUE,
                    transpose_method = "tcrossprod",
                    keep_ids = NULL,
                    saveable = FALSE,
                    resume = FALSE,
                    save_rate = 5,
                    adjacency_method = "direct",
                    save_rate_gen = save_rate,
                    save_rate_parlist = 1000 * save_rate,
                    save_path = "checkpoint/",
                    compress = TRUE,
                    force_symmetric = FALSE,
                    ...) {
  ped2com(
    ped = ped,
    max_gen = max_gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "generation",
    adjacency_method = adjacency_method,
    flatten_diag = flatten_diag,
    standardize_colnames = standardize_colnames,
    transpose_method = transpose_method,
    keep_ids = keep_ids,
    saveable = saveable,
    resume = resume,
    save_rate_gen = save_rate_gen,
    save_rate_parlist = save_rate_parlist,
    save_path = save_path,
    compress = compress,
    force_symmetric = force_symmetric,
    ...
  )
}


#' Take a pedigree and turn it into an extended environmental relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @param personID Character. Column name for individual IDs.
#' @export
#'
ped2ce <- function(ped, personID = "ID",
                   keep_ids = NULL,
                   sparse = FALSE, verbose = FALSE,
                   ...) {
  if (!is.null(keep_ids)) {
    ped <- ped[ped[[personID]] %in% keep_ids, ]
  }
  if (sparse) {
    mat <- Matrix::sparseMatrix(
      i = seq_len(nrow(ped)),
      j = seq_len(nrow(ped)),
      x = 1,
      dimnames = list(ped[[personID]], ped[[personID]])
    )
    return(mat)
  } else {
    matrix(1, nrow = nrow(ped), ncol = nrow(ped), dimnames = list(ped[[personID]], ped[[personID]]))
  }
}

#' Add a focal-person additive relatedness column to a pedigree
#' @inheritParams ped2focal
#' @inherit ped2focal details
#' @seealso \code{\link{ped2focal}}
#' @export
ped2addFocal <- function(
  ped,
  focal_id,
  personID  = "ID",
  col_name  = NULL,
  max_gen   = 25,
  sparse    = TRUE,
  verbose   = FALSE,
  gc        = FALSE,
  flatten_diag         = FALSE,
  standardize_colnames = TRUE,
  transpose_method     = "tcrossprod",
  chunk_size           = 1000L,
  keep_ids             = NULL,
  adjacency_method     = "direct",
  saveable   = FALSE,
  resume     = FALSE,
  save_rate  = 5,
  save_rate_gen     = save_rate,
  save_rate_parlist = 100000 * save_rate,
  save_path  = "checkpoint/",
  compress   = TRUE,
  mz_twins   = FALSE,
  mz_method  = "addtwins",
  force_symmetric = TRUE,
  ...
) {
  ped2focal(
    ped                  = ped,
    component            = "additive",
    focal_id             = focal_id,
    personID             = personID,
    col_name             = col_name,
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
}
