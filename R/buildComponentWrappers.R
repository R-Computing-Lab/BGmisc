
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
                    force_symmetric = FALSE,
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
#' @export
#'
ped2ce <- function(ped, personID = "ID",
                   ...) {
  matrix(1, nrow = nrow(ped), ncol = nrow(ped), dimnames = list(ped[[personID]], ped[[personID]]))
}

