#' Initialize checkpoint files
#' @inheritParams ped2com
#' @keywords internal

initializeCheckpoint <- function(config = list(
                                   verbose = FALSE,
                                   saveable = FALSE,
                                   resume = FALSE,
                                   save_path = "checkpoint/"
                                 )) {
  # Define checkpoint files
  # Ensure save path exists
  if (config$saveable == TRUE && !dir.exists(config$save_path)) {
    if (config$verbose == TRUE) cat("Creating save path...\n")
    dir.create(config$save_path, recursive = TRUE)
  } else if (config$resume == TRUE && !dir.exists(config$save_path)) {
    stop("Cannot resume from checkpoint. Save path does not exist.")
  }

  checkpoint_files <- list(
    parList = file.path(config$save_path, "parList.rds"),
    lens = file.path(config$save_path, "lens.rds"),
    isPar = file.path(config$save_path, "isPar.rds"),
    iss = file.path(config$save_path, "iss.rds"),
    jss = file.path(config$save_path, "jss.rds"),
    isChild = file.path(config$save_path, "isChild.rds"),
    r_checkpoint = file.path(config$save_path, "r_checkpoint.rds"),
    gen_checkpoint = file.path(config$save_path, "gen_checkpoint.rds"),
    newIsPar_checkpoint = file.path(
      config$save_path,
      "newIsPar_checkpoint.rds"
    ),
    mtSum_checkpoint = file.path(config$save_path, "mtSum_checkpoint.rds"),
    ram_checkpoint = file.path(config$save_path, "ram_checkpoint.rds"),
    r2_checkpoint = file.path(config$save_path, "r2_checkpoint.rds"),
    tcrossprod_checkpoint = file.path(
      config$save_path,
      "tcrossprod_checkpoint.rds"
    ),
    tcrossprod_ids = file.path(config$save_path, "tcrossprod_ids.rds"),
    count_checkpoint = file.path(config$save_path, "count_checkpoint.rds"),
    final_matrix = file.path(config$save_path, "final_matrix.rds")
  )

  checkpoint_files
}


#' Load or compute a checkpoint
#' @param file The file path to load the checkpoint from.
#' @param compute_fn The function to compute the checkpoint if it doesn't exist.
#' @param config A list containing configuration parameters such as `resume`, `verbose`, and `saveable`.
#' @param message_resume Optional message to display when resuming from a checkpoint.
#' @param message_compute Optional message to display when computing the checkpoint.
#' @param compress a logical specifying whether saving to a named file is to use "gzip" compression, or one of "gzip", "bzip2", "xz" or "zstd" to indicate the type of compression to be used. Ignored if file is a connection.
#' @return The loaded or computed checkpoint.
#' @keywords internal
loadOrComputeCheckpoint <- function(file, compute_fn,
                                    config, message_resume = NULL,
                                    message_compute = NULL,
                                    compress = TRUE) {
  if (config$resume == TRUE && file.exists(file)) {
    if (config$verbose == TRUE && !is.null(message_resume)) cat(message_resume)
    readRDS(file)
  } else {
    if (config$verbose == TRUE && !is.null(message_compute)) cat(message_compute)
    result <- compute_fn()
    if (config$saveable == TRUE) saveRDS(result, file = file, compress = compress)
    result
  }
}

#' Subset output to requested IDs
#' @inheritParams ped2com
#' @param component A component to subset.
#' @param keep_ids Character vector of IDs to retain.
#' @param available_ids Character vector of IDs available in \code{x}.
#' @param verbose_message Character. Message prefix to print when \code{config$verbose == TRUE}.
#' @param drop logical. Passed to \code{[} when subsetting matrices.
#' @keywords internal
.subsetKeepIds <- function(component, keep_ids = NULL, available_ids, config,
                           verbose_message = "Subsetting to %d target individuals\n",
                           drop = FALSE) {
  if (is.null(keep_ids)) {
    return(component)
  }

  idx <- match(keep_ids, available_ids)
  missing <- keep_ids[is.na(idx)]

  if (length(missing) > 0) {
    warning(
      length(missing), " keep_ids not found in pedigree and will be dropped: ",
      paste(Matrix::head(missing, 5), collapse = ", "),
      if (length(missing) > 5) " ..." else ""
    )
  }

  idx <- idx[!is.na(idx)]

  if (config$verbose == TRUE) {
    cat(sprintf(verbose_message, length(idx)))
  }
  # consequence is missing data
  if (is.matrix(component) || methods::is(component, "Matrix")) {
    component <- component[idx, , drop = drop]
  } else {
    component <- component[idx]
  }

  component
}
