# Internal helper: read a chunk from a CSV with optional retry on smaller chunks
#
# @param input_file Path to CSV file
# @param start_line Line to start reading from
# @param chunk_size Number of rows to read
# @param error_handling If TRUE, retry with halved chunk_size up to max_retries times
# @param max_retries Maximum number of retries (each halves chunk_size)
# @param progress_status Path to progress status log file
# @return A list with components `data` (data.table or NULL) and `chunk_size` (possibly reduced)
# @keywords internal
.safe_fread <- function(input_file,
                        start_line, chunk_size,
                        error_handling = FALSE,
                        max_retries = 3,
                        progress_status = NULL) {
  attempt <- 0
  result <- NULL

  repeat {
    result <- tryCatch(
      {
        data.table::fread(input_file,
          skip = start_line - 1,
          nrows = chunk_size,
          header = FALSE,
          sep = ",",
          fill = TRUE
        )
      },
      error = function(e) {
        message("Error reading file: ", e$message)
        if (!is.null(progress_status)) {
          base::cat(
            paste0("Error reading file at line ", start_line, ": ", e$message, "\n"),
            file = progress_status, append = TRUE
          )
        }
        return(NULL)
      }
    )

    if (!is.null(result) || !error_handling || attempt >= max_retries) {
      break
    }

    # Retry with smaller chunk
    attempt <- attempt + 1
    chunk_size <- chunk_size / 2
    message("Trying smaller chunk size (attempt ", attempt, "/", max_retries, "): ", chunk_size)
    gc()
  }

  list(data = result, chunk_size = chunk_size)
}

# Internal helper: filter data by relatedness bin and mitRel value, then append to CSV
#
# @param data A data.table with columns including addRel and mitRel
# @param range_min Minimum additive relatedness for this bin
# @param range_max Maximum additive relatedness for this bin
# @param mit_val mitochondrial relatedness value to filter on (0 or 1)
# @param data_directory Output directory path
# @param verbose Print file names if TRUE
# @keywords internal
.write_bin_data <- function(data, range_min, range_max,
                            mit_val = NULL,
                            data_directory, verbose = FALSE) {
  if (!is.null(mit_val)) {
    range_data <- data[
      base::round(data$addRel, 6) >= range_min &
        base::round(data$addRel, 6) < range_max &
        data$mitRel == mit_val,
    ]
    file_path <- file.path(data_directory, paste0("df_mt", mit_val, "_r", range_min, "-r", range_max, ".csv"))
  } else {
    range_data <- data[
      base::round(data$addRel, 6) >= range_min &
        base::round(data$addRel, 6) < range_max
    ]
    file_path <- file.path(data_directory, paste0("df_r", range_min, "-r", range_max, ".csv"))
  }

  if (base::nrow(range_data) > 0) {
    file_name <- file_path
    if (verbose) {
      message(file_name)
    }
    data.table::fwrite(range_data,
      file = file_name,
      sep = ",",
      append = TRUE,
      row.names = FALSE,
      col.names = FALSE
    )
  }
}
