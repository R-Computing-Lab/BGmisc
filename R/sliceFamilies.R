#' sliceFamilies
#'
#' @description
#' Slices up families by additive relatedness, creating CSV files grouped by degree of relatedness.
#' Operates on a potentially large file by reading in chunks and binning links by additive relatedness.
#'
#' @param outcome_name Name of the outcome variable (used for naming input/output files)
#' @param biggest Logical; whether to process the "biggest" family dataset (TRUE) or all-but-biggest (FALSE)
#' @param bin_width Width of additive relatedness bins (default is 0.10)
#' @param chunk_size Number of lines to read in each chunk (default 2e7)
#' @param max_lines Max number of lines to process from input file (default 1e13)
#' @param input_file Path to the input CSV file. If NULL, defaults to a specific file based on `biggest` flag.
#' @param progress_csv Path to a CSV file for tracking progress (default "progress.csv")
#' @param progress_status Path to a text file for logging progress status (default "progress.txt")
#' @param file_column_names Names of the columns in the input file (default c("ID1", "ID2", "addRel", "mitRel", "cnuRel"))
#' @param degreerelatedness Maximum degree of relatedness to consider (default 12)
#' @param folder_prefix Prefix for the output folder (default "data")
#' @param data_directory Directory where output files will be saved. If NULL, it is constructed based on `outcome_name` and `folder_prefix`.
#' @param verbose Logical; whether to print progress messages (default FALSE)
#' @param addRel_ceiling Numeric. Maximum relatedness value to bin to. Default is 1.5
#' @param mtdna Logical. Whether to separate bins by mitochondrial relatedness (mitRel) value. Default is TRUE
#' @param error_handling Logical. Should more aggressive error handling be attempted? Default is FALSE
#' @param max_retries Integer. Number of retry attempts with halved chunk size when error_handling is TRUE. Default is 2
#' @return NULL. Writes CSV files to disk and updates progress logs.
#' @export
#'
sliceFamilies <- function(
  outcome_name = "AD_demo",
  biggest = TRUE,
  bin_width = 0.10,
  degreerelatedness = 12,
  chunk_size = 2e7,
  max_lines = 1e13,
  addRel_ceiling = 1.5,
  mtdna= TRUE,
  input_file = NULL,
  folder_prefix = "data",
  progress_csv = "progress.csv",
  progress_status = "progress.txt",
  data_directory = NULL,
  verbose = FALSE,
  error_handling = FALSE,
  max_retries = 4,
  file_column_names = c("ID1", "ID2", "addRel", "mitRel", "cnuRel")
) {
  bin_width_string <- as.character(bin_width * 100)

#  if(mtdna ==FALSE & "mitRel" %in% file_column_names) {
 #   file_column_names <- file_column_names[!file_column_names %in% "mitRel"]
  #  message("mtdna is set to FALSE, so 'mitRel' column will be ignored in processing.")
 # }

  if (is.null(data_directory)) {
    # Set the data directory based on the outcome name and folder prefix
    link_suffix <- if (biggest == TRUE) "links_" else "links_allbut_"
    data_directory <- file.path(outcome_name, folder_prefix, paste0(link_suffix, bin_width_string))
  }
  # Ensure the data_directory exists, creating it along with any necessary parent directories
  if (!base::dir.exists(data_directory)) {
    dir.create(data_directory, showWarnings = FALSE, recursive = TRUE)
  }
  # Ensure the data_directory exists, creating it along with any necessary parent directories
  if (is.null(input_file)) {
    file_suffix <- if (biggest == TRUE) "_dataBiggestRelatedPairsTake2.csv" else "_dataAllbutBiggestRelatedPairsTake2.csv"
    input_file <- paste0(outcome_name, file_suffix)
  } else if (!base::file.exists(input_file)) {
    stop("Input file does not exist: ", input_file)
  }

  if (verbose == TRUE) {
    message("Output folder: ", data_directory)
    message("Input file: ", input_file)
  }


  # create binning ranges for additive relatedness
  addRel_center <- 2^(0:(-degreerelatedness))

  addRel_maxs_temp <- addRel_center * (1 + bin_width)
  # inclusive
  addRel_mins_temp <- addRel_center * (1 - bin_width)

  # this is supposed to have one of each
  addRel_real_maxs <- addRel_mins_temp[-length(addRel_mins_temp)]
  addRel_real_mins <- addRel_maxs_temp[-1]

  addRel_maxs <- c(
    addRel_ceiling,
    base::sort(c(addRel_real_maxs, addRel_maxs_temp), decreasing = TRUE),
    addRel_mins_temp[length(addRel_mins_temp)]
  )
  addRel_mins <- c(
    addRel_maxs_temp[1],
    base::sort(c(addRel_real_mins, addRel_mins_temp), decreasing = TRUE),
    0
  )

  start_line <- 1 + 1 # Start from the second line (skip header)
  end_line <- chunk_size + 1
  total_lines <- 0


  # pick up where left off if progress file exists
  if (base::file.exists(progress_csv)) {
    if (verbose == TRUE) {
      message("Resuming from previous progress...")
    }
    progress_data <- data.table::fread(progress_csv, header = TRUE)
    start_line <- progress_data$start_line
    total_lines <- progress_data$total_lines
    chunk_size <- progress_data$chunk_size
    writeLines <- base::paste0("Resuming from line ", start_line, " with total lines processed: ", total_lines)
    base::cat(writeLines, "\n", file = progress_status, append = TRUE)
  } else {
    # If progress file does not exist, create it with initial values
    progress_data <- data.frame(
      start_line = start_line,
      total_lines = total_lines,
      end_line = end_line,
      chunk_size = chunk_size
    )

    data.table::fwrite(progress_data, file = progress_csv, sep = ",", col.names = TRUE)

    writeLines <- base::paste0(
      "Starting from line ", start_line,
      " with chunk size: ", chunk_size,
      "\nStart time: ", base::Sys.time()
    )

    base::cat(writeLines, "\n",
      file = progress_status,
      append = FALSE
    )
  }

  start_time <- base::Sys.time()

  while (start_line <= max_lines) {
    # error handling

    read_result <- tryCatch(
      {
        .safe_fread(
          input_file = input_file,
          start_line = start_line,
          chunk_size = chunk_size,
          error_handling = error_handling,
          max_retries = max_retries,
          progress_status = progress_status
        )
      },
      error = function(e) {
        message("Error reading file: ", e$message)
        writeLines <- base::paste0("Error reading file at line ", start_line, ": ", e$message)
        base::cat(writeLines, "\n", file = progress_status, append = TRUE)
        return(NULL)
      }
    )
    dataRelatedPair_merge <- read_result$data
    chunk_size <- read_result$chunk_size
    remove(read_result)

    if (is.null(dataRelatedPair_merge)) {
      message("No data read from file or error occurred. Skipping.")
      writeLines <- base::paste0("No data read from file or error occurred at line ", start_line)
      base::cat(writeLines, "\n", file = progress_status, append = TRUE)
      start_line <- start_line + chunk_size
      next
    }
    base::colnames(dataRelatedPair_merge) <- file_column_names

    for (i in 1:length(addRel_maxs)) {
      range_max <- addRel_maxs[i]
      range_min <- addRel_mins[i]

      if(mtdna  == TRUE) {
        if (verbose == TRUE) {
          message("Processing bin: ", range_min, " to ", range_max, " with mitRel = 1")
        }
      # filter the data for the current bin
      .write_bin_data(
        data = dataRelatedPair_merge,
        range_min = range_min,
        range_max = range_max,
        mit_val = 1,
        data_directory = data_directory,
        verbose = verbose
      )
      .write_bin_data(
        data = dataRelatedPair_merge,
        range_min = range_min,
        range_max = range_max,
        mit_val = 0,
        data_directory = data_directory,
        verbose = verbose
      )
      } else {
        if (verbose == TRUE) {
          message("Processing bin: ", range_min, " to ", range_max)
        }
        .write_bin_data(
          data = dataRelatedPair_merge,
          range_min = range_min,
          range_max = range_max,
          mit_val = NULL,
          data_directory = data_directory,
          verbose = verbose
        )
      }
    }


    base::message(start_line)
    df_nrows <- base::nrow(dataRelatedPair_merge)
    if (verbose == TRUE) {
      message("Processed ", df_nrows, " rows from lines ", start_line, " to ", end_line)
    }
    # update the line ranges for the next iteration
    end_line <- start_line + chunk_size - 1

    # check if reached end.
    if (df_nrows < chunk_size) {
      end_line <- start_line + df_nrows - 1
      break
    }

    end_time <- base::Sys.time()
    elapsed <- end_time - start_time
    total_lines <- total_lines + df_nrows


    # update start line for next iteration
    start_line <- end_line + 1

    base::message(start_line)

    progress_data <- base::data.frame(
      start_line = start_line,
      total_lines = total_lines,
      end_line = end_line,
      chunk_size = chunk_size
    )
    data.table::fwrite(progress_data,
      file = progress_csv,
      sep = ",",
      col.names = TRUE
    )
  }

  progress_status_conn <- base::file(progress_status, open = "w")
  end_time <- base::Sys.time()
  elapsed <- end_time - start_time
  # Create a status text summarizing the process
  statstext <- base::paste0(
    "Done!\n Total Lines read: ", total_lines,
    " \nInput File: ", input_file,
    " \nOutput Directory: ", data_directory,
    " \nBin Width: ", bin_width,
    " \nFinal chunk size: ", chunk_size,
    " \nStart Line: ", start_line,
    " \nEnd Line: ", end_line,
    " \nTime: ", elapsed,
    " \nCompleted at: ", base::Sys.time()
  )

  # Write the final status to the progress file
  base::cat(statstext, "\n", file = progress_status_conn, append = TRUE)

  # Close the progress file
  base::close(progress_status_conn)
  }

