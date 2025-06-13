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
#'
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
  input_file = NULL,
  progress_csv = "progress.csv",
  progress_status = "progress.txt",
  file_column_names= c("ID1", "ID2", "addRel", "mitRel", "cnuRel")
) {

  bin_width_string <- as.character(bin_width * 100)
    # Ensure the outcome_name directory exists
  if (biggest==TRUE && !base::dir.exists(file.path(outcome_name, "data", paste0("links_", bin_width_string)))) {
    dir.create(file.path(outcome_name, "data", paste0("links_", bin_width_string)),
               showWarnings = FALSE, recursive = TRUE)
  } else if (biggest==FALSE && !base::dir.exists(file.path(outcome_name,
                                                           "data", paste0("links_allbut_",
                                                                          bin_width_string)))) {
    dir.create(file.path(outcome_name, "data", paste0("links_allbut_", bin_width_string)),
               showWarnings = FALSE, recursive = TRUE)
  }

  # create binning ranges for additive relatedness
  addRel_center <- 2^(0:(-degreerelatedness))

  addRel_maxs_temp <- addRel_center * (1 + bin_width)
  # inclusive
  addRel_mins_temp <- addRel_center * (1 - bin_width)

  # this is supposed to have one of each
  addRel_real_maxs <- addRel_mins_temp[-length(addRel_mins_temp)]
  addRel_real_mins <- addRel_maxs_temp[-1]

  addRel_maxs <- c(1.5,
                   base::sort(c(addRel_real_maxs, addRel_maxs_temp), decreasing = TRUE),
                   addRel_mins_temp[length(addRel_mins_temp)])
  addRel_mins <- c(addRel_maxs_temp[1],
                   base::sort(c(addRel_real_mins, addRel_mins_temp), decreasing = TRUE),
                   0)

  start_line <- 1 + 1  # Start from the second line (skip header)
  end_line <- chunk_size + 1
  total_lines <- 0

  input_file <- if (biggest==TRUE && is.null(input_file)) {
    base::paste0(outcome_name, '_datacnmitBiggestRelatedPairs_2nddegree_take1.csv')
  } else if (biggest==FALSE && is.null(input_file)) {
    base::paste0(outcome_name, '_dataAllbutBiggestRelatedPairsTake2.csv')
  }

  if (base::file.exists(progress_csv)) {
    progress_data <- data.table::fread(progress_csv, header = TRUE)
    start_line <- progress_data$start_line
    total_lines <- progress_data$total_lines
  }

  start_time <- base::Sys.time()

  while (start_line <= max_lines) {

    dataRelatedPair_merge <- data.table::fread(input_file,
                                               skip = start_line - 1,
                                               nrows = chunk_size,
                                               header = FALSE,
                                               sep = ",",
                                               fill = TRUE)

    base::colnames(dataRelatedPair_merge) <- file_column_names

    for (i in 1:length(addRel_maxs)) {
      range_max <- addRel_maxs[i]
      range_min <- addRel_mins[i]

      range_data <- dataRelatedPair_merge[
        base::round(dataRelatedPair_merge$addRel, 6) >= range_min &
          base::round(dataRelatedPair_merge$addRel, 6) < range_max &
          dataRelatedPair_merge$mitRel == 1, ]

      if (base::nrow(range_data) > 0) {
        file_name <- if (biggest) {
          base::paste0(outcome_name, "/data/links_", bin_width_string,
                       "/df_mt1_r", range_min, "-r", range_max, ".csv")
        } else {
          base::paste0(outcome_name, "/data/links_allbut_", bin_width_string,
                       "/df_mt1_r", range_min, "-r", range_max, ".csv")
        }

        data.table::fwrite(range_data,
                           file = file_name,
                           sep = ",",
                           append = TRUE,
                           row.names = FALSE,
                           col.names = FALSE)
      }

      range_data <- dataRelatedPair_merge[
        base::round(dataRelatedPair_merge$addRel, 6) >= range_min &
          base::round(dataRelatedPair_merge$addRel, 6) < range_max &
          dataRelatedPair_merge$mitRel == 0, ]

      if (base::nrow(range_data) > 0) {
        file_name <- if (biggest) {
          base::paste0(outcome_name, "/data/links_", bin_width_string,
                       "/df_mt0_r", range_min, "-r", range_max, ".csv")
        } else {
          base::paste0(outcome_name, "/data/links_allbut_", bin_width_string,
                       "/df_mt0_r", range_min, "-r", range_max, ".csv")
        }

        data.table::fwrite(range_data,
                           file = file_name,
                           sep = ",",
                           append = TRUE,
                           row.names = FALSE,
                           col.names = FALSE)
      }
    }

    df_nrows <- base::nrow(dataRelatedPair_merge)

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

    base::print(start_line)

    #update start line for next iteration
    start_line <- end_line + 1

    base::print(start_line)

    progress_data <- base::data.frame(start_line = start_line,
                                      total_lines = total_lines)
    data.table::fwrite(progress_data,
                       file = progress_csv,
                       sep = ",",
                       col.names = TRUE)
  }

  progress_status_conn <- base::file(progress_status, open = "w")
  end_time <- base::Sys.time()
  elapsed <- end_time - start_time
  # Create a status text summarizing the process
  statstext <- base::paste0("Done!\n Total Lines read: ", total_lines,
                            " \nStart Line: ", start_line,
                            " \nEnd Line: ", end_line,
                            " \nTime: ", elapsed)

  # Write the final status to the progress file
  base::cat(statstext, "\n", file = progress_status_conn)

 #Close the progress file
  base::close(progress_status_conn)
}
