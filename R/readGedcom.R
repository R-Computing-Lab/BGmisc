#' Read a GEDCOM File
#'
#' This function reads a GEDCOM file and parses it into a structured data frame of individuals.
#'
#' @param file_path The path to the GEDCOM file.
#' @param add_parents A logical value indicating whether to add parents to the data frame.
#' @param remove_empty_cols A logical value indicating whether to remove columns with all missing values.
#' @param combine_cols A logical value indicating whether to combine columns with duplicate values.
#' @param verbose A logical value indicating whether to print messages.
#' @param skinny A logical value indicating whether to return a skinny data frame.
#' @param update_rate numeric. The rate at which to print progress
#' @param ... Additional arguments to be passed to the function.
#' @return A data frame containing information about individuals, with the following potential columns:
#' - `id`: ID of the individual
#' - `momID`: ID of the individual's mother
#' - `dadID`: ID of the individual's father
#' - `sex`: Sex of the individual
#' - `name`: Full name of the individual
#' - `name_given`: First name of the individual
#' - `name_surn`: Last name of the individual
#' - `name_marriedsurn`: Married name of the individual
#' - `name_nick`: Nickname of the individual
#' - `name_npfx`: Name prefix
#' - `name_nsfx`: Name suffix
#' - `birth_date`: Birth date of the individual
#' - `birth_lat`: Latitude of the birthplace
#' - `birth_long`: Longitude of the birthplace
#' - `birth_place`: Birthplace of the individual
#' - `death_caus`: Cause of death
#' - `death_date`: Death date of the individual
#' - `death_lat`: Latitude of the place of death
#' - `death_long`: Longitude of the place of death
#' - `death_place`: Place of death of the individual
#' - `attribute_caste`: Caste of the individual
#' - `attribute_children`: Number of children of the individual
#' - `attribute_description`: Description of the individual
#' - `attribute_education`: Education of the individual
#' - `attribute_idnumber`: Identification number of the individual
#' - `attribute_marriages`: Number of marriages of the individual
#' - `attribute_nationality`: Nationality of the individual
#' - `attribute_occupation`: Occupation of the individual
#' - `attribute_property`: Property owned by the individual
#' - `attribute_religion`: Religion of the individual
#' - `attribute_residence`: Residence of the individual
#' - `attribute_ssn`: Social security number of the individual
#' - `attribute_title`: Title of the individual
#' - `FAMC`: ID(s) of the family where the individual is a child
#' - `FAMS`: ID(s) of the family where the individual is a spouse
#' @export
readGedcom.alpha <- function(file_path,
                       verbose = FALSE,
                       add_parents = TRUE,
                       remove_empty_cols = TRUE,
                       combine_cols = TRUE,
                       skinny = FALSE,
                       update_rate = 1000,
                       post_process = TRUE,
                       ...) {

  # Ensure the file exists and read all lines.
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  if (verbose) message("Reading file: ", file_path)
  lines <- readLines(file_path)
  total_lines <- length(lines)
  if (verbose) message("File is ", total_lines, " lines long")

  # Count pattern occurrences (pattern_rows remains used in subfunctions)
  pattern_rows <- countPatternRows.alpha(data.frame(X1 = lines))

  # List of variables to initialize
  all_var_names <- unlist(list(
    identifiers = c("id", "momID", "dadID"),
    names       = c("name", "name_given", "name_given_pieces", "name_surn", "name_surn_pieces", "name_marriedsurn",
                    "name_nick", "name_npfx", "name_nsfx"),
    sex         = c("sex"),
    birth       = c("birth_date", "birth_lat", "birth_long", "birth_place"),
    death       = c("death_caus", "death_date", "death_lat", "death_long", "death_place"),
    attributes  = c("attribute_caste", "attribute_children", "attribute_description", "attribute_education",
                    "attribute_idnumber", "attribute_marriages", "attribute_nationality", "attribute_occupation",
                    "attribute_property", "attribute_religion", "attribute_residence", "attribute_ssn",
                    "attribute_title"),
    relationships = c("FAMC", "FAMS")
  ), use.names = FALSE)

  # Split the file into blocks; each block corresponds to one individual.
  blocks <- splitIndividuals.alpha(lines, verbose)

  # Parse each individual block into a record (a named list)
  records <- lapply(blocks, parseIndividualBlock.alpha,
                    pattern_rows = pattern_rows,
                    all_var_names = all_var_names, verbose = verbose)

  # Remove any NULLs (if a block did not contain an individual id)
  records <- Filter(Negate(is.null), records)

  if (length(records) == 0) {
    warning("No people found in file")
    return(NULL)
  }

  # Convert the list of records to a data frame.
  df_temp <- do.call(rbind, lapply(records, function(rec) {
    as.data.frame(rec, stringsAsFactors = FALSE)
  }))

  if (verbose) message("File has ", nrow(df_temp), " people")

  # Run post-processing if requested.
  if (post_process) {
    if (verbose) message("Post-processing data frame")
    df_temp <- postProcessGedcom.alpha(
      df_temp = df_temp,
      remove_empty_cols = remove_empty_cols,
      combine_cols = combine_cols,
      add_parents = add_parents,
      skinny = skinny,
      verbose = verbose
    )
  }

  return(df_temp)
}

# --- SUBFUNCTIONS ---
#' Split GEDCOM Lines into Individual Blocks
#'
#' This function partitions the GEDCOM file (as a vector of lines) into a list of blocks,
#' where each block corresponds to a single individual starting with an "@ INDI" line.
#'
#' @param lines A character vector of lines from the GEDCOM file.
#' @param verbose Logical indicating whether to output progress messages.
#' @return A list of character vectors, each representing one individual.
splitIndividuals.alpha <- function(lines, verbose = FALSE) {
  indi_idx <- grep("@ INDI", lines)
  if (length(indi_idx) == 0) return(list())

  blocks <- list()
  for (i in seq_along(indi_idx)) {
    start <- indi_idx[i]
    end <- if (i < length(indi_idx)) indi_idx[i + 1] - 1 else length(lines)
    block <- lines[start:end]
    blocks[[length(blocks) + 1]] <- block
  }
  if (verbose) message("Found ", length(blocks), " individual blocks")
  return(blocks)
}

#' Initialize an Empty Individual Record
#'
#' Creates a named list with all GEDCOM fields set to NA.
#'
#' @param all_var_names A character vector of variable names.
#' @return A named list representing an empty individual record.
initializeRecord.alpha <- function(all_var_names) {
  setNames(as.list(rep(NA_character_, length(all_var_names))), all_var_names)
}

#' Parse a GEDCOM Individual Block
#'
#' Processes a block of GEDCOM lines corresponding to a single individual.
#'
#' @param block A character vector containing the GEDCOM lines for one individual.
#' @param pattern_rows A list with counts of lines matching specific GEDCOM tags.
#' @param all_var_names A character vector of variable names.
#' @param verbose Logical indicating whether to print progress messages.
#' @return A named list representing the parsed record for the individual, or NULL if no ID is found.
#' @keywords internal
parseIndividualBlock.alpha <- function(block, pattern_rows, all_var_names, verbose = FALSE) {
  record <- initializeRecord.alpha(all_var_names)
  n_lines <- length(block)

  # Loop through the block by index so that we can look ahead for event details.
  i <- 1
  while (i <= n_lines) {
    line <- block[i]

    # Process individual identifier (e.g., "@ INDI ...")
    if (grepl("@ INDI", line)) {
      record$id <- stringr::str_extract(line, "(?<=@.)\\d*(?=@)")
      i <- i + 1
      next
    }

    # Special processing for full name using " NAME" tag.
    if (grepl(" NAME", line) && pattern_rows$num_name_rows > 0) {
      record <- parseNameLine.alpha(line, record)
      i <- i + 1
      next
    }

    # Process birth and death events by consuming multiple lines.
    if (grepl(" BIRT", line) && pattern_rows$num_birt_rows > 0) {
      record <- processEventLine.alpha("birth", block, i, record, pattern_rows)
      i <- i + 1  # Skip further processing of this line.
      next
    }
    if (grepl(" DEAT", line) && pattern_rows$num_deat_rows > 0) {
      record <- processEventLine.alpha("death", block, i, record, pattern_rows)
      i <- i + 1
      next
    }

    # Process other tags using common mappings.
    # Define mappings for name pieces (if not handled by NAME tag).
    name_piece_mappings <- list(
      list(tag = "GIVN", field = "name_given_pieces", mode = "replace"),
      list(tag = "NPFX", field = "name_npfx", mode = "replace"),
      list(tag = "NICK", field = "name_nick", mode = "replace"),
      list(tag = "SURN", field = "name_surn_pieces", mode = "replace"),
      list(tag = "NSFX", field = "name_nsfx", mode = "replace"),
      list(tag = "_MARNM", field = "name_marriedsurn", mode = "replace")
    )
    out <- applyTagMappings.alpha(line, record, pattern_rows, name_piece_mappings)
    if (out$matched) { record <- out$record
    i <- i + 1
    next }

    # Process attribute tags.
    attribute_mappings <- list(
      list(tag = "SEX",  field = "sex", mode = "replace"),
      list(tag = "CAST", field = "attribute_caste", mode = "replace"),
      list(tag = "DSCR", field = "attribute_description", mode = "replace"),
      list(tag = "EDUC", field = "attribute_education", mode = "replace"),
      list(tag = "IDNO", field = "attribute_idnumber", mode = "replace"),
      list(tag = "NATI", field = "attribute_nationality", mode = "replace"),
      list(tag = "NCHI", field = "attribute_children", mode = "replace"),
      list(tag = "NMR",  field = "attribute_marriages", mode = "replace"),
      list(tag = "OCCU", field = "attribute_occupation", mode = "replace"),
      list(tag = "PROP", field = "attribute_property", mode = "replace"),
      list(tag = "RELI", field = "attribute_religion", mode = "replace"),
      list(tag = "RESI", field = "attribute_residence", mode = "replace"),
      list(tag = "SSN",  field = "attribute_ssn", mode = "replace"),
      list(tag = "TITL", field = "attribute_title", mode = "replace")
    )
    out <- applyTagMappings.alpha(line, record, pattern_rows, attribute_mappings)
    if (out$matched) { record <- out$record
    i <- i + 1
    next }

    # Process relationship tags, using a custom extractor.
    relationship_mappings <- list(
      list(tag = "FAMC", field = "FAMC", mode = "append",
           extractor = function(x) stringr::str_extract(x, "(?<=@.)\\d*(?=@)")),
      list(tag = "FAMS", field = "FAMS", mode = "append",
           extractor = function(x) stringr::str_extract(x, "(?<=@.)\\d*(?=@)"))
    )
    out <- applyTagMappings.alpha(line, record, pattern_rows, relationship_mappings)
    if (out$matched) { record <- out$record
    i <- i + 1
    next }

    # Optionally print progress for long records.
    i <- i + 1
  }

  # If the record has no ID, return NULL.
  if (is.na(record$id)) return(NULL)
  return(record)
}

#' Parse a Full Name Line
#'
#' Extracts full name information from a GEDCOM "NAME" line and updates the record accordingly.
#'
#' @param line A character string containing the name line.
#' @param record A named list representing the individual's record.
#' @return The updated record with parsed name information.
parseNameLine.alpha <- function(line, record) {
  record$name <- extract_info.alpha(line, "NAME")
  record$name_given <- stringr::str_extract(record$name, ".*(?= /)")
  record$name_surn <- stringr::str_extract(record$name, "(?<=/).*(?=/)")
  record$name <- stringr::str_squish(stringr::str_replace(record$name, "/", " "))
  return(record)
}

#' Process Event Lines (Birth or Death)
#'
#' Extracts event details (e.g., date, place, cause, latitude, longitude) from a block of GEDCOM lines.
#' For "birth": expect DATE on line i+1, PLAC on i+2, LATI on i+4, LONG on i+5.
#' For "death": expect DATE on line i+1, PLAC on i+2, CAUS on i+3, LATI on i+4, LONG on i+5.
#' @param event A character string indicating the event type ("birth" or "death").
#' @param block A character vector of GEDCOM lines.
#' @param i The current line index where the event tag is found.
#' @param record A named list representing the individual's record.
#' @param pattern_rows A list with counts of GEDCOM tag occurrences.
#' @return The updated record with parsed event information.#
# For "death": expect DATE on line i+1, PLAC on i+2, CAUS on i+3, LATI on i+4, LONG on i+5.
processEventLine.alpha <- function(event, block, i, record, pattern_rows) {
  n_lines <- length(block)
  if (event == "birth") {
    if (i + 1 <= n_lines) record$birth_date <- extract_info.alpha(block[i+1], "DATE")
    if (i + 2 <= n_lines) record$birth_place <- extract_info.alpha(block[i+2], "PLAC")
    if (i + 4 <= n_lines) record$birth_lat <- extract_info.alpha(block[i+4], "LATI")
    if (i + 5 <= n_lines) record$birth_long <- extract_info.alpha(block[i+5], "LONG")
  } else if (event == "death") {
    if (i + 1 <= n_lines) record$death_date <- extract_info.alpha(block[i+1], "DATE")
    if (i + 2 <= n_lines) record$death_place <- extract_info.alpha(block[i+2], "PLAC")
    if (i + 3 <= n_lines) record$death_caus <- extract_info.alpha(block[i+3], "CAUS")
    if (i + 4 <= n_lines) record$death_lat <- extract_info.alpha(block[i+4], "LATI")
    if (i + 5 <= n_lines) record$death_long <- extract_info.alpha(block[i+5], "LONG")
  }
  return(record)
}

#' Apply Tag Mappings to a Line
#'
#' Iterates over a list of tag mappings and, if a tag matches the line, updates the record.
#'
#' @param line A character string from the GEDCOM file.
#' @param record A named list representing the individual's record.
#' @param pattern_rows A list with GEDCOM tag counts.
#' @param tag_mappings A list of lists. Each sublist should define:
#'   - \code{tag}: the GEDCOM tag,
#'   - \code{field}: the record field to update,
#'   - \code{mode}: either "replace" or "append",
#'   - \code{extractor}: (optional) a custom extraction function.
#' @return A list with the updated record (\code{record}) and a logical flag (\code{matched}).
applyTagMappings.alpha <- function(line, record, pattern_rows, tag_mappings) {
  for (mapping in tag_mappings) {
    extractor <- if (is.null(mapping$extractor)) NULL else mapping$extractor
    result <- process_tag.alpha(mapping$tag, mapping$field, pattern_rows, line, record,
                          extractor = extractor, mode = mapping$mode)
    record <- result$vars
    if (result$matched) {
      return(list(record = record, matched = TRUE))
    }
  }
  return(list(record = record, matched = FALSE))
}


#' Extract Information from Line
#'
#' This function extracts information from a line based on a specified type.
#' @param line A character string representing a line from a GEDCOM file.
#' @param type A character string representing the type of information to extract.
#' @return A character string with the extracted information.
#' @keywords internal
extract_info.alpha <- function(line, type) {
  stringr::str_squish(stringr::str_extract(line, paste0("(?<=", type, " ).+")))
}

#' Count GEDCOM Pattern Rows
#'
#' Counts the number of lines in a file (passed as a data frame with column "X1")
#' that match various GEDCOM patterns.
#'
#' @param file A data frame with a column \code{X1} containing GEDCOM lines.
#' @return A list with counts of specific GEDCOM tag occurrences.
countPatternRows.alpha <- function(file) {
  pattern_counts <- sapply(
    c(
      "@ INDI", " NAME", " GIVN", " NPFX", " NICK", " SURN", " NSFX", " _MARNM",
      " BIRT", " DEAT", " SEX", " CAST", " DSCR", " EDUC", " IDNO", " NATI",
      " NCHI", " NMR", " OCCU", " PROP", " RELI", " RESI", " SSN", " TITL",
      " FAMC", " FAMS", " PLAC", " LATI", " LONG", " DATE", " CAUS"
    ),
    function(pat) sum(grepl(pat, file$X1))
  )
  num_rows <- list(
    num_indi_rows = pattern_counts["@ INDI"],
    num_name_rows = pattern_counts[" NAME"],
    num_givn_rows = pattern_counts[" GIVN"],
    num_npfx_rows = pattern_counts[" NPFX"],
    num_nick_rows = pattern_counts[" NICK"],
    num_surn_rows = pattern_counts[" SURN"],
    num_nsfx_rows = pattern_counts[" NSFX"],
    num_marnm_rows = pattern_counts[" _MARNM"],
    num_birt_rows = pattern_counts[" BIRT"],
    num_deat_rows = pattern_counts[" DEAT"],
    num_sex_rows = pattern_counts[" SEX"],
    num_cast_rows = pattern_counts[" CAST"],
    num_dscr_rows = pattern_counts[" DSCR"],
    num_educ_rows = pattern_counts[" EDUC"],
    num_idno_rows = pattern_counts[" IDNO"],
    num_nati_rows = pattern_counts[" NATI"],
    num_nchi_rows = pattern_counts[" NCHI"],
    num_nmr_rows = pattern_counts[" NMR"],
    num_occu_rows = pattern_counts[" OCCU"],
    num_prop_rows = pattern_counts[" PROP"],
    num_reli_rows = pattern_counts[" RELI"],
    num_resi_rows = pattern_counts[" RESI"],
    num_ssn_rows = pattern_counts[" SSN"],
    num_titl_rows = pattern_counts[" TITL"],
    num_famc_rows = pattern_counts[" FAMC"],
    num_fams_rows = pattern_counts[" FAMS"],
    num_plac_rows = pattern_counts[" PLAC"],
    num_lati_rows = pattern_counts[" LATI"],
    num_long_rows = pattern_counts[" LONG"],
    num_date_rows = pattern_counts[" DATE"],
    num_caus_rows = pattern_counts[" CAUS"]
  )
  return(num_rows)
}

#' Process a GEDCOM Tag
#'
#' Extracts and assigns a value to a specified field in `vars` if the pattern is present.
#' Returns both the updated variable list and a flag indicating whether the tag was matched.
#'
#' @param tag The GEDCOM tag (e.g., "SEX", "CAST", etc.).
#' @param field_name The name of the variable to assign to in `vars`.
#' @param pattern_rows Output from `countPatternRows()`.
#' @param line The GEDCOM line to parse.
#' @param vars The current list of variables to update.
#' @return A list with updated `vars` and a `matched` flag.
#' @keywords internal
process_tag.alpha <- function(tag, field_name, pattern_rows, line, vars,
                        extractor = NULL, mode = "replace") {
  count_name <- paste0("num_", tolower(tag), "_rows")
  matched <- FALSE
  if (!is.null(pattern_rows[[count_name]]) &&
      pattern_rows[[count_name]] > 0 &&
      grepl(paste0(" ", tag), line)) {
    value <- if (is.null(extractor)) {
      extract_info.alpha(line, tag)
    } else {
      extractor(line)
    }
    if (mode == "append" && !is.na(vars[[field_name]])) {
      vars[[field_name]] <- paste0(vars[[field_name]], ", ", value)
    } else {
      vars[[field_name]] <- value
    }
    matched <- TRUE
  }
  return(list(vars = vars, matched = matched))
}

#' Post-process GEDCOM Data Frame
#'
#' This function optionally adds parent information, combines duplicate columns,
#' and removes empty columns from the GEDCOM data frame.
#'
#' @param df_temp A data frame produced by \code{readGedcom()}.
#' @param remove_empty_cols Logical indicating whether to remove columns that are entirely missing.
#' @param combine_cols Logical indicating whether to combine columns with duplicate values.
#' @param add_parents Logical indicating whether to add parent information.
#' @param skinny Logical indicating whether to slim down the data frame.
#' @param verbose Logical indicating whether to print progress messages.
#' @return The post-processed data frame.
postProcessGedcom.alpha <- function(df_temp,
                              remove_empty_cols = TRUE,
                              combine_cols = TRUE,
                              add_parents = TRUE,
                              skinny = TRUE,
                              verbose = FALSE) {
  if (add_parents) {
    if (verbose) message("Processing parents")
    df_temp <- processParents.alpha(df_temp, datasource = "gedcom")
  }
  if (combine_cols) {
    df_temp <- collapseNames.alpha(verbose = verbose, df_temp = df_temp)
  }
  if (remove_empty_cols) {
    if (verbose) message("Removing empty columns")
    df_temp <- df_temp[, colSums(is.na(df_temp)) < nrow(df_temp)]
  }
  if (skinny) {
    if (verbose) message("Slimming down the data frame")
    df_temp <- df_temp[, colSums(is.na(df_temp)) < nrow(df_temp)]
    df_temp$FAMC <- NULL
    df_temp$FAMS <- NULL
  }
  return(df_temp)
}

#' Process Parents Information from GEDCOM Data
#'
#' Adds parent IDs to the individuals based on family relationship data.
#'
#' @param df_temp A data frame produced by \code{readGedcom()}.
#' @param datasource Character string indicating the data source ("gedcom" or "wiki").
#' @return The updated data frame with parent IDs added.
processParents.alpha <- function(df_temp, datasource) {
  if (datasource == "gedcom") {
    required_cols <- c("FAMC", "sex", "FAMS")
  } else if (datasource == "wiki") {
    required_cols <- c("id")
  } else {
    stop("Invalid datasource")
  }
  if (!all(required_cols %in% colnames(df_temp))) {
    missing_cols <- setdiff(required_cols, colnames(df_temp))
    warning("Missing necessary columns: ", paste(missing_cols, collapse = ", "))
    return(df_temp)
  }
  family_to_parents <- mapFAMS2parents.alpha(df_temp)
  if (is.null(family_to_parents) || length(family_to_parents) == 0) {
    return(df_temp)
  }
  df_temp <- mapFAMC2parents.alpha(df_temp, family_to_parents)
  return(df_temp)
}

#' Create a Mapping from Family IDs to Parent IDs
#'
#' This function scans the data frame and creates a mapping of family IDs
#' to the corresponding parent IDs.
#'
#' @param df_temp A data frame produced by \code{readGedcom()}.
#' @return A list mapping family IDs to parent information.
mapFAMS2parents.alpha <- function(df_temp) {
  if (!all(c("FAMS", "sex") %in% colnames(df_temp))) {
    warning("The data frame does not contain the necessary columns (FAMS, sex)")
    return(NULL)
  }
  family_to_parents <- list()
  for (i in 1:nrow(df_temp)) {
    if (!is.na(df_temp$FAMS[i])) {
      fams_ids <- unlist(strsplit(df_temp$FAMS[i], ", "))
      for (fams_id in fams_ids) {
        if (!is.null(family_to_parents[[fams_id]])) {
          if (df_temp$sex[i] == "M") {
            family_to_parents[[fams_id]]$father <- df_temp$id[i]
          } else if (df_temp$sex[i] == "F") {
            family_to_parents[[fams_id]]$mother <- df_temp$id[i]
          }
        } else {
          family_to_parents[[fams_id]] <- list()
          if (df_temp$sex[i] == "M") {
            family_to_parents[[fams_id]]$father <- df_temp$id[i]
          } else if (df_temp$sex[i] == "F") {
            family_to_parents[[fams_id]]$mother <- df_temp$id[i]
          }
        }
      }
    }
  }
  return(family_to_parents)
}

#' Assign momID and dadID based on family mapping
#'
#' This function assigns mother and father IDs to individuals in the data frame
#' based on the mapping of family IDs to parent IDs.
#'
#' @param df_temp A data frame containing individual information.
#' @param family_to_parents A list mapping family IDs to parent IDs.
#' @return A data frame with added momID and dad_ID columns.
#' @keywords internal
mapFAMC2parents.alpha <- function(df_temp, family_to_parents) {
  df_temp$momID <- NA_character_
  df_temp$dadID <- NA_character_
  for (i in 1:nrow(df_temp)) {
    if (!is.na(df_temp$FAMC[i])) {
      famc_ids <- unlist(strsplit(df_temp$FAMC[i], ", "))
      for (famc_id in famc_ids) {
        if (!is.null(family_to_parents[[famc_id]])) {
          if (!is.null(family_to_parents[[famc_id]]$father)) {
            df_temp$dadID[i] <- family_to_parents[[famc_id]]$father
          }
          if (!is.null(family_to_parents[[famc_id]]$mother)) {
            df_temp$momID[i] <- family_to_parents[[famc_id]]$mother
          }
        }
      }
    }
  }
  return(df_temp)
}

#' collapse Names
#'
#' This function combines the `name_given` and `name_given_pieces` columns in a data frame.
#'
#' @inheritParams readGedcom
#' @param df_temp A data frame containing the columns to be combined.
#' @return A data frame with the combined columns.
collapseNames.alpha <- function(verbose, df_temp) {
  if (verbose) message("Combining Duplicate Columns")

  if (!all(is.na(df_temp$name_given_pieces)) | !all(is.na(df_temp$name_given))) {
    result <- combine_columns.alpha(df_temp$name_given, df_temp$name_given_pieces)
    df_temp$name_given <- result$combined
    if (!result$retain_col2) df_temp$name_given_pieces <- NULL
  }

  if (!all(is.na(df_temp$name_surn_pieces)) | !all(is.na(df_temp$name_surn))) {
    result <- combine_columns.alpha(df_temp$name_surn, df_temp$name_surn_pieces)
    df_temp$name_surn <- result$combined
    if (!result$retain_col2) df_temp$name_surn_pieces <- NULL
  }
  return(df_temp)
}

#' Combine Columns
#'
#' This function combines two columns, handling conflicts and merging non-conflicting data.
#' @param col1 The first column to combine.
#' @param col2 The second column to combine.
#' @return A list with the combined column and a flag indicating if the second column should be retained.
#' @keywords internal
# Helper function to check for conflicts and merge columns
combine_columns.alpha <- function(col1, col2) {
  col1_lower <- stringr::str_to_lower(col1)
  col2_lower <- stringr::str_to_lower(col2)
  conflicts <- !is.na(col1_lower) & !is.na(col2_lower) & col1_lower != col2_lower
  if (any(conflicts)) {
    warning("Columns have conflicting values. They were not merged.")
    return(list(combined = col1, retain_col2 = TRUE))
  } else {
    combined <- ifelse(is.na(col1), col2, col1)
    return(list(combined = combined, retain_col2 = FALSE))
  }
}

# --- Exported Aliases ---
#' @rdname readGedcom.alpha
#' @export
readGed.alpha <- readGedcom.alpha
#' @rdname readGedcom.alpha
#' @export
readgedcom.alpha <- readGedcom.alpha
