#' Read a GEDCOM File
#'
#' Ingests a GEDCOM genealogy file, identifies individual records, and parses
#' person-level identifiers, names, life events, attributes, and family
#' relationships into a structured data frame. Optional post-processing can infer
#' parental IDs from family relationships, reconcile redundant name fields, and
#' remove uninformative columns from the parsed output.
#'
#' @details
#' `readGedcom()` is a line-oriented parser tuned to common GEDCOM 5.5 and 5.5.1
#' structures. Individual records are identified from blocks that begin with an
#' `@ INDI` line. Each individual block is passed to an internal parser that uses
#' simple GEDCOM tag pattern matches to extract identifiers, names, life events,
#' attributes, and family relationships.
#'
#' Name information is parsed primarily from the GEDCOM `NAME` tag, which often
#' encodes given names and surnames using slash-delimited surname notation, such
#' as `NAME John /Smith/`. The parser extracts the given name, surname, and a
#' cleaned full name. Additional name components are parsed when present,
#' including name prefix, name suffix, nickname, and married surname.
#'
#' Birth and death events are recognized from `BIRT` and `DEAT` tags. Event
#' details are currently parsed using fixed offsets within the individual block.
#' For birth events, the parser expects `DATE` at `i + 1`, `PLAC` at `i + 2`,
#' `LATI` at `i + 4`, and `LONG` at `i + 5`. For death events, the parser
#' expects `DATE` at `i + 1`, `PLAC` at `i + 2`, `CAUS` at `i + 3`, `LATI` at
#' `i + 4`, and `LONG` at `i + 5`. Missing elements leave the corresponding
#' output fields as `NA`.
#'
 #' Attribute tags such as `OCCU`, `EDUC`, `RELI`, `CAST`, `NCHI`, `NMR`, `NATI`,
#' `RESI`, `PROP`, `SSN`, `TITL`, `DSCR`, and `IDNO` are parsed directly into
#' dedicated columns prefixed with `attribute_`.
#'
#' Family relationships are parsed from `FAMC` and `FAMS` tags. `FAMC` identifies
#' the family in which an individual is a child, and `FAMS` identifies families
#' in which an individual is a spouse. These raw family identifiers are retained
#' in the parsed output unless removed during post-processing. When
#' `add_parents = TRUE`, they are also used to infer `momID` and `dadID`.
#'
#' If `post_process = TRUE`, `readGedcom()` applies optional cleanup steps
#' controlled by `add_parents`, `combine_cols`, `remove_empty_cols`, and
#' `skinny`. These steps can infer parent IDs, collapse redundant name fields,
#' remove columns that are entirely missing, and drop raw family relationship
#' columns for a slimmer output.
#'
#' @param file_path Character string. Path to the GEDCOM file.
#' @param verbose Logical. If `TRUE`, print progress messages.
#' @param add_parents Logical. If `TRUE`, infer `momID` and `dadID` from `FAMC`
#'   and `FAMS` mappings during post-processing.
#' @param remove_empty_cols Logical. If `TRUE`, drop columns that are entirely
#'   `NA` during post-processing.
#' @param combine_cols Logical. If `TRUE`, combine redundant name columns, such
#'   as `name_given` with `name_given_pieces` and `name_surn` with
#'   `name_surn_pieces`, when their values do not conflict.
#' @param skinny Logical. If `TRUE`, return a slimmer data frame by dropping
#'   `FAMC`, `FAMS`, and columns that are entirely `NA` during post-processing.
#' @param update_rate Numeric. Intended rate at which progress messages should
#'   be printed. Currently unused.
#' @param post_process Logical. If `TRUE`, apply post-processing steps controlled
#'   by `add_parents`, `combine_cols`, `remove_empty_cols`, and `skinny`.
#' @param ... Additional arguments. Currently unused.
#' @return A data frame containing information about individuals, with the following potential columns:
#' \describe{
#'   \item{personID}{Individual ID parsed from the `@ INDI` line.}
#'   \item{momID}{ID of the individual's mother, if inferred.}
#'   \item{dadID}{ID of the individual's father, if inferred.}
#'   \item{sex}{Sex of the individual.}
#'   \item{name}{Cleaned full name of the individual.}
#'   \item{name_given}{Given name parsed from the `NAME` tag.}
#'   \item{name_given_pieces}{Given name parsed from a separate `GIVN` tag, if present.}
#'   \item{name_surn}{Surname parsed from the `NAME` tag.}
#'   \item{name_surn_pieces}{Surname parsed from a separate `SURN` tag, if present.}
#'   \item{name_marriedsurn}{Married surname parsed from `_MARNM`, if present.}
#'   \item{name_nick}{Nickname parsed from `NICK`, if present.}
#'   \item{name_npfx}{Name prefix parsed from `NPFX`, if present.}
#'   \item{name_nsfx}{Name suffix parsed from `NSFX`, if present.}
#'   \item{birth_date}{Birth date of the individual.}
#'   \item{birth_lat}{Latitude of the birthplace.}
#'   \item{birth_long}{Longitude of the birthplace.}
#'   \item{birth_place}{Birthplace of the individual.}
#'   \item{death_caus}{Cause of death.}
#'   \item{death_date}{Death date of the individual.}
#'   \item{death_lat}{Latitude of the place of death.}
#'   \item{death_long}{Longitude of the place of death.}
#'   \item{death_place}{Place of death of the individual.}
#'   \item{attribute_caste}{Caste of the individual.}
#'   \item{attribute_children}{Number of children of the individual.}
#'   \item{attribute_description}{Description of the individual.}
#'   \item{attribute_education}{Education of the individual.}
#'   \item{attribute_idnumber}{Identification number of the individual.}
#'   \item{attribute_marriages}{Number of marriages of the individual.}
#'   \item{attribute_nationality}{Nationality of the individual.}
#'   \item{attribute_occupation}{Occupation of the individual.}
#'   \item{attribute_property}{Property owned by the individual.}
#'   \item{attribute_religion}{Religion of the individual.}
#'   \item{attribute_residence}{Residence of the individual.}
#'   \item{attribute_ssn}{Social Security number of the individual.}
#'   \item{attribute_title}{Title of the individual.}
#'   \item{FAMC}{ID or IDs of the family in which the individual is a child.}
#'   \item{FAMS}{ID or IDs of families in which the individual is a spouse.}
#' }
#'
#' If no individual records are found, the function returns `NULL` with a
#' warning.
#' @export
#'
readGedcom <- function(file_path,
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
  if (verbose == TRUE) message("Reading file: ", file_path)
  lines <- readLines(file_path)
  total_lines <- length(lines)
  if (verbose == TRUE) message("File is ", total_lines, " lines long")

  # Count pattern occurrences (pattern_rows remains used in subfunctions)
  pattern_rows <- countPatternRows(data.frame(X1 = lines))

  # List of variables to initialize
  all_var_names <- unlist(list(
    identifiers = c("personID", "momID", "dadID"),
    names = c(
      "name", "name_given", "name_given_pieces",
      "name_surn", "name_surn_pieces", "name_marriedsurn",
      "name_nick", "name_npfx", "name_nsfx"
    ),
    sex = c("sex"),
    birth = c("birth_date", "birth_lat", "birth_long", "birth_place"),
    death = c("death_caus", "death_date", "death_lat", "death_long", "death_place"),
    attributes = c(
      "attribute_caste", "attribute_children",
      "attribute_description", "attribute_education",
      "attribute_idnumber", "attribute_marriages",
      "attribute_nationality", "attribute_occupation",
      "attribute_property", "attribute_religion",
      "attribute_residence", "attribute_ssn",
      "attribute_title"
    ),
    relationships = c("FAMC", "FAMS")
  ), use.names = FALSE)

  # Split the file into blocks; each block corresponds to one individual.
  blocks <- splitIndividuals(lines, verbose)

  # Parse each individual block into a record (a named list)
  records <- lapply(blocks, parseIndividualBlock,
    pattern_rows = pattern_rows,
    all_var_names = all_var_names, verbose = verbose
  )

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

  if (verbose == TRUE) message("File has ", nrow(df_temp), " people")

  # Run post-processing if requested.
  if (post_process) {
    if (verbose == TRUE) message("Post-processing data frame")
    df_temp <- postProcessGedcom(
      df_temp = df_temp,
      remove_empty_cols = remove_empty_cols,
      combine_cols = combine_cols,
      add_parents = add_parents,
      skinny = skinny,
      verbose = verbose
    )
  }

  df_temp
}

# --- SUBFUNCTIONS ---
#' Split GEDCOM Lines into Individual Blocks
#'
#' @description
#' This function partitions the GEDCOM file (as a vector of lines) into a list of blocks,
#' where each block corresponds to a single individual starting with an "@ INDI" line.

#' @details Each block runs until the next "@ INDI" line or end-of-file.
#' Blocks are raw subsets of the file; no parsing occurs here.
#'
#' @param lines A character vector of lines from the GEDCOM file.
#' @param verbose Logical indicating whether to output progress messages.
#' @return A list of character vectors, each representing one individual.
#' @keywords internal
#'
splitIndividuals <- function(lines, verbose = FALSE) {
  indi_idx <- grep("@ INDI", lines)
  if (length(indi_idx) == 0) {
    return(list())
  }
  record_idx <- grep("@ (INDI|FAM|SOUR|REPO|OBJE|SUBM|SUBN|NOTE|_MTCAT)\\b| TRLR\\b", lines)


  blocks <- list()
  for (i in seq_along(indi_idx)) {
    start <- indi_idx[i]

    next_record <- record_idx[record_idx > start]

    end <- if (length(next_record) > 0) {
      next_record[1] - 1
    } else {
      length(lines)
    }
    block <- lines[start:end]
    blocks[[length(blocks) + 1]] <- block
  }
  if (verbose == TRUE) message("Found ", length(blocks), " individual blocks")
  blocks
}

#' Initialize an Empty Individual Record
#'
#' @description Creates a named list with all GEDCOM initialized to NA_character_.
#'
#' @param all_var_names A character vector of variable names.
#' @return A named list representing an empty individual record.
#' @importFrom stats setNames
initializeRecord <- function(all_var_names) {
  stats::setNames(as.list(rep(NA_character_, length(all_var_names))), all_var_names)
}

#' Parse a GEDCOM Individual Block
#'
#' @description Processes a block of GEDCOM lines corresponding to a single individual.
#'
#' @param block A character vector containing the GEDCOM lines for one individual.
#' @param pattern_rows A list with counts of lines matching specific GEDCOM tags.
#' @param all_var_names A character vector of variable names.
#' @param verbose Logical indicating whether to print progress messages.
#' @return A named list representing the parsed record for the individual, or NULL if no ID is found.
#' @keywords internal
parseIndividualBlock <- function(block, pattern_rows, all_var_names, verbose = FALSE) {
  record <- initializeRecord(all_var_names)
  n_lines <- length(block)

  # Loop through the block by index so that we can look ahead for event details.
  i <- 1
  while (i <= n_lines) {
    line <- block[i]

    # Process individual identifier (e.g., "@ INDI ...")
    if (grepl("@ INDI", line)) {
      record$personID <- stringr::str_extract(line, "(?<=@.)\\d*(?=@)")
      i <- i + 1
      next
    }

    # Special processing for full name using " NAME" tag.
    if (grepl(" NAME", line) && pattern_rows$num_name_rows > 0) {
      record <- parseNameLine(line, record)
      i <- i + 1
      next
    }

    # Process birth and death events by consuming multiple lines.
    if (grepl(" BIRT", line) && pattern_rows$num_birt_rows > 0) {
      record <- processEventLine("birth", block, i, record, pattern_rows)
      i <- i + 1 # Skip further processing of this line.
      next
    }
    if (grepl(" DEAT", line) && pattern_rows$num_deat_rows > 0) {
      record <- processEventLine("death", block, i, record, pattern_rows)
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
    out <- applyTagMappings(line, record, pattern_rows, name_piece_mappings)
    if (out$matched) {
      record <- out$record
      i <- i + 1
      next
    }

    # Process attribute tags.
    attribute_mappings <- list(
      list(tag = "SEX", field = "sex", mode = "replace"),
      list(tag = "CAST", field = "attribute_caste", mode = "replace"),
      list(tag = "DSCR", field = "attribute_description", mode = "replace"),
      list(tag = "EDUC", field = "attribute_education", mode = "replace"),
      list(tag = "IDNO", field = "attribute_idnumber", mode = "replace"),
      list(tag = "NATI", field = "attribute_nationality", mode = "replace"),
      list(tag = "NCHI", field = "attribute_children", mode = "replace"),
      list(tag = "NMR", field = "attribute_marriages", mode = "replace"),
      list(tag = "OCCU", field = "attribute_occupation", mode = "replace"),
      list(tag = "PROP", field = "attribute_property", mode = "replace"),
      list(tag = "RELI", field = "attribute_religion", mode = "replace"),
      list(tag = "RESI", field = "attribute_residence", mode = "replace"),
      list(tag = "SSN", field = "attribute_ssn", mode = "replace"),
      list(tag = "TITL", field = "attribute_title", mode = "replace")
    )
    out <- applyTagMappings(line, record, pattern_rows, attribute_mappings)
    if (out$matched) {
      record <- out$record
      i <- i + 1
      next
    }

    # Process relationship tags, using a custom extractor.
    relationship_mappings <- list(
      list(
        tag = "FAMC", field = "FAMC", mode = "append",
        extractor = function(x) stringr::str_extract(x, "(?<=@.)\\d*(?=@)")
      ),
      list(
        tag = "FAMS", field = "FAMS", mode = "append",
        extractor = function(x) stringr::str_extract(x, "(?<=@.)\\d*(?=@)")
      )
    )
    out <- applyTagMappings(line, record, pattern_rows, relationship_mappings)
    if (out$matched) {
      record <- out$record
      i <- i + 1
      next
    }

    # Optionally print progress for long records.
    i <- i + 1
  }

  # If the record has no ID, return NULL.
  if (is.na(record$personID)) {
    return(NULL)
  }
  record
}

#' @title Parse Name Line
#'
#' @description Extracts full name information from a GEDCOM "NAME" line and updates the record accordingly.
#'
#' @param line A character string containing the name line.
#' @param record A named list representing the individual's record.
#' @return The updated record with parsed name information.
parseNameLine <- function(line, record) {
  record$name <- extract_info(line, "NAME")
  record$name_given <- stringr::str_extract(record$name, ".*(?= /)")
  record$name_surn <- stringr::str_extract(record$name, "(?<=/).*(?=/)")
  record$name <- stringr::str_squish(stringr::str_replace(record$name, "/", " "))
  record
}

#' Process Event Lines (Birth or Death)
#'
#' @description Extracts event details (e.g., date, place, cause, latitude, longitude) from a block of GEDCOM lines.
#' For "birth": expect DATE on line i+1, PLAC on i+2, LATI on i+4, LONG on i+5.
#' For "death": expect DATE on line i+1, PLAC on i+2, CAUS on i+3, LATI on i+4, LONG on i+5.
#' @param event A character string indicating the event type ("birth" or "death").
#' @param block A character vector of GEDCOM lines.
#' @param i The current line index where the event tag is found.
#' @param record A named list representing the individual's record.
#' @param pattern_rows A list with counts of GEDCOM tag occurrences.
#' @return The updated record with parsed event information.#
# For "death": expect DATE on line i+1, PLAC on i+2, CAUS on i+3, LATI on i+4, LONG on i+5.
processEventLine <- function(event, block, i, record, pattern_rows) {
  n_lines <- length(block)
  if (event == "birth") {
    if (i + 1 <= n_lines) record$birth_date <- extract_info(block[i + 1], "DATE")
    if (i + 2 <= n_lines) record$birth_place <- extract_info(block[i + 2], "PLAC")
    if (i + 4 <= n_lines) record$birth_lat <- extract_info(block[i + 4], "LATI")
    if (i + 5 <= n_lines) record$birth_long <- extract_info(block[i + 5], "LONG")
  } else if (event == "death") {
    if (i + 1 <= n_lines) record$death_date <- extract_info(block[i + 1], "DATE")
    if (i + 2 <= n_lines) record$death_place <- extract_info(block[i + 2], "PLAC")
    if (i + 3 <= n_lines) record$death_caus <- extract_info(block[i + 3], "CAUS")
    if (i + 4 <= n_lines) record$death_lat <- extract_info(block[i + 4], "LATI")
    if (i + 5 <= n_lines) record$death_long <- extract_info(block[i + 5], "LONG")
  }
  record
}

#' Apply Tag Mappings to a Line
#'
#' @description Iterates over a list of tag mappings and, if a tag matches the line, updates the record.
#' Stops after the first match.
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
#'
applyTagMappings <- function(line, record, pattern_rows, tag_mappings) {
  for (mapping in tag_mappings) {
    extractor <- if (is.null(mapping$extractor)) NULL else mapping$extractor
    result <- process_tag(mapping$tag, mapping$field, pattern_rows, line, record,
      extractor = extractor, mode = mapping$mode
    )
    record <- result$vars
    if (result$matched) {
      return(list(record = record, matched = TRUE))
    }
  }
  list(record = record, matched = FALSE)
}


#' Extract Information from Line
#'
#' @description
#' Extracts the relevant information from a GEDCOM line based on the specified type.
#' The function uses regular expressions to locate and return the desired data.
#'
#' @param line A character string representing a line from a GEDCOM file.
#' @param type A character string representing the type of information to extract.
#' @return A character string with the extracted information.
#' @keywords internal
extract_info <- function(line, type) {
  stringr::str_squish(stringr::str_extract(line, paste0("(?<=", type, " ).+")))
}

#' Count GEDCOM Pattern Rows
#'
#' @description
#' Counts the number of lines in a file (passed as a data frame with column "X1")
#' that match various GEDCOM patterns. Returns a list with counts for each pattern.
#'
#' @param file A data frame with a column \code{X1} containing GEDCOM lines.
#' @return A list with counts of specific GEDCOM tag occurrences.
countPatternRows <- function(file) {
  x <- file$X1
  pattern_counts <- vapply(
    c(
      "@ INDI", " NAME", " GIVN", " NPFX", " NICK", " SURN", " NSFX", " _MARNM",
      " BIRT", " DEAT", " SEX", " CAST", " DSCR", " EDUC", " IDNO", " NATI",
      " NCHI", " NMR", " OCCU", " PROP", " RELI", " RESI", " SSN", " TITL",
      " FAMC", " FAMS", " PLAC", " LATI", " LONG", " DATE", " CAUS"
    ),
    function(pat) sum(grepl(pat, x, fixed = TRUE)),
    integer(1L)
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
  num_rows
}

#' Process a GEDCOM Tag
#'
#' @description
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
process_tag <- function(tag, field_name, pattern_rows, line, vars,
                        extractor = NULL, mode = "replace") {
  count_name <- paste0("num_", tolower(tag), "_rows")
  matched <- FALSE
  if (!is.null(pattern_rows[[count_name]]) &&
    pattern_rows[[count_name]] > 0 &&
    grepl(paste0(" ", tag), line)) {
    value <- if (is.null(extractor)) {
      extract_info(line, tag)
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
  list(vars = vars, matched = matched)
}

#' Post-process GEDCOM Data Frame
#'
#' @description This function optionally adds parent information, combines duplicate columns,
#' and removes empty columns from the GEDCOM data frame. It is called by \code{readGedcom()} if \code{post_process = TRUE}.
#'
#' @param df_temp A data frame produced by \code{readGedcom()}.
#' @param remove_empty_cols Logical indicating whether to remove columns that are entirely missing.
#' @param combine_cols Logical indicating whether to combine columns with duplicate values.
#' @param add_parents Logical indicating whether to add parent information.
#' @param skinny Logical indicating whether to slim down the data frame.
#' @param verbose Logical indicating whether to print progress messages.
#' @return The post-processed data frame.
postProcessGedcom <- function(df_temp,
                              remove_empty_cols = TRUE,
                              combine_cols = TRUE,
                              add_parents = TRUE,
                              skinny = TRUE,
                              verbose = FALSE) {
  if (add_parents == TRUE) {
    if (verbose == TRUE) message("Processing parents")
    df_temp <- processParents(df_temp, datasource = "gedcom")
  }
  if (combine_cols == TRUE) {
    df_temp <- collapseNames(verbose = verbose, df_temp = df_temp)
  }
  if (remove_empty_cols == TRUE) {
    if (verbose == TRUE) message("Removing empty columns")
    df_temp <- df_temp[, colSums(is.na(df_temp)) < nrow(df_temp)]
  }
  if (skinny == TRUE) {
    if (verbose == TRUE) message("Slimming down the data frame")
    # Remove columns that are entirely NA
    df_temp <- df_temp[, colSums(is.na(df_temp)) < nrow(df_temp)]
    # Remove raw family relationship columns
    df_temp$FAMC <- NULL
    df_temp$FAMS <- NULL
  }
  df_temp
}

#' Process Parents Information from GEDCOM Data
#'
#' @description This function adds mother and father IDs to individuals in the data frame
#'
#' @param df_temp A data frame produced by \code{readGedcom()}.
#' @param datasource Character string indicating the data source ("gedcom" or "wiki").
#' @return The updated data frame with parent IDs added.
processParents <- function(df_temp, datasource) {
  if (datasource == "gedcom") {
    required_cols <- c("FAMC", "sex", "FAMS")
  } else if (datasource == "wiki") {
    required_cols <- c("personID")
  } else {
    stop("Invalid datasource")
  }
  if (!all(required_cols %in% colnames(df_temp))) {
    missing_cols <- setdiff(required_cols, colnames(df_temp))
    warning("Missing necessary columns: ", paste(missing_cols, collapse = ", "))
    return(df_temp)
  }
  family_to_parents <- mapFAMS2parents(df_temp)
  if (is.null(family_to_parents) || length(family_to_parents) == 0) {
    return(df_temp)
  }
  df_temp <- mapFAMC2parents(df_temp, family_to_parents)
  df_temp
}

#' Create a Mapping from Family IDs to Parent IDs
#'
#' This function scans the data frame and creates a mapping of family IDs
#' to the corresponding parent IDs.
#'
#' @param df_temp A data frame produced by \code{readGedcom()}.
#' @return A list mapping family IDs to parent information.
mapFAMS2parents <- function(df_temp) {
  if (!all(c("FAMS", "sex") %in% colnames(df_temp))) {
    warning("The data frame does not contain the necessary columns (FAMS, sex)")
    return(NULL)
  }
  family_to_parents <- list()
  for (i in seq_len(nrow(df_temp))) {
    if (!is.na(df_temp$FAMS[i])) {
      fams_ids <- unlist(strsplit(df_temp$FAMS[i], ", "))
      for (fams_id in fams_ids) {
        if (!is.null(family_to_parents[[fams_id]])) {
          if (df_temp$sex[i] == "M") {
            family_to_parents[[fams_id]]$father <- df_temp$personID[i]
          } else if (df_temp$sex[i] == "F") {
            family_to_parents[[fams_id]]$mother <- df_temp$personID[i]
          }
        } else {
          family_to_parents[[fams_id]] <- list()
          if (df_temp$sex[i] == "M") {
            family_to_parents[[fams_id]]$father <- df_temp$personID[i]
          } else if (df_temp$sex[i] == "F") {
            family_to_parents[[fams_id]]$mother <- df_temp$personID[i]
          }
        }
      }
    }
  }
  family_to_parents
}

#' Assign momID and dadID based on family mapping
#'
#' This function assigns mother and father IDs to individuals in the data frame
#' based on the mapping of family IDs to parent IDs. It updates the data frame in place.
#'
#' @param df_temp A data frame containing individual information.
#' @param family_to_parents A list mapping family IDs to parent IDs.
#' @return A data frame with added momID and dad_ID columns.
#' @keywords internal
mapFAMC2parents <- function(df_temp, family_to_parents) {
  df_temp$momID <- NA_character_
  df_temp$dadID <- NA_character_
  for (i in seq_len(nrow(df_temp))) {
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
  df_temp
}

#' collapse Names
#'
#' This function combines the `name_given` and `name_given_pieces` columns in a data frame. If both columns have non-missing values that differ, a warning is issued and the original `name_given` is retained. If one column is missing, the other is used. The same logic applies to the `name_surn` and `name_surn_pieces` columns.
#'
#' @inheritParams readGedcom
#' @param df_temp A data frame containing the columns to be combined.
#' @return A data frame with the combined columns.
collapseNames <- function(verbose, df_temp) {
  if (verbose == TRUE) message("Combining Duplicate Columns")

  if (!all(is.na(df_temp$name_given_pieces)) || !all(is.na(df_temp$name_given))) {
    result <- combine_columns(df_temp$name_given, df_temp$name_given_pieces)
    df_temp$name_given <- result$combined
    if (!result$retain_col2) df_temp$name_given_pieces <- NULL
  }

  if (!all(is.na(df_temp$name_surn_pieces)) || !all(is.na(df_temp$name_surn))) {
    result <- combine_columns(df_temp$name_surn, df_temp$name_surn_pieces)
    df_temp$name_surn <- result$combined
    if (!result$retain_col2) df_temp$name_surn_pieces <- NULL
  }
  df_temp
}

#' Combine Columns
#'
#' This function combines two columns, handling conflicts and merging non-conflicting data.
#' @param col1 The first column to combine.
#' @param col2 The second column to combine.
#' @return A list with the combined column and a flag indicating if the second column should be retained.
#' @keywords internal
# Helper function to check for conflicts and merge columns
combine_columns <- function(col1, col2) {
  col1_lower <- stringr::str_to_lower(col1)
  col2_lower <- stringr::str_to_lower(col2)
  conflicts <- !is.na(col1_lower) & !is.na(col2_lower) & col1_lower != col2_lower
  if (any(conflicts)) {
    warning("Columns have conflicting values. They were not merged.")
    list(combined = col1, retain_col2 = TRUE)
  } else {
    combined <- ifelse(is.na(col1), col2, col1)
    list(combined = combined, retain_col2 = FALSE)
  }
}

# --- Exported Aliases ---
#' @rdname readGedcom
#' @export
readGed <- readGedcom
#' @rdname readGedcom
#' @export
readgedcom <- readGedcom
