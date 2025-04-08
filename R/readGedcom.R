#' Read a GEDCOM File
#'
#' This function reads a GEDCOM file and parses it into a structured data frame of individuals.
#' Inspired by https://raw.githubusercontent.com/jjfitz/readgedcom/master/R/read_gedcom.R
#'
#' @param file_path The path to the GEDCOM file.
#' @param add_parents A logical value indicating whether to add parents to the data frame.
#' @param remove_empty_cols A logical value indicating whether to remove columns with all missing values.
#' @param combine_cols A logical value indicating whether to combine columns with duplicate values.
#' @param verbose A logical value indicating whether to print messages.
#' @param skinny A logical value indicating whether to return a skinny data frame.
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
readGedcom <- function(file_path,
                       verbose = FALSE,
                       add_parents = TRUE,
                       remove_empty_cols = TRUE,
                       combine_cols = TRUE,
                       skinny = FALSE,
                       ...) {
  # Checks
  if (!file.exists(file_path)) stop("File does not exist: ", file_path)
  if (verbose) {
    print(paste("Reading file:", file_path))
  }
  file <- data.frame(X1 = readLines(file_path))
  file_length <- nrow(file)
  if (verbose) {
    print(paste0("File is ", file_length, " lines long"))
  }

  # Count the number of rows containing specific patterns
  num_rows <- countPatternRows(file)

  # List of variables to initialize
  var_names <- list(
    identifiers = c("id", "momID", "dadID"),
    names = c(
      "name", "name_given", "name_given_pieces",
      "name_surn", "name_surn_pieces", "name_marriedsurn", "name_nick", "name_npfx", "name_nsfx"
    ),
    sex = c("sex"),
    birth = c("birth_date", "birth_lat", "birth_long", "birth_place"),
    death = c("death_caus", "death_date", "death_lat", "death_long", "death_place"),
    attributes = c(
      "attribute_caste", "attribute_children", "attribute_description",
      "attribute_education", "attribute_idnumber", "attribute_marriages",
      "attribute_nationality", "attribute_occupation",
      "attribute_property", "attribute_religion", "attribute_residence",
      "attribute_ssn", "attribute_title"
    ),
    relationships = c("FAMC", "FAMS")
  )

  all_var_names <- unlist(var_names, use.names = FALSE)

  # Initialize all variables to NA
  vars <- stats::setNames(as.list(rep(NA_character_, length(all_var_names))), all_var_names)

  df_temp <- as.data.frame(matrix(nrow = 1, ncol = length(all_var_names)))
  names(df_temp) <- all_var_names

  if (verbose) {
    print("Parsing GEDCOM file")
  }
  for (i in 1:length(file[1][[1]])) {
    tmpv <- file[1][[1]][[i]]

    if (grepl("@ INDI", tmpv)) {
      line_to_write <- as.data.frame(vars)
      df_temp <- rbind(df_temp, line_to_write)

      # Reset all variables to NA
      vars <- stats::setNames(as.list(rep(NA_character_, length(all_var_names))), all_var_names)

      vars$id <- stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)")
      next
    }

    # names
    if (num_rows$num_name_rows > 0 && grepl(" NAME", tmpv)) {
      vars$name <- extract_info(tmpv, "NAME")
      vars$name_given <- stringr::str_extract(vars$name, ".*(?= /)")
      vars$name_surn <- stringr::str_extract(vars$name, "(?<=/).*(?=/)")
      vars$name <- stringr::str_squish(stringr::str_replace(vars$name, "/", " "))
      next
    }
    # PERSONAL_NAME_PIECES := NAME | NPFX | GIVN | NICK | SPFX | SURN | NSFX
    result <- process_tag("GIVN", "name_given_pieces", num_rows, tmpv, vars)
    vars <- result$vars
    if (result$matched) next

    # npfx := Name Prefix
    result <- process_tag("NPFX", "name_npfx", num_rows, tmpv, vars)
    vars <- result$vars
    if (result$matched) next

    # NICK := Nickname
    result <- process_tag("NICK", "name_nick", num_rows, tmpv, vars)
    vars <- result$vars
    if (result$matched) next

    # surn := Surname
    result <- process_tag("SURN", "name_surn_pieces", num_rows, tmpv, vars)
    vars <- result$vars
    if (result$matched) next

    # nsfx := Name suffix
    result <- process_tag("NSFX", "name_nsfx", num_rows, tmpv, vars)
    vars <- result$vars
    if (result$matched) next

    result <- process_tag("_MARNM", "name_marriedsurn", num_rows, tmpv, vars)
    vars <- result$vars
    if (result$matched) next

    # Birth event related information
    if (num_rows$num_birt_rows > 0 && grepl(" BIRT", tmpv)) {
      if (num_rows$num_date_rows > 0 && i + 1 <= file_length) {
        vars$birth_date <- extract_info(file[1][[1]][[i + 1]], "DATE")
        if (num_rows$num_plac_rows > 0 && i + 2 <= file_length) {
          vars$birth_place <- extract_info(file[1][[1]][[i + 2]], "PLAC")
          if (num_rows$num_lati_rows > 0 && i + 4 <= file_length) {
            vars$birth_lat <- extract_info(file[1][[1]][[i + 4]], "LATI")
            if (num_rows$num_long_rows > 0 && i + 5 <= file_length) {
              vars$birth_long <- extract_info(file[1][[1]][[i + 5]], "LONG")
            }
          }
        }
      }
      next
    }

    # Death event related information
    # the ifs are nested so that there is no need to check if you've already run out of
    if (num_rows$num_deat_rows > 0 && grepl(" DEAT", tmpv)) {
      if (num_rows$num_date_rows > 0 && i + 1 <= file_length) {
        vars$death_date <- extract_info(file[1][[1]][[i + 1]], "DATE")
        if (num_rows$num_plac_rows > 0 && i + 2 <= file_length) {
          vars$death_place <- extract_info(file[1][[1]][[i + 2]], "PLAC")
          if (num_rows$num_caus_rows > 0 && i + 3 <= file_length) {
            vars$death_caus <- extract_info(file[1][[1]][[i + 3]], "CAUS")
            if (num_rows$num_lati_rows > 0 && i + 4 <= file_length) {
              vars$death_lat <- extract_info(file[1][[1]][[i + 4]], "LATI")
              if (num_rows$num_long_rows > 0 && i + 5 <= file_length) {
                vars$death_long <- extract_info(file[1][[1]][[i + 5]], "LONG")
              }
            }
          }
        }
      }
      next
    }


    # Attribute tags using process_tag()
    for (tag_field in list(
      c("SEX", "sex"),

      # CAST	caste
      #  g7:CAST	The name of an individual’s rank or status in society which is sometimes based on racial or religious differences, or differences in wealth, inherited rank, profession, or occupation.
      c("CAST", "attribute_caste"),

      # DSCR	physical description
      # g7:DSCR	The physical characteristics of a person.
      c("DSCR", "attribute_description"),

      # EDUC	education
      #  g7:EDUC	Indicator of a level of education attained.
      c("EDUC", "attribute_education"),

      # IDNO	identifying number
      # g7:IDNO	A number or other string assigned to identify a person within some significant external system. It must have a TYPE substructure to define what kind of identification number is being provided.
      c("IDNO", "attribute_idnumber"),

      # NATI	nationality
      # g7:NATI	An individual’s national heritage or origin, or other folk, house, kindred, lineage, or tribal interest.
      c("NATI", "attribute_nationality"),

      # NCHI	number of children
      # g7:INDI-NCHI	The number of children that this person is known to be the parent of (all marriages).
      c("NCHI", "attribute_children"),

      # NMR	number of marriages
      # g7:NMR	The number of times this person has participated in a family as a spouse or parent.
      c("NMR", "attribute_marriages"),

      # OCCU	occupation
      # g7:OCCU	The type of work or profession of an individual.
      c("OCCU", "attribute_occupation"),

      # PROP	property
      # g7:PROP	Pertaining to possessions such as real estate or other property of interest.
      c("PROP", "attribute_property"),

      # RELI	religion
      #  g7:INDI-RELI	A religious denomination to which a person is affiliated or for which a record applies.
      c("RELI", "attribute_religion"),

      # RESI	residence
      # g7:INDI-RESI	An address or place of residence where an individual resided.
      c("RESI", "attribute_residence"),

      # SSN	social security number
      # g7:SSN	A number assigned by the United States Social Security Administration, used for tax identification purposes. It is a type of IDNO.
      c("SSN", "attribute_ssn"),

      # TITL	title
      # g7:INDI-TITL	A formal designation used by an individual in connection with positions of royalty or other social status, such as Grand Duke.
      c("TITL", "attribute_title")
    )) {
      result <- process_tag(tag_field[1], tag_field[2], num_rows, tmpv, vars)
      vars <- result$vars
      if (result$matched) next
    }

    # relationship data
    # g7:INDI-FAMC
    ## The family in which an individual appears as a child. It is also used with a g7:FAMC-STAT substructure to show individuals who are not children of the family. See FAMILY_RECORD for more details.
    result <- process_tag("FAMC", "FAMC", num_rows, tmpv, vars,
      extractor = function(x) stringr::str_extract(x, "(?<=@.)\\d*(?=@)"),
      mode = "append"
    )
    vars <- result$vars
    if (result$matched) next

    # FAMS (Family spouse) g7:FAMS
    #  The family in which an individual appears as a partner. See FAMILY_RECORD for more details.
    result <- process_tag("FAMS", "FAMS", num_rows, tmpv, vars,
      extractor = function(x) stringr::str_extract(x, "(?<=@.)\\d*(?=@)"),
      mode = "append"
    )
    vars <- result$vars
    if (result$matched) next

    if (verbose && i %% 1000 == 0) {
      cat("Processed", i, "lines\n")
    }
  }
  # write final file line
  line_to_write <- as.data.frame(vars)
  df_temp <- rbind(df_temp, line_to_write)
  df_temp <- df_temp[!is.na(df_temp$id), ]

  if (verbose) {
    print(paste0("File has ", nrow(df_temp), " people"))
  }
  if (nrow(df_temp) == 0) {
    warning("No people found in file")
    return(NULL)
  }
  if (nrow(df_temp) != num_rows$num_indi_rows) {
    warning("The number of people found in the processed file does not match the number of individuals raw data")
  }
  # Add mom and dad ids
  if (add_parents) {
    if (verbose) {
      print("Processing parents")
    }
    df_temp <- processParents(df_temp, datasource = "gedcom")
  }



  if (combine_cols) {
    if (verbose) {
      print("Combining Duplicate Columns")
    }
    # need to check if any values aren't NA in name_given_pieces and name_surn_pieces
    # Combine `name_given` and `name_given_pieces`

    # Combine `name_given` and `name_given_pieces`
    if (!all(is.na(df_temp$name_given_pieces)) | !all(is.na(df_temp$name_given))) {
      result <- combine_columns(df_temp$name_given, df_temp$name_given_pieces)
      df_temp$name_given <- result$combined
      if (!result$retain_col2) {
        df_temp$name_given_pieces <- NULL
      }
    }

    # Combine `name_surn` and `name_surn_pieces`
    if (!all(is.na(df_temp$name_surn_pieces)) | !all(is.na(df_temp$name_surn))) {
      result <- combine_columns(df_temp$name_surn, df_temp$name_surn_pieces)
      df_temp$name_surn <- result$combined
      if (!result$retain_col2) {
        df_temp$name_surn_pieces <- NULL
      }
    }
  }

  if (remove_empty_cols) {
    # Remove empty columns
    if (verbose) {
      print("Removing empty columns")
    }
    df_temp <- df_temp[, colSums(is.na(df_temp)) < nrow(df_temp)]
  }
  if (skinny) {
    if (verbose) {
      print("Slimming down the data frame")
    }
    df_temp <- df_temp[, colSums(is.na(df_temp)) < nrow(df_temp)]
    df_temp$FAMC <- NULL
    df_temp$FAMS <- NULL
  }
  return(df_temp)
}


#' Create a mapping of family IDs to parent IDs
#'
#' This function creates a mapping from family IDs to the IDs of the parents.
#'
#' @param df_temp A data frame containing information about individuals.
#' @return A list mapping family IDs to parent IDs.
#' @keywords internal
#'
mapFAMS2parents <- function(df_temp) {
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
mapFAMC2parents <- function(df_temp, family_to_parents) {
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

#' Process parents information
#'
#' This function processes the dataframe to add momID and dadID columns.
#'
#' @param df_temp A data frame containing information about individuals.
#' @return A data frame with added momID and dadID columns.
#' @keywords internal
processParents <- function(df_temp, datasource) {
  # Ensure required columns are present
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

  family_to_parents <- mapFAMS2parents(df_temp)
  if (is.null(family_to_parents) || length(family_to_parents) == 0) {
    return(df_temp)
  }
  df_temp <- mapFAMC2parents(df_temp, family_to_parents)
  return(df_temp)
}



#' Extract Information from Line
#'
#' This function extracts information from a line based on a specified type.
#' @param line A character string representing a line from a GEDCOM file.
#' @param type A character string representing the type of information to extract.
#' @return A character string with the extracted information.
#' @keywords internal
extract_info <- function(line, type) {
  stringr::str_squish(stringr::str_extract(line, paste0("(?<=", type, " ).+")))
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

  # Check if there are any conflicting non-NA values
  conflicts <- !is.na(col1_lower) & !is.na(col2_lower) & col1_lower != col2_lower

  if (any(conflicts)) {
    warning("Columns have conflicting values. They were not merged.")
    return(list(combined = col1, retain_col2 = TRUE)) # Indicate to retain col2
  } else {
    combined <- ifelse(is.na(col1), col2, col1)
    return(list(combined = combined, retain_col2 = FALSE))
  }
}

#' Check for Pattern Rows
#'
#' This function counts the number of rows containing specific patterns.
#' @param file A data frame containing the GEDCOM file.
#' @return A list with the number of rows containing each pattern.
#' @keywords internal
#'
countPatternRows <- function(file) {
  # Count the number of rows containing specific patterns
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
#'
process_tag <- function(tag, field_name, pattern_rows, line, vars,
                        extractor = NULL, mode = "replace") {
  count_name <- paste0("num_", tolower(tag), "_rows")
  matched <- FALSE
  if (!is.null(pattern_rows[[count_name]]) &&
    pattern_rows[[count_name]] > 0 &&
    grepl(paste0(" ", tag), line)) {
    value <- if (is.null(extractor)) extract_info(line, tag) else extractor(line)

    if (mode == "append" && !is.na(vars[[field_name]])) {
      vars[[field_name]] <- paste0(vars[[field_name]], ", ", value)
    } else {
      vars[[field_name]] <- value
    }

    matched <- TRUE
  }
  return(list(vars = vars, matched = matched))
}
