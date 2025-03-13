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
                       skinny = FALSE) {
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
    if (num_rows$num_givn_rows > 0 && grepl(" GIVN", tmpv)) {
      vars$name_given_pieces <- extract_info(tmpv, "GIVN")
      next
    }

    # npfx := Name Prefix
    if (num_rows$num_npfx_rows > 0 && grepl(" NPFX", tmpv)) {
      vars$name_npfx <- extract_info(tmpv, "NPFX")
      next
    }

    # NICK := Nickname
    if (num_rows$num_nick_rows > 0 && grepl(" NICK", tmpv)) {
      vars$name_nick <- extract_info(tmpv, "NICK")
      next
    }

    # surn := Surname
    if (num_rows$num_surn_rows > 0 && grepl(" SURN", tmpv)) {
      vars$name_surn_pieces <- extract_info(tmpv, "SURN")
      next
    }

    # nsfx := Name suffix
    if (num_rows$num_nsfx_rows > 0 && grepl(" NSFX", tmpv)) {
      vars$name_nsfx <- extract_info(tmpv, "NSFX")
      next
    }
    if (num_rows$num_marnm_rows > 0 && grepl(" _MARNM", tmpv)) {
      vars$name_marriedsurn <- extract_info(tmpv, "_MARNM")
      next
    }
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

    if (grepl(" SEX", tmpv)) {
      vars$sex <- extract_info(tmpv, "SEX")
      next
    }

    # Individual Attributes

    # CAST	caste
    #  g7:CAST	The name of an individual’s rank or status in society which is sometimes based on racial or religious differences, or differences in wealth, inherited rank, profession, or occupation.
    if (num_rows$num_cast_rows > 0 && grepl(" CAST", tmpv)) {
      vars$attribute_caste <- extract_info(tmpv, "CAST")
      next
    }
    # DSCR	physical description
    # g7:DSCR	The physical characteristics of a person.
    if (num_rows$num_dscr_rows > 0 && grepl(" DSCR", tmpv)) {
      vars$attribute_description <- extract_info(tmpv, "DSCR")
      next
    }
    # EDUC	education
    #  g7:EDUC	Indicator of a level of education attained.
    if (num_rows$num_educ_rows > 0 && grepl(" EDUC", tmpv)) {
      vars$attribute_education <- extract_info(tmpv, "EDUC")
      next
    }
    # IDNO	identifying number
    # g7:IDNO	A number or other string assigned to identify a person within some significant external system. It must have a TYPE substructure to define what kind of identification number is being provided.
    if (num_rows$num_idno_rows > 0 && grepl(" IDNO", tmpv)) {
      vars$attribute_idnumber <- extract_info(tmpv, "IDNO")
      next
    }
    # NATI	nationality
    # g7:NATI	An individual’s national heritage or origin, or other folk, house, kindred, lineage, or tribal interest.
    if (num_rows$num_nati_rows > 0 && grepl(" NATI", tmpv)) {
      vars$attribute_nationality <- extract_info(tmpv, "NATI")
      next
    }
    # NCHI	number of children
    # g7:INDI-NCHI	The number of children that this person is known to be the parent of (all marriages).
    if (num_rows$num_nchi_rows > 0 && grepl(" NCHI", tmpv)) {
      vars$attribute_children <- extract_info(tmpv, "NCHI")
      next
    }

    # NMR	number of marriages
    # g7:NMR	The number of times this person has participated in a family as a spouse or parent.
    if (num_rows$num_nmr_rows > 0 && grepl(" NMR", tmpv)) {
      vars$attribute_marriages <- extract_info(tmpv, "NMR")
      next
    }

    # OCCU	occupation
    # g7:OCCU	The type of work or profession of an individual.
    if (num_rows$num_occu_rows > 0 && grepl(" OCCU", tmpv)) {
      vars$attribute_occupation <- extract_info(tmpv, "OCCU")
      next
    }
    # PROP	property
    # g7:PROP	Pertaining to possessions such as real estate or other property of interest.

    if (num_rows$num_prop_rows > 0 && grepl(" PROP", tmpv)) {
      vars$attribute_property <- extract_info(tmpv, "PROP")
      next
    }

    # RELI	religion
    #  g7:INDI-RELI	A religious denomination to which a person is affiliated or for which a record applies.
    if (num_rows$num_reli_rows > 0 && grepl(" RELI", tmpv)) {
      vars$attribute_religion <- extract_info(tmpv, "RELI")
      next
    }
    # RESI	residence
    # g7:INDI-RESI	An address or place of residence where an individual resided.
    if (num_rows$num_resi_rows > 0 && grepl(" RESI", tmpv)) {
      vars$attribute_residence <- extract_info(tmpv, "RESI")
      next
    }

    # SSN	social security number
    # g7:SSN	A number assigned by the United States Social Security Administration, used for tax identification purposes. It is a type of IDNO.
    if (num_rows$num_ssn_rows > 0 && grepl(" SSN", tmpv)) {
      vars$attribute_ssn <- extract_info(tmpv, "SSN")
      next
    }
    # TITL	title
    # g7:INDI-TITL	A formal designation used by an individual in connection with positions of royalty or other social status, such as Grand Duke.
    if (num_rows$num_titl_rows > 0 && grepl(" TITL", tmpv)) {
      vars$attribute_title <- extract_info(tmpv, "TITL")
      next
    }

    # relationship data
    # g7:INDI-FAMC
    ## The family in which an individual appears as a child. It is also used with a g7:FAMC-STAT substructure to show individuals who are not children of the family. See FAMILY_RECORD for more details.
    if (num_rows$num_famc_rows > 0 && grepl(" FAMC", tmpv)) {
      if (is.na(vars$FAMC)) {
        vars$FAMC <- stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)")
      } else {
        vars$FAMC <- paste0(vars$FAMC, ", ", stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)"))
      }
      next
    }
    # FAMS (Family spouse) g7:FAMS
    #  The family in which an individual appears as a partner. See FAMILY_RECORD for more details.
    if (num_rows$num_fams_rows > 0 && grepl(" FAMS", tmpv)) {
      if (is.na(vars$FAMS)) {
        vars$FAMS <- stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)")
      } else {
        vars$FAMS <- paste0(vars$FAMS, ", ", stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)"))
      }
      next
    }
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
    df_temp <- processParents(df_temp, datasource="gedcom")
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
createFamilyToParentsMapping <- function(df_temp,datasource) {
  if (datasource == "gedcom") {
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
  } else if (datasource == "wiki") {

    message("The data source is not supported")
      return(df_temp)
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
#' @param datasource A string indicating the data source. Options are "gedcom" and "wiki".
#' @return A data frame with added momID and dad_ID columns.
#' @keywords internal
assignParentIDs <- function(df_temp, family_to_parents,datasource) {
  df_temp$momID <- NA_character_
  df_temp$dadID <- NA_character_
if(datasource == "gedcom"){
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
} else if(datasource=="wiki"){
  message("No parents information available for wiki data")
return(df_temp)
  }
}

#' Process parents information
#'
#' This function processes the dataframe to add momID and dadID columns.
#'
#' @param df_temp A data frame containing information about individuals.
#' @return A data frame with added momID and dadID columns.
#' @keywords internal
processParents <- function(df_temp,datasource) {
  # Ensure required columns are present
if(datasource=="gedcom"){
  required_cols <- c("FAMC", "sex", "FAMS")
} else if(datasource=="wiki"){
  required_cols <- c("id")
} else {
  stop("Invalid datasource")
}

if (!all(required_cols %in% colnames(df_temp))) {
    missing_cols <- setdiff(required_cols, colnames(df_temp))
    warning("Missing necessary columns: ", paste(missing_cols, collapse = ", "))
    return(df_temp)
  }

  family_to_parents <- createFamilyToParentsMapping(df_temp,datasource=datasource)
  if (is.null(family_to_parents) || length(family_to_parents) == 0) {
    return(df_temp)
  }
  df_temp <- assignParentIDs(df_temp, family_to_parents,datasource=datasource)
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

#' Read Wiki Family Tree
#'
#' @param text A character string containing the text of a family tree in wiki format.
#' @export
readWikifamilytree <- function(text) {
  # Extract summary text

  summary_text <- extractSummaryText(text)
  # Extract all lines defining the family tree
  tree_lines <- unlist(stringr::str_extract_all(text, "\\{\\{familytree.*?\\}\\}"))
  tree_lines <- tree_lines[!stringr::str_detect(tree_lines, "start|end")] # Remove start/end markers
  tree_lines <- gsub("\\{\\{familytree(.*?)\\}\\}", "\\1", tree_lines) # Remove wrapping markup

  # Convert tree structure into a coordinate grid (preserves symbols!)
  tree_df <- parseTree(tree_lines)

  # Identify columns that start with "Y"
  cols_to_pivot <- grep("^Y", names(tree_df), value = TRUE)

  # Reshape from wide to long format
  tree_long <- makeLongTree(tree_df, cols_to_pivot)

  # Extract member definitions
  members_df <- matchMembers(text)
  members_df$id <- paste0("P", seq_len(nrow(members_df)))  # Assign unique person IDs

  # Merge names into the tree structure (keeping all symbols!)
  tree_long <- merge(tree_long, members_df, by.x = "Value", by.y = "identifier", all.x = TRUE)

  tree_long$DisplayName <- ifelse(!is.na(tree_long$name), tree_long$name, tree_long$Value) # Use name if available

  # parse relationships and infer them

  relationships_df <- processParents(tree_long, datasource = "wiki")



  # Return structured table of the family tree (symbols included)
  list(
    summary = summary_text,
    members = members_df,
    structure = tree_long,
    relationships = relationships_df
  )
}

#' Make Long Tree
#' @param tree_df A data frame containing the tree structure.
#' @param cols_to_pivot A character vector of column names to pivot.
#' @return A long data frame containing the tree structure.
#' @keywords internal
makeLongTree <- function(tree_df, cols_to_pivot) {
  tree_long <- stats::reshape(tree_df,
    varying = cols_to_pivot,
    v.names = "Value",
    timevar = "Column",
    times = cols_to_pivot,
    idvar = setdiff(names(tree_df), cols_to_pivot),
    direction = "long"
  )

  tree_long <- tree_long[!is.na(tree_long$Value), ]
  tree_long$Value <- stringr::str_trim(tree_long$Value)
  tree_long$Column <- as.numeric(gsub("^Y", "", tree_long$Column))
  return(tree_long)
}

#' Match Members
#' @inheritParams readWikifamilytree
#' @return A data frame containing information about the members of the family tree.
#' @keywords internal

matchMembers <- function(text) {
  member_matches <- stringr::str_extract_all(text, "\\|\\s*([A-Za-z0-9]+)\\s*=\\s*([^|}]*)")[[1]]
  member_matches <- gsub("\\[|\\]|'''", "", member_matches) # Remove formatting

  members_df <- data.frame(
    identifier = stringr::str_trim(stringr::str_extract(member_matches, "^[^=]+")),
    name = stringr::str_trim(stringr::str_extract(member_matches, "(?<=\\=).*")),
    stringsAsFactors = FALSE
  )

  # Remove leading pipes (`|`) from identifiers for consistency
  members_df$identifier <- gsub("^\\|\\s*", "", members_df$identifier)

  # remove summary row
  members_df <- members_df[members_df$identifier != "summary", ]

  return(members_df)
}

#' Extract Summary Text
#' @inheritParams readWikifamilytree
#' @return A character string containing the summary text.
#' @keywords internal
#' @export

extractSummaryText <- function(text) {
  summary_match <- stringr::str_match(text, "\\{\\{familytree/start \\|summary=(.*?)\\}\\}")
  summary_text <- ifelse(!is.na(summary_match[, 2]), summary_match[, 2], NA)
  return(summary_text)
}

#' Parse Tree
#' @param tree_lines A character vector containing the lines of the tree structure.
#' @return A data frame containing the tree structure.
#' @keywords internal
#' @export

parseTree <- function(tree_lines) {
  tree_matrix <- base::strsplit(tree_lines, "\\|") # Split each row into columns
  max_cols <- max(sapply(tree_matrix, length)) # Find the max column count

  # Convert to a data frame (ensures correct structure)
  tree_df <- do.call(rbind, lapply(tree_matrix, function(row) {
    length(row) <- max_cols # Ensure uniform column length
    return(row)
  }))

  tree_df <- as.data.frame(tree_df, stringsAsFactors = FALSE)
  colnames(tree_df) <- paste0("Y", seq_len(ncol(tree_df))) # Assign column names
  tree_df$Row <- seq_len(nrow(tree_df)) # Assign row numbers
  return(tree_df)
}

#' infer relationship from tree template
#'
#' @param tree_long A data frame containing the tree structure in long format.
#' @return A data frame containing the relationships between family members.
#' @keywords internal
#'
parseRelationships <- function(tree_long) {
  relationships <- data.frame(
    id = unique(tree_long$Value),
    momID = NA_character_,
    dadID = NA_character_,
    spouseID = NA_character_,
    stringsAsFactors = FALSE
  )

  # Loop through rows to find connections
  for (i in seq_len(nrow(tree_long))) {
    row <- tree_long[i, ]

    # **Parent-Child Detection**
    if (row$Value == "y") {
      parent <- tree_long$Value[tree_long$Row == row$Row - 1 & tree_long$Column == row$Column]
      child <- tree_long$Value[tree_long$Row == row$Row + 1 & tree_long$Column == row$Column]

      # Assign mom/dad IDs based on tree structure
      if (!is.na(parent) && !is.na(child)) {
        relationships$momID[relationships$id == child] <- parent
        relationships$dadID[relationships$id == child] <- parent  # Assuming one parent detected for now
      }
    }

    # **Spouse Detection**
    if (row$Value == "+") {
      spouse1 <- tree_long$Value[tree_long$Row == row$Row & tree_long$Column == row$Column - 1]
      spouse2 <- tree_long$Value[tree_long$Row == row$Row & tree_long$Column == row$Column + 1]

      if (!is.na(spouse1) && !is.na(spouse2)) {
        relationships$spouseID[relationships$id == spouse1] <- spouse2
        relationships$spouseID[relationships$id == spouse2] <- spouse1
      }
    }
  }

  return(relationships)
}
