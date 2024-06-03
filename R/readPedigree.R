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
readGedcom <- function(file_path, verbose = FALSE, add_parents = TRUE, remove_empty_cols = TRUE,
                       combine_cols=TRUE,skinny=FALSE) {
  if(verbose) {
    print(paste("Reading file:", file_path))
  }
  file <- data.frame(X1 = readLines(file_path))
  file_length <- nrow(file)
  if(verbose) {
    print(paste0("File is ", file_length, " lines long"))
  }
  # Count the number of rows containing specific patterns
  pattern_counts <- sapply(c("@ INDI", " NAME", " GIVN", " NPFX", " NICK", " SURN", " NSFX", " _MARNM",
                             " BIRT", " DEAT", " SEX", " CAST", " DSCR", " EDUC", " IDNO", " NATI",
                             " NCHI", " NMR", " OCCU", " PROP", " RELI", " RESI", " SSN", " TITL",
                             " FAMC", " FAMS", " PLAC", " LATI", " LONG", " DATE", " CAUS"),
                           function(pat) sum(grepl(pat, file$X1)))

  num_indi_rows <- pattern_counts["@ INDI"]
  num_name_rows <- pattern_counts[" NAME"]
  num_givn_rows <- pattern_counts[" GIVN"]
  num_npfx_rows <- pattern_counts[" NPFX"]
  num_nick_rows <- pattern_counts[" NICK"]
  num_surn_rows <- pattern_counts[" SURN"]
  num_nsfx_rows <- pattern_counts[" NSFX"]
  num_marnm_rows <- pattern_counts[" _MARNM"]
  num_birt_rows <- pattern_counts[" BIRT"]
  num_deat_rows <- pattern_counts[" DEAT"]
  num_sex_rows <- pattern_counts[" SEX"]
  num_cast_rows <- pattern_counts[" CAST"]
  num_dscr_rows <- pattern_counts[" DSCR"]
  num_educ_rows <- pattern_counts[" EDUC"]
  num_idno_rows <- pattern_counts[" IDNO"]
  num_nati_rows <- pattern_counts[" NATI"]
  num_nchi_rows <- pattern_counts[" NCHI"]
  num_nmr_rows <- pattern_counts[" NMR"]
  num_occu_rows <- pattern_counts[" OCCU"]
  num_prop_rows <- pattern_counts[" PROP"]
  num_reli_rows <- pattern_counts[" RELI"]
  num_resi_rows <- pattern_counts[" RESI"]
  num_ssn_rows <- pattern_counts[" SSN"]
  num_titl_rows <- pattern_counts[" TITL"]
  num_famc_rows <- pattern_counts[" FAMC"]
  num_fams_rows <- pattern_counts[" FAMS"]
  num_plac_rows <- pattern_counts[" PLAC"]
  num_lati_rows <- pattern_counts[" LATI"]
  num_long_rows <- pattern_counts[" LONG"]
  num_date_rows <- pattern_counts[" DATE"]
  num_caus_rows <- pattern_counts[" CAUS"]

  # List of variables to initialize
  var_names <- c("id",
                 "momID",
                 "dadID",
                 "sex",
                 "name",
                 "name_given",
                 "name_given_pieces",
                 "name_surn",
                 "name_surn_pieces",
                 "name_marriedsurn",
                 "name_nick",
                 "name_npfx",
                 "name_nsfx",
                 "birth_date",
                 "birth_lat",
                 "birth_long",
                 "birth_place",
                 "death_caus",
                 "death_date",
                 "death_lat",
                 "death_long",
                 "death_place",
                 "attribute_caste",
                 "attribute_children",
                 "attribute_description",
                 "attribute_education",
                 "attribute_idnumber",
                 "attribute_marriages",
                 "attribute_nationality",
                 "attribute_occupation",
                 "attribute_property",
                 "attribute_religion",
                 "attribute_residence",
                 "attribute_ssn",
                 "attribute_title",
                 "FAMC",
                 "FAMS")

  # Initialize all variables to NA
  vars <- stats::setNames(as.list(rep(NA_character_, length(var_names))), var_names)

  df_temp <- as.data.frame(matrix(nrow = 1, ncol = length(var_names)))
  names(df_temp) <- var_names

  if(verbose) { print("Parsing GEDCOM file") }
  for (i in 1:length(file[1][[1]])) {
    tmpv <- file[1][[1]][[i]]

    if(grepl("@ INDI", tmpv)) {
      line_to_write <- as.data.frame(vars)
      df_temp <- rbind(df_temp, line_to_write)

      # Reset all variables to NA
      vars <- stats::setNames(as.list(rep(NA_character_, length(var_names))), var_names)

      vars$id <- stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)")
      next
    }

# names
    if(num_name_rows>0 && grepl(" NAME", tmpv)) {
      vars$name <- extract_info(tmpv, "NAME")
      vars$name_given <- stringr::str_extract(vars$name, ".*(?= /)")
      vars$name_surn <- stringr::str_extract(vars$name, "(?<=/).*(?=/)")
      vars$name <- stringr::str_squish(stringr::str_replace(vars$name, "/", " "))
      next
    }
    # PERSONAL_NAME_PIECES := NAME | NPFX | GIVN | NICK | SPFX | SURN | NSFX
    if(num_givn_rows > 0 && grepl(" GIVN", tmpv)) {
      vars$name_given_pieces <- extract_info(tmpv, "GIVN")
      next
    }

    # npfx := Name Prefix
    if(num_npfx_rows > 0 && grepl(" NPFX", tmpv)) {
      vars$name_npfx <- extract_info(tmpv, "NPFX")
      next
    }

    # NICK := Nickname
    if(num_nick_rows > 0 && grepl(" NICK", tmpv)) {
      vars$name_nick <- extract_info(tmpv, "NICK")
      next
    }

    # surn := Surname
    if(num_surn_rows > 0 && grepl(" SURN", tmpv)) {
      vars$name_surn_pieces <- extract_info(tmpv, "SURN")
      next
    }
    }
    # nsfx := Name suffix
    if(num_nsfx_rows>0 &&  grepl(" NSFX", tmpv)) {
      vars$name_nsfx <- extract_info(tmpv, "NSFX")
      next
    }
    if(num_marnm_rows>0 &&  grepl(" _MARNM", tmpv)) {
      vars$name_marriedsurn <- extract_info(tmpv, "_MARNM")
      next
    }
    # Birth event related information
    if(num_birt_rows > 0 && grepl(" BIRT", tmpv)) {
      if(num_date_rows > 0 && i+1<=file_length) {
        vars$birth_date <- extract_info(file[1][[1]][[i+1]], "DATE")
      if(num_plac_rows > 0 && i+2<=file_length) {
        vars$birth_place <- extract_info(file[1][[1]][[i+2]], "PLAC")
      if(num_lati_rows > 0 && i+4<=file_length) {
        vars$birth_lat <- extract_info(file[1][[1]][[i+4]], "LATI")
      if(num_long_rows > 0 && i+5<=file_length) {
        vars$birth_long <- extract_info(file[1][[1]][[i+5]], "LONG")
        }}}}
      next
    }

    # Death event related information
  # the ifs are nested so that there is no need to check if you've already run out of
    if(num_deat_rows > 0 && grepl(" DEAT", tmpv)) {
      if(num_date_rows > 0 && i+1<=file_length) {
        vars$death_date <- extract_info(file[1][[1]][[i+1]], "DATE")
      if(num_plac_rows > 0&& i+2<=file_length) {
      vars$death_place <- extract_info(file[1][[1]][[i + 2]], "PLAC")
      if(num_caus_rows > 0&&i+3<=file_length) {
        vars$death_caus <- extract_info(file[1][[1]][[i + 3]], "CAUS")
      if(num_lati_rows > 0 && i+4<=file_length) {
      vars$death_lat <- extract_info(file[1][[1]][[i + 4]], "LATI")
      if(num_long_rows > 0 && i+5<=file_length) {
      vars$death_long <- extract_info(file[1][[1]][[i + 5]], "LONG")
      }}}}}
      next
    }

    if(grepl(" SEX", tmpv)) {
      vars$sex <- extract_info(tmpv, "SEX")
      next
    }

    # Individual Attributes

   # CAST	caste
  #  g7:CAST	The name of an individual’s rank or status in society which is sometimes based on racial or religious differences, or differences in wealth, inherited rank, profession, or occupation.
    if(num_cast_rows > 0 && grepl(" CAST", tmpv)) {
      vars$attribute_caste <- extract_info(tmpv, "CAST")
      next
    }
    # DSCR	physical description
   # g7:DSCR	The physical characteristics of a person.
    if( num_dscr_rows > 0 && grepl(" DSCR", tmpv)) {
      vars$attribute_description <- extract_info(tmpv, "DSCR")
      next
    }
    # EDUC	education
  #  g7:EDUC	Indicator of a level of education attained.
    if( num_educ_rows > 0 && grepl(" EDUC", tmpv)) {
      vars$attribute_education <- extract_info(tmpv, "EDUC")
      next
    }
   # IDNO	identifying number
   # g7:IDNO	A number or other string assigned to identify a person within some significant external system. It must have a TYPE substructure to define what kind of identification number is being provided.
    if(num_idno_rows > 0 && grepl(" IDNO", tmpv)) {
      vars$attribute_idnumber <- extract_info(tmpv, "IDNO")
      next
    }
    # NATI	nationality
    # g7:NATI	An individual’s national heritage or origin, or other folk, house, kindred, lineage, or tribal interest.
    if(num_nati_rows > 0 && grepl(" NATI", tmpv)) {
      vars$attribute_nationality <- extract_info(tmpv, "NATI")
      next
    }
    # NCHI	number of children
    # g7:INDI-NCHI	The number of children that this person is known to be the parent of (all marriages).
    if(num_nchi_rows > 0 && grepl(" NCHI", tmpv)) {
      vars$attribute_children <- extract_info(tmpv, "NCHI")
      next
    }

    # NMR	number of marriages
   # g7:NMR	The number of times this person has participated in a family as a spouse or parent.
    if(num_nmr_rows > 0 && grepl(" NMR", tmpv)) {
      vars$attribute_marriages <- extract_info(tmpv, "NMR")
      next
    }

    # OCCU	occupation
   # g7:OCCU	The type of work or profession of an individual.
    if(num_occu_rows > 0 && grepl(" OCCU", tmpv)) {
      vars$attribute_occupation <- extract_info(tmpv, "OCCU")
      next
    }
  # PROP	property
  # g7:PROP	Pertaining to possessions such as real estate or other property of interest.

   if(num_prop_rows > 0 && grepl(" PROP", tmpv)) {
      vars$attribute_property <- extract_info(tmpv, "PROP")
      next
    }

   # RELI	religion
   #  g7:INDI-RELI	A religious denomination to which a person is affiliated or for which a record applies.
    if(num_reli_rows > 0 && grepl(" RELI", tmpv)) {
      vars$attribute_religion <- extract_info(tmpv, "RELI")
      next
    }
    #RESI	residence
    #g7:INDI-RESI	An address or place of residence where an individual resided.
  if(num_resi_rows > 0 && grepl(" RESI", tmpv)) {
      vars$attribute_residence <- extract_info(tmpv, "RESI")
      next
    }

   # SSN	social security number
   # g7:SSN	A number assigned by the United States Social Security Administration, used for tax identification purposes. It is a type of IDNO.
  if(num_ssn_rows > 0 && grepl(" SSN", tmpv)) {
      vars$attribute_ssn <- extract_info(tmpv, "SSN")
      next
    }
    # TITL	title
    # g7:INDI-TITL	A formal designation used by an individual in connection with positions of royalty or other social status, such as Grand Duke.
    if(num_titl_rows > 0 && grepl(" TITL", tmpv)) {
      vars$attribute_title <- extract_info(tmpv, "TITL")
      next
    }

    # relationship data

    # relationship data
    # g7:INDI-FAMC
    ## The family in which an individual appears as a child. It is also used with a g7:FAMC-STAT substructure to show individuals who are not children of the family. See FAMILY_RECORD for more details.
    if(num_famc_rows > 0 && grepl(" FAMC", tmpv)) {
      if(is.na(vars$FAMC)) {
        vars$FAMC <- stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)")
      } else {
        vars$FAMC <- paste0(vars$FAMC, ", ", stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)"))
      }
      next
    }
   # FAMS (Family spouse) g7:FAMS
  #  The family in which an individual appears as a partner. See FAMILY_RECORD for more details.
    if(num_fams_rows > 0 && grepl(" FAMS", tmpv)) {
      if(is.na(vars$FAMS)) {
        vars$FAMS <- stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)")
      } else {
        vars$FAMS <- paste0(vars$FAMS, ", ", stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)"))
      }
      next
    }
# write file line
  df_temp <- rbind(df_temp, line_to_write)
  df_temp <- df_temp[!is.na(df_temp$id), ]

  if(verbose) { print(paste0("File has ", nrow(df_temp), " people")) }
if(nrow(df_temp) == 0) {
    warning("No people found in file")
    return(NULL)
}
  if(nrow(df_temp) != num_indi_rows) {
    warning("The number of people found in the processed file does not match the number of individuals raw data")
  }
  # Add mom and dad ids
  if(add_parents) {
    if(verbose) { print("Processing parents") }
    df_temp <- processParents(df_temp)
  }



if(combine_cols){
  if(verbose) { print("Combining Duplicate Columns")}
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

  if(remove_empty_cols) {
    # Remove empty columns
    if(verbose) { print("Removing empty columns") }
    df_temp <- df_temp[, colSums(is.na(df_temp)) < nrow(df_temp)]
  }
  if(skinny){
    if(verbose) { print("Slimming down the data frame")}
    df_temp <- df_temp[, colSums(is.na(df_temp)) < nrow(df_temp)]
    df_temp$FAMC <- NULL
    df_temp$FAMS <- NULL
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
processParents <- function(df_temp) {
  # Adding momID and dadID columns
  df_temp$momID <- NA_character_
  df_temp$dadID <- NA_character_

  # Identify parents and their respective family IDs
  # Create a mapping of family IDs to parent IDs
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

  # Assign momID and dadID based on family mapping
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
      return(list(combined = col1, retain_col2 = TRUE))  # Indicate to retain col2
    } else {
      combined <- ifelse(is.na(col1), col2, col1)
      return(list(combined = combined, retain_col2 = FALSE))
    }
  }
