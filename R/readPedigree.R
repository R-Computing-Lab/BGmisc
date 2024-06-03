#' Read a GEDCOM File
#'
#' This function reads a GEDCOM file and parses it into a structured data frame of individuals.
#' inpired by https://raw.githubusercontent.com/jjfitz/readgedcom/master/R/read_gedcom.R
#' @param file_path The path to the GEDCOM file.
#' @param add_parents A logical value indicating whether to add parents to the data frame.
#' @param remove_empty_cols A logical value indicating whether to remove columns with all missing values.
#' @param remove_duplicate_cols A logical value indicating whether to remove columns with duplicate values.
#' @param verbose A logical value indicating whether to print messages.
#' @return A data frame containing information about individuals, with the following columns:
#' - `id`: ID of the individual
#' - `name_given`: First name of the individual
#' - `name_surn`: Last name of the individual
#' - `birth_date`: Birth date of the individual
#' - `birthplace`: Birthplace of the individual
#' - `birthlat`: Latitude of the birthplace
#' - `birthlong`: Longitude of the birthplace
#' - `deathdate`: Death date of the individual
#' - `deathplace`: Place of death of the individual
#' - `deathlat`: Latitude of the place of death
#' - `deathlong`: Longitude of the place of death
#' - `sex`: Sex of the individual
#' - `FAMC`: ID(s) of the family where the individual is a child
#' - `FAMS`: ID(s) of the family where the individual is a spouse
#' - `momID`: ID of the individual's mother
#' - `dadID`: ID of the individual's father
#' - `name_marriedsurn`: Married Name
#' - `NPFX`: Name Prefix
#' - `CAUS`: Cause of death
#' - `name_title`: Title of the individual
#' - `occupation`: Occupation of the individual
#' - `religion`: Religion of the individual
#' - `education`: Education of the individual
#' @export
readGedcom <- function(file_path, verbose = FALSE, add_parents = TRUE, remove_empty_cols = TRUE, remove_duplicate_cols) {
  if(verbose) {
    print(paste("Reading file:", file_path))
  }
  file <- data.frame(X1 = readLines(file_path))
  if(verbose) {
    print(paste0("File is ", nrow(file), " lines long"))
  }

  # List of variables to initialize
  var_names <- c("id",
                 "name",
                 "sex",
                 "FAMC",
                 "FAMS",
                 "name_given",
                 "name_given_pieces",
                 "name_surn",
                 "name_surn_pieces",
                 "name_marriedsurn",
                 "name_nick",
                 "name_npfx",
                 "name_nsfx",
                 "name_title",
                 "birth_date",
                 "birth_place",
                 "birth_lat",
                 "birth_long",
                 "death_date",
                 "death_place",
                 "death_lat",
                 "death_long",
                 "death_caus",
                 "occupation", "religion", "education")

  # Initialize all variables to NA
  vars <- stats::setNames(as.list(rep(NA, length(var_names))), var_names)

  df_temp <- as.data.frame(matrix(nrow = 1, ncol = length(var_names)))
  names(df_temp) <- var_names

  if(verbose) { print("Parsing GEDCOM file") }

  # Helper function for extracting information
  extract_info <- function(line, type) {
    stringr::str_squish(stringr::str_extract(line, paste0("(?<=", type, " ).+")))
  }

  for (i in 1:length(file[1][[1]])) {
    tmpv <- file[1][[1]][[i]]

    if(grepl("@ INDI", tmpv)) {
      line_to_write <- as.data.frame(vars)
      df_temp <- rbind(df_temp, line_to_write)

      # Reset all variables to NA
      vars <- stats::setNames(as.list(rep(NA, length(var_names))), var_names)

      vars$id <- stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)")
      next
    }

    if(grepl(" NAME", tmpv)) {
      vars$name <- extract_info(tmpv, "NAME")
      vars$name_given <- stringr::str_extract(vars$name, ".*(?= /)")
      vars$name_surn <- stringr::str_extract(vars$name, "(?<=/).*(?=/)")
   #   vars$name <- stringr::str_extract(tmpv,"(?<= NAME ).*")
      next
    }
    # PERSONAL_NAME_PIECES := NAME | NPFX | GIVN | NICK | SPFX | SURN | NSFX
    if(grepl(" GIVN", tmpv)) {
      vars$name_given_pieces <- extract_info(tmpv, "GIVN")
      next
    }
    # npfx := Name Prefix
    if(grepl(" NPFX", tmpv)) {
      vars$name_npfx <- extract_info(tmpv, "NPFX")
      next
    }
    # NICK := Nickname
    if(grepl(" NICK", tmpv)) {
      vars$name_nick <- extract_info(tmpv, "NICK")
      next
    }
    # surn := Surname
    if(grepl(" SURN", tmpv)) {
      vars$name_surn_pieces <- extract_info(tmpv, "SURN")
      next
    }
    # nsfx := Name suffix
    if(grepl(" NSFX", tmpv)) {
      vars$name_nsfx <- extract_info(tmpv, "NSFX")
      next
    }

    if(grepl(" TITL", tmpv)) {
      vars$name_title <- extract_info(tmpv, "TITL")
      next
    }
    if(grepl(" OCCU", tmpv)) {
      vars$occupation <- extract_info(tmpv, "OCCU")
      next
    }
    if(grepl(" RELI", tmpv)) {
      vars$religion <- extract_info(tmpv, "RELI")
      next
    }
    if(grepl(" EDUC", tmpv)) {
      vars$education <- extract_info(tmpv, "EDUC")
      next
    }
    if(grepl(" BIRT", tmpv)) {
      vars$birth_date <- extract_info(file[1][[1]][[i+1]], "DATE")
      vars$birth_place <- extract_info(file[1][[1]][[i+2]], "PLAC")
      vars$birth_lat <- extract_info(file[1][[1]][[i+4]], "LATI")
      vars$birth_long <- extract_info(file[1][[1]][[i+5]], "LONG")
      next
    }

    if(grepl(" DEAT", tmpv)) {
      vars$death_date <- extract_info(file[1][[1]][[i + 1]], "DATE")
      vars$death_place <- extract_info(file[1][[1]][[i + 2]], "PLAC")
      vars$death_lat <- extract_info(file[1][[1]][[i + 4]], "LATI")
      vars$death_long <- extract_info(file[1][[1]][[i + 5]], "LONG")
      vars$death_caus <- extract_info(file[1][[1]][[i + 3]], "CAUS")
      next
    }

    if(grepl(" SEX", tmpv)) {
      vars$sex <- extract_info(tmpv, "SEX")
      next
    }
    if(grepl(" _MARNM", tmpv)) {
      vars$name_marriedsurn <- extract_info(tmpv, "_MARNM")
      next
    }

    # relationship data

    if(grepl(" FAMC", tmpv)) {
      if(is.na(vars$FAMC)) {
        vars$FAMC <- stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)")
      } else {
        vars$FAMC <- paste0(vars$FAMC, ", ", stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)"))
      }
      next
    }

    if(grepl(" FAMS", tmpv)) {
      if(is.na(vars$FAMS)) {
        vars$FAMS <- stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)")
      } else {
        vars$FAMS <- paste0(vars$FAMS, ", ", stringr::str_extract(tmpv, "(?<=@.)\\d*(?=@)"))
      }
      next
    }
  }

  df_temp <- df_temp[!is.na(df_temp$id), ]

  if(verbose) { print(paste0("File has ", nrow(df_temp), " people")) }

  # Add mom and dad ids
  if(add_parents) {
    if(verbose) { print("Processing parents") }
    df_temp <- processParents(df_temp)
  }
if(remove_duplicate_cols){
  if(verbose) { print("Removing Duplicate Columns")}
  df_temp$name <- NULL
  df_temp$FAMC <- NULL
  df_temp$FAMS <- NULL

  # need to check if any values aren't NA in name_given_pieces and name_surn_pieces
  if(!all(is.na(df_temp$name_given_pieces)) | !all(is.na(df_temp$name_given))) {
  # checking if names match and if not, replacing missing values
    given_fn <- try_na(mean(stringr::str_to_lower(df_temp$name_given) ==  stringr::str_to_lower(df_temp$name_given_pieces)))
  if(given_fn == 1&!is.na( given_fn )) {
    df_temp$name_given_pieces <- NULL
  } else if(mean(stringr::str_to_lower(df_temp$name_given) ==  stringr::str_to_lower(df_temp$name_given_pieces), na.rm = TRUE) == 1) {
    df_temp$name_given[is.na(df_temp$name_given)] <- df_temp$name_given_pieces[is.na(df_temp$name_given)]
    df_temp$name_given_pieces <- NULL
  }
  }

  if(!all(is.na(df_temp$name_surn_pieces))|!all(is.na(df_temp$name_surn))){
    surname_fn <- try_na(mean(stringr::str_to_lower(df_temp$name_surn) ==  stringr::str_to_lower(df_temp$name_surn_pieces)))
    if(surname_fn == 1&!is.na(surname_fn)) {
      df_temp$name_surn_pieces <- NULL
    } else if(mean(stringr::str_to_lower(df_temp$name_surn) ==  stringr::str_to_lower(df_temp$name_surn_pieces), na.rm = TRUE)  == 1) {
    df_temp$name_surn[is.na(df_temp$name_surn)] <- df_temp$name_surn_pieces[is.na(df_temp$name_surn)]
    df_temp$name_surn_pieces <- NULL
  }
  }
}
  if(remove_empty_cols) {
    # Remove empty columns
    if(verbose) { print("Removing empty columns") }
    df_temp <- df_temp[, colSums(is.na(df_temp)) < nrow(df_temp)]
  }

  return(df_temp)
}

#' Process parents information
#'
#' This function processes the dataframe to add momID and dadID columns.
#'
#' @param df_temp A data frame containing information about individuals.
#' @return A data frame with added momID and dadID columns.
#' @export
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
