#' Read a GEDCOM File
#'
#' This function reads a GEDCOM file and parses it into a structured data frame of individuals.
#' inpired by https://raw.githubusercontent.com/jjfitz/readgedcom/master/R/read_gedcom.R
#' @param file_path The path to the GEDCOM file.
#' @param add_parents A logical value indicating whether to add parents to the data frame.
#' @param remove_empty_cols A logical value indicating whether to remove columns with all missing values.
#' @param verbose A logical value indicating whether to print messages.
#' @return A data frame containing information about individuals, with the following columns:
#' - `id`: ID of the individual
#' - `firstname`: First name of the individual
#' - `lastname`: Last name of the individual
#' - `birthdate`: Birth date of the individual
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
#' - `MARNM`: Married Name
#' - `NPFX`: Name Prefix
#' - `CAUS`: Cause of death
#' - `title`: Title of the individual
#' - `occupation`: Occupation of the individual
#' - `religion`: Religion of the individual
#' - `education`: Education of the individual
#' @export
readGedcom <- function(file_path, verbose = FALSE, add_parents = TRUE, remove_empty_cols = TRUE) {
  if(verbose) {
    print(paste("Reading file:", file_path))
  }
  file <- data.frame(X1 = readLines(file_path))
  if(verbose) {
    print(paste0("File is ", nrow(file), " lines long"))
  }

  # List of variables to initialize
  var_names <- c("id",
                 "firstname",
                 "firstname_alt",
                 "lastname",
                 "lastname_alt",
                 "birthdate",
                 "birthplace",
                 "birthlat",
                 "birthlong",
                 "deathdate",
                 "deathplace",
                 "deathlat",
                 "deathlong",
                 "title",
                 "sex",
                 "FAMC",
                 "FAMS",
                 "marnm", "npfx", "caus",
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
      vars$firstname <- extract_info(tmpv, "NAME")
      vars$lastname <- extract_info(tmpv, "/")
      next
    }
    if(grepl(" GIVN", tmpv)) {
      vars$firstname_alt <- extract_info(tmpv, "GIVN")
      next
    }
    if(grepl(" SURN", tmpv)) {
      vars$lastname_alt <- extract_info(tmpv, "SURN")
      next
    }
    if(grepl(" TITL", tmpv)) {
      vars$title <- extract_info(tmpv, "TITL")
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
      vars$birthdate <- extract_info(file[1][[1]][[i+1]], "DATE")
      vars$birthplace <- extract_info(file[1][[1]][[i+2]], "PLAC")
      vars$birthlat <- extract_info(file[1][[1]][[i+4]], "LATI")
      vars$birthlong <- extract_info(file[1][[1]][[i+5]], "LONG")
      next
    }

    if(grepl(" DEAT", tmpv)) {
      vars$deathdate <- extract_info(file[1][[1]][[i + 1]], "DATE")
      vars$deathplace <- extract_info(file[1][[1]][[i + 2]], "PLAC")
      vars$deathlat <- extract_info(file[1][[1]][[i + 4]], "LATI")
      vars$deathlong <- extract_info(file[1][[1]][[i + 5]], "LONG")
      vars$caus <- extract_info(file[1][[1]][[i + 3]], "CAUS")
      next
    }

    if(grepl(" SEX", tmpv)) {
      vars$sex <- extract_info(tmpv, "SEX")
      next
    }
    if(grepl(" _MARNM", tmpv)) {
      vars$marnm <- extract_info(tmpv, "_MARNM")
      next
    }

    if(grepl(" NPFX", tmpv)) {
      vars$npfx <- extract_info(tmpv, "NPFX")
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

  if(verbose) { print("Removing Duplicate Columns")}

  # need to check if any values aren't NA in firstname_alt and lastname_alt
  if(!all(is.na(df_temp$firstname_alt)) | !all(is.na(df_temp$firstname))) {
  # checking if names match and if not, replacing missing values
  if( mean(stringr::str_to_lower(df_temp$firstname) ==  stringr::str_to_lower(df_temp$firstname_alt)) == 1) {
    df_temp$firstname_alt <- NULL
  } else if(mean(stringr::str_to_lower(df_temp$firstname) ==  stringr::str_to_lower(df_temp$firstname_alt), na.rm = TRUE) == 1) {
    df_temp$firstname[is.na(df_temp$firstname)] <- df_temp$firstname_alt[is.na(df_temp$firstname)]
    df_temp$firstname_alt <- NULL
  }
  }

  if(!all(is.na(df_temp$lastname_alt))|!all(is.na(df_temp$lastname))){
  if( mean(stringr::str_to_lower(df_temp$lastname) ==  stringr::str_to_lower(df_temp$lastname_alt)) == 1) {
    df_temp$lastname_alt <- NULL
  } else if(mean(stringr::str_to_lower(df_temp$lastname) ==  stringr::str_to_lower(df_temp$lastname_alt), na.rm = TRUE)  == 1) {
    df_temp$lastname[is.na(df_temp$lastname)] <- df_temp$lastname_alt[is.na(df_temp$lastname)]
    df_temp$lastname_alt <- NULL
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
