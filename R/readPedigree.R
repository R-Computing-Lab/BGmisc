#' Read a GEDCOM File
#'
#' This function reads a GEDCOM file and parses it into a structured data frame of individuals.
#' adapted from: https://raw.githubusercontent.com/jjfitz/readgedcom/master/R/read_gedcom.R
#' @param file_path The path to the GEDCOM file.
#' @param add_parents A logical value indicating whether to add parents to the data frame.
#' @param remove_empty_cols A logical value indicating whether to remove columns with all missing values.
#' @param verbose A logical value indicating whether to print messages.
#' @return A data frame containing information about individuals, with the following columns:
#' \itemize{
#'   \item{id}{ID of the individual.}
#'   \item{firstname}{First name of the individual.}
#'   \item{lastname}{Last name of the individual.}
#'   \item{birthdate}{Birth date of the individual.}
#'   \item{birthplace}{Birthplace of the individual.}
#'   \item{birthlat}{Latitude of the birthplace.}
#'   \item{birthlong}{Longitude of the birthplace.}
#'   \item{deathdate}{Death date of the individual.}
#'   \item{deathplace}{Place of death of the individual.}
#'   \item{deathlat}{Latitude of the place of death.}
#'   \item{deathlong}{Longitude of the place of death.}
#'   \item{sex}{Sex of the individual.}
#'   \item{FAMC}{ID(s) of the family where the individual is a child.}
#'   \item{FAMS}{ID(s) of the family where the individual is a spouse.}
#'   \item{momID}{ID of the individual's mother.}
#'   \item{dadID}{ID of the individual's father.}
#'   \item{MARNM}{Married Name.}
#'   \item{NPFX}{Name Prefix.}
#'   \item{CAUS}{Cause of death.}
#' }



readGedcom <- function(file_path, verbose = FALSE, add_parents = TRUE, remove_empty_cols = TRUE) {
  #file_path <- "E:/Dropbox/Lab/Research/Projects/2024/BGMiscJoss/BGmisc_main/data-raw/ASOIAF.ged"
  if(verbose) {
    print(paste("Reading file:", file_path))
  }
  file <- data.frame(X1 = readLines(file_path))
  if(verbose) {
    print(paste0("File is ",nrow(file)," lines long"))
  }
  # Initialize variables to store data
  birthdate <- birthplace <- birthlat <- birthlong <- NA
  firstname <- lastname <- sex <- famc <- fams <- id <- NA
  deathdate <- deathplace <- deathlat <- deathlong <- NA
  firstname_alt <- lastname_alt <- marnm <- npfx <- caus <- NA

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
                 "sex",
                 "FAMC",
                 "FAMS",
                 "marnm", "npfx", "caus")
  df_temp <- as.data.frame(matrix(nrow=1,
                                 ncol=length(var_names)))
  names(df_temp) <- var_names

if(verbose) { print("Parsing GEDCOM file") }
  for (i in 1:length(file[1][[1]])) {
    tmpv <- file[1][[1]][[i]]

    if(grepl("@ INDI",tmpv)) {
      line_to_write <- data.frame(id, firstname, firstname_alt, lastname, lastname_alt,
                                  birthdate, birthplace,
                                  birthlat, birthlong,
                                  deathdate, deathplace,
                                  deathlat, deathlong, sex, famc, fams,
                                  marnm, npfx, caus)
      names(line_to_write) <- var_names
      df_temp <- rbind(df_temp, line_to_write)
      birthdate <-  NA
      birthplace <-  NA
      birthlat <-  NA
      birthlong <-  NA
      deathdate <-  NA
      deathplace <-  NA
      deathlat <-  NA
      deathlong <-  NA
      firstname <- NA
      firstname_alt <- NA
      lastname <- NA
      lastname_alt <- NA
      sex <- NA
      famc <- NA
      fams <- NA
      marnm <- NA
      npfx <- NA
      caus <- NA

      id <- stringr::str_extract(tmpv,"(?<=@.)\\d*(?=@)")
      next
    }

    if(grepl(" NAME", tmpv)) {
      firstname <- stringr::str_squish(stringr::str_extract(tmpv,"(?<=NAME ).+(?= ?/+.)"))
      lastname <- stringr::str_squish(stringr::str_extract(tmpv,"(?<=/).*(?=/)"))
      next
    }
    if(grepl(" GIVN", tmpv)) {
      firstname_alt <- stringr::str_squish(stringr::str_extract(tmpv,"(?<=GIVN ).+"))
      next
    }
    if(grepl(" SURN", tmpv)) {
      lastname_alt <- stringr::str_squish(stringr::str_extract(tmpv,"(?<=SURN ).+"))
      next
    }

    if(grepl(" BIRT", tmpv)) {
      birthdate <- stringr::str_extract(file[1][[1]][[i+1]],"(?<=DATE ).+")
      birthplace <- stringr::str_extract(file[1][[1]][[i+2]],"(?<=PLAC ).+")
      birthlat <- stringr::str_extract(file[1][[1]][[i+4]],"(?<=LATI ).+")
      birthlong <- stringr::str_extract(file[1][[1]][[i+5]],"(?<=LONG ).+")
      next
    }

    if(grepl(" DEAT", tmpv)) {
      deathdate <- stringr::str_extract(file[1][[1]][[i+1]],"(?<=DATE ).+")
      deathplace <- stringr::str_extract(file[1][[1]][[i+2]],"(?<=PLAC ).+")
      deathlat <- stringr::str_extract(file[1][[1]][[i+4]],"(?<=LATI ).+")
      deathlong <- stringr::str_extract(file[1][[1]][[i+5]],"(?<=LONG ).+")
      caus <- stringr::str_extract(file[1][[1]][[i+3]],"(?<=CAUS ).+")
      next
    }

    if(grepl(" SEX", tmpv)) {
      sex <- stringr::str_extract(tmpv,"(?<=SEX ).+")
      next
    }
    if(grepl(" _MARNM", tmpv)) {
      marnm <- stringr::str_extract(tmpv,"(?<=_MARNM ).+")
      next
    }

    if(grepl(" NPFX", tmpv)) {
      npfx <- stringr::str_extract(tmpv,"(?<=NPFX ).+")
      next
    }
    if(grepl(" FAMC", tmpv)) {
      if(is.na(famc)) {
        famc <- stringr::str_extract(tmpv,"(?<=@.)\\d*(?=@)")
        next
      } else {
        famc <- paste0(famc, ", ",stringr::str_extract(tmpv,"(?<=@.)\\d*(?=@)"))
        next
      }
    }

    if(grepl(" FAMS", tmpv)) {
      if(is.na(fams)) {
        fams <- stringr::str_extract(tmpv,"(?<=@.)\\d*(?=@)")
        next
      } else {
        fams <- paste0(fams, ", ",stringr::str_extract(tmpv,"(?<=@.)\\d*(?=@)"))
        next
      }
    }
  }

  df_temp <- df_temp[!is.na(df_temp$id), ]

  if(verbose) { print(paste0("File has ",nrow(df_temp)," people")) }
  # Add mom and dad ids
  if(add_parents) {
    if(verbose) { print("Processing parents") }
    df_temp <- processParents(df_temp)
  }

  if(verbose) { print("Removing Duplicate Columns")}

  # checking if names match and if not, replacing missing values
 if(mean(stringr::str_to_lower(df_temp$firstname) ==  stringr::str_to_lower(df_temp$firstname_alt) == 1)) {
    df_temp$firstname_alt <- NULL
  } else if(mean(stringr::str_to_lower(df_temp$firstname) ==  stringr::str_to_lower(df_temp$firstname_alt) == 1, na.rm = TRUE)){

    df_temp$firstname[is.na(df_temp$firstname)] <- df_temp$firstname_alt[is.na(df_temp$firstname)]
    df_temp$firstname_alt <- NULL

  }
 if(mean(stringr::str_to_lower(df_temp$lastname) ==  stringr::str_to_lower(df_temp$lastname_alt) == 1)) {
    df_temp$lastname_alt <- NULL
  } else if(mean(stringr::str_to_lower(df_temp$lastname) ==  stringr::str_to_lower(df_temp$lastname_alt) == 1, na.rm = TRUE)){
    df_temp$lastname[is.na(df_temp$lastname)] <- df_temp$lastname_alt[is.na(df_temp$lastname)]
    df_temp$lastname_alt <- NULL
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
#'
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
