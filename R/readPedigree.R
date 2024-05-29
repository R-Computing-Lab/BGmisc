#' Read a GEDCOM File
#'
#' This function reads a GEDCOM file and parses it into a structured data frame of individuals.
#' adapted from: https://raw.githubusercontent.com/jjfitz/readgedcom/master/R/read_gedcom.R
#' @param file_path The path to the GEDCOM file.
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
#' }



readGedcom <- function(file_path) {
  #file_path <- "E:/Dropbox/Lab/Research/Projects/2024/BGMiscJoss/BGmisc_main/data-raw/ASOIAF.ged"

  file <- data.frame(X1 = readLines(file_path))
  start_recording <- FALSE
  is_first <- 0
  # Initialize variables to store data
  birthdate <- birthplace <- birthlat <- birthlong <- NA
  firstname <- lastname <- sex <- famc <- fams <- id <- NA
  deathdate <- deathplace <- deathlat <- deathlong <- NA

  var_names <- c("id",
                 "firstname",
                 "lastname",
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
                 "FAMS")
  df_temp <- as.data.frame(matrix(nrow=1,
                                 ncol=length(var_names)))
  names(df_temp) <- var_names

  for (i in 1:length(file[1][[1]])) {
    tmpv <- file[1][[1]][[i]]

    if(grepl("@ INDI",tmpv)) {
      line_to_write <- data.frame(id, firstname, lastname, birthdate, birthplace,
                                  birthlat, birthlong, deathdate, deathplace,
                                  deathlat, deathlong, sex, famc, fams)
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
      lastname <- NA
      sex <- NA
      famc <- NA
      fams <- NA

      id <- stringr::str_extract(tmpv,"(?<=@.)\\d*(?=@)")
      next
    }

    if(grepl(" NAME", tmpv)) {
      firstname <- stringr::str_extract(tmpv,"(?<=NAME ).+(?= /+.)")
      lastname <- stringr::str_extract(tmpv,"(?<=/).+(?=/)")
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
      next
    }

    if(grepl(" SEX", tmpv)) {
      sex <- stringr::str_extract(tmpv,"(?<=SEX ).+")
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

# recover mom and dad ids
  for (i in 1:nrow(df_temp)) {
    if (!is.na(df_temp$FAMC[i])) {
      famc_ids <- unlist(strsplit(df_temp$FAMC[i], ", "))
      for (famc_id in famc_ids) {
        family_record <- df_temp[df_temp$id == famc_id, ]
        if (nrow(family_record) > 0) {
          if (family_record$sex == "M") {
            df_temp$father_id[i] <- famc_id
          } else if (family_record$sex == "F") {
            df_temp$mother_id[i] <- famc_id
          }
        }
      }
    }
  }

  return(df_temp)
}

