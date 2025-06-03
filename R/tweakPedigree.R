#' makeTwins
#' A function to impute twins in the simulated pedigree \code{data.frame}.
#' Twins can be imputed by specifying their IDs or by specifying the generation the twin should be imputed.
#' This is a supplementary function for \code{simulatePedigree}.
#' @param ped A \code{data.frame} in the same format as the output of \code{simulatePedigree}.
#' @param ID_twin1 A vector of \code{ID} of the first twin.
#' @param ID_twin2 A vector of \code{ID} of the second twin.
#' @param verbose logical.  If TRUE, print progress through stages of algorithm
#' @param gen_twin A vector of \code{generation} of the twin to be imputed.
#' @param zygosity A character string indicating the zygosity of the twins. Default is "MZ" for monozygotic twins.
#' @return Returns a \code{data.frame} with MZ twins information added as a new column.
#' @export

# A function to impute twins in the simulated pedigree \code{data.frame}.
# Twins can be imputed by specifying their IDs or by specifying the generation the twin should be imputed.
makeTwins <- function(ped, ID_twin1 = NA_integer_,
                      ID_twin2 = NA_integer_,
                      gen_twin = 2,
                      verbose = FALSE,
                      zygosity = "MZ") {
  # Check if the ped is the same format as the output of simulatePedigree
  if (paste0(colnames(ped), collapse = "") != paste0(c(
    "famID", "ID", "gen",
    "dadID", "momID", "spID", "sex"
  ), collapse = "")) {
    ped <- standardizeColnames(ped, verbose = verbose)
    if (verbose) {
      cat("The input pedigree is not in the same format as the output of simulatePedigree\n")
    }
    # stop("The input pedigree is not in the same format as the output of simulatePedigree")
  }
  ped$MZtwin <- NA_integer_
  ped$zygosity <- NA_character_
  # Check if the two IDs are provided
  if (is.na(ID_twin1) || is.na(ID_twin2)) {
    # Check if the generation is provided
    if (is.na(gen_twin)) {
      stop("You should provide either the IDs of the twins or the generation of the twins")
    } else {
      # Check if the generation is valid
      if (gen_twin < 2 || gen_twin > max(ped$gen)) {
        stop("The generation of the twins should be an integer between 2 and the maximum generation in the pedigree")
      } else {
        idx <- nrow(ped[ped$gen == gen_twin & !is.na(ped$dadID), ])
        usedID <- c()
        # randomly loop through all the individuals in the generation until find an individual who is the same sex and shares the same dadID and momID with another individual
        for (i in 1:idx) {
          # cat("loop", i, "\n")
          # check if i is equal to the number of individuals in the generation
          usedID <- c(usedID, ID_twin1)
          # message(usedID)
          if (i < idx) {
            # randomly select one individual from the generation
            ID_twin1 <- resample(ped$ID[ped$gen == gen_twin & !(ped$ID %in% usedID) & !is.na(ped$dadID)], 1)
            # cat("twin1", ID_twin1, "\n")
            # find one same sex sibling who has the same dadID and momID as the selected individual
            if (zygosity %in% c("MZ", "SS")) {
              twin2_Pool <- ped$ID[ped$ID != ID_twin1 & ped$gen == gen_twin & ped$sex == ped$sex[ped$ID == ID_twin1] & ped$dadID == ped$dadID[ped$ID == ID_twin1] & ped$momID == ped$momID[ped$ID == ID_twin1]]
            } else if (zygosity == "DZ") {
              twin2_Pool <- ped$ID[ped$ID != ID_twin1 & ped$gen == gen_twin & ped$dadID == ped$dadID[ped$ID == ID_twin1] & ped$momID == ped$momID[ped$ID == ID_twin1]]
            } else if (zygosity == "OS") {
              twin2_Pool <- ped$ID[ped$ID != ID_twin1 & ped$gen == gen_twin & ped$sex != ped$sex[ped$ID == ID_twin1] & ped$dadID == ped$dadID[ped$ID == ID_twin1] & ped$momID == ped$momID[ped$ID == ID_twin1]]
            } else {
              stop("The zygosity should be either 'MZ', 'DZ', or 'OS'")
            }

            # if there is an non-NA value in the twin2_Pool, get rid of the NA value
            if (all(is.na(twin2_Pool))) {
              cat("twin2_Pool is all NA\n")
              next
            } else {
              twin2_Pool <- twin2_Pool[!is.na(twin2_Pool)]
              ID_twin2 <- resample(twin2_Pool, 1)
              break
            }
            # test if the ID_twin2 is missing
            # if(!is.na(ID_twin2)){
            #   break
            # }
          } else {
            # randomly select all males or females in the generation and put them in a vector
            if (zygosity %in% c("MZ", "SS")) {
              selectGender <- ped$ID[ped$gen == gen_twin & ped$sex == resample(c("M", "F"), 1) & !is.na(ped$dadID) & !is.na(ped$momID)]
            } else if (zygosity %in% c("DZ")) {
              selectGender <- ped$ID[ped$gen == gen_twin & !is.na(ped$dadID) & !is.na(ped$momID)]
            } else if (zygosity %in% c("OS")) {
              stop("Opposite sex twins are not supported yet. Please use 'MZ' for monozygotic twins or SS for same-sex twins or 'DZ' for dizygotic twins.")
            } else {
              stop("The zygosity should be either 'MZ' or 'DZ'")
            }

            # message(selectGender)
            if (length(selectGender) < 2) {
              stop("There are no available same-sex people in the generation to make twins")
            }
            # randomly select two individuals from the vector
            ID_DoubleTwin <- resample(selectGender, 2)
            # message(ID_DoubleTwin)
            # change the second person's dadID and momID to the first person's dadID and momID
            ped$dadID[ped$ID == ID_DoubleTwin[2]] <- ped$dadID[ped$ID == ID_DoubleTwin[1]]
            ped$momID[ped$ID == ID_DoubleTwin[2]] <- ped$momID[ped$ID == ID_DoubleTwin[1]]
            # let the two individuals be twins!
            ID_twin1 <- ID_DoubleTwin[1]
            ID_twin2 <- ID_DoubleTwin[2]
            break
          }
        }
        # Impute the IDs of the twin in the MZtwin column
        ped$MZtwin[ped$ID == ID_twin1] <- ID_twin2
        ped$MZtwin[ped$ID == ID_twin2] <- ID_twin1
        # Set the zygosity of the twins
      }
    }
  } else {
    # Impute the IDs of the twin in the MZtwin column
    ped$MZtwin[ped$ID == ID_twin1] <- ID_twin2
    ped$MZtwin[ped$ID == ID_twin2] <- ID_twin1
  }
  if (verbose) {
    cat("twin1", ID_twin1, "\n")
    cat("twin2", ID_twin2, "\n")
  }
  names(ped)[names(ped) == "MZtwin"] <- "twinID"
  ped$zygosity[ped$ID == ID_twin1] <- zygosity
  ped$zygosity[ped$ID == ID_twin2] <- zygosity
  return(ped)
}


#' makeInbreeding
#' A function to create inbred mates in the simulated pedigree \code{data.frame}.
#' Inbred mates can be created by specifying their IDs or the generation the inbred mate should be created.
#' When specifying the generation, inbreeding between siblings or 1st cousin needs to be specified.
#' This is a supplementary function for \code{simulatePedigree}.
#' @param ped A \code{data.frame} in the same format as the output of \code{simulatePedigree}.
#' @param ID_mate1 A vector of \code{ID} of the first mate. If not provided, the function will randomly select two individuals from the second generation.
#' @param ID_mate2 A vector of \code{ID} of the second mate.
#' @param verbose logical.  If TRUE, print progress through stages of algorithm
#' @param gen_inbred A vector of \code{generation} of the twin to be imputed.
#' @param type_inbred A character vector indicating the type of inbreeding. "sib" for sibling inbreeding and "cousin" for cousin inbreeding.
#' @return Returns a \code{data.frame} with some inbred mates.
#' @details
#' This function creates inbred mates in the simulated pedigree \code{data.frame}. This function's purpose is to evaluate the effect of inbreeding on model fitting and parameter estimation. In case it needs to be said, we do not condone inbreeding in real life. But we recognize that it is a common practice in some fields to create inbred strains for research purposes.
#' @export

# A function to create inbred mates in the simulated pedigree.

makeInbreeding <- function(ped,
                           ID_mate1 = NA_integer_,
                           ID_mate2 = NA_integer_,
                           verbose = FALSE,
                           gen_inbred = 2,
                           type_inbred = "sib") {
  # check if the ped is the same format as the output of simulatePedigree

  if (paste0(colnames(ped),
    collapse = ""
  ) != paste0(
    c("famID", "ID", "gen", "dadID", "momID", "spID", "sex"),
    collapse = ""
  )) {
    ped <- standardizeColnames(ped, verbose = verbose)
    if (verbose) {
      cat("The input pedigree is not in the same format as the output of simulatePedigree\n")
    }
  }
  # check if the type of inbreeding is valid
  if (type_inbred %in% c("siblings", "sib", "sibling")) {
    type_inbred <- "sib"
  } else if (type_inbred %in% c("cousins", "cousin")) {
    type_inbred <- "cousin"
    stop("Cousin inbreeding is not supported yet. Please use 'sib' for sibling inbreeding.")
    return()
  } else {
    stop("The type of inbreeding should be either 'sib' or 'cousin'")
    return()
  }
  # check if the two IDs are provided
  if (is.na(ID_mate1) || is.na(ID_mate2)) {
    # Check if the generation is provided
    if (is.na(gen_inbred)) {
      stop("You should provide either the IDs of the inbred mates or the generation of the inbred mates")
    } else {
      # Check if the generation is valid
      if (gen_inbred < 2 || gen_inbred > max(ped$gen)) {
        stop("The generation of the mates should be an integer between 2 and the maximum generation in the pedigree")
      } else {
        if (type_inbred == "sib") {
          # loop through all the nuclear families in the generation
          idx <- nrow(ped[ped$gen == gen_inbred & !is.na(ped$dadID), ])
          usedID <- c()
          ID_mate2 <- NA_integer_
          for (i in 1:idx) {
            # if ID_mate2 is not missing, break the loop
            if (!is.na(ID_mate2)) {
              break
            }
            ID_pool_mate1 <- ped$ID[ped$gen == gen_inbred & !is.na(ped$dadID) & !is.na(ped$momID) & is.na(ped$spID) & !(ped$ID %in% usedID)]
            # if the pool is empty, find all individuals who have the same dadID and momID as the selected individual but mated
            if (length(ID_pool_mate1) == 0) {
              ID_pool_mate1 <- ped$ID[ped$gen == gen_inbred & !is.na(ped$dadID) & !is.na(ped$momID) & !(ped$ID %in% usedID)]
            }
            ID_mate1 <- resample(ID_pool_mate1, 1)
            usedID <- c(usedID, ID_mate1)
            # try to find one opposite-sex individual who has the same dadID and momID as the selected individual, preferalbly not mated
            ID_pool_mate2 <- ped$ID[ped$gen == gen_inbred & ped$sex != ped$sex[ped$ID == ID_mate1] & ped$dadID == ped$dadID[ped$ID == ID_mate1] & ped$momID == ped$momID[ped$ID == ID_mate1] & is.na(ped$spID)]
            # if the pool is not empty, randomly select one individual from the pool
            if (length(ID_pool_mate2) > 0) {
              ID_mate2 <- resample(ID_pool_mate2, 1)
            } else {
              # if the pool is empty, find all individuals who have the same dadID and momID as the selected individual but mated
              ID_pool_mate2 <- ped$ID[ped$gen == gen_inbred & ped$sex != ped$sex[ped$ID == ID_mate1] & ped$dadID == ped$dadID[ped$ID == ID_mate1] & ped$momID == ped$momID[ped$ID == ID_mate1]]
              # if the pool is not empty, randomly select one individual from the pool
              if (length(ID_pool_mate2) > 0) {
                ID_mate2 <- resample(ID_pool_mate2, 1)
              } else {
                next
              }
            }
          }
        } else if (type_inbred == "cousin") {
          stop("cousin inbreeding is not supported yet\n")
        } else {
          stop("The type of inbreeding should be either sib or cousin")
        }
      }
    }
  }
  # save the two individual's former mates' IDs if they have any
  ID_mate1_former_mate <- ped$spID[ped$ID == ID_mate1]
  #  cat(ID_mate1, "\n")
  ID_mate2_former_mate <- ped$spID[ped$ID == ID_mate2]
  #  cat(ID_mate2, "\n")
  # remove two individuals' former mates from the pedigree if they have any
  ped$spID[ped$ID == ID_mate1] <- NA_integer_
  ped$spID[ped$ID == ID_mate2] <- NA_integer_
  # change the spouseID of ID_mate1 and ID_mate2 to each other
  ped$spID[ped$ID == ID_mate1] <- ID_mate2
  ped$spID[ped$ID == ID_mate2] <- ID_mate1
  # change the individuals in next generation whose dadID and momID are ID_mate1 and ID_mate2's former mates to ID_mate1 and ID_mate2
  for (j in seq_len(nrow(ped))) {
    if (!is.na(ped$dadID[j]) & !is.na(ID_mate1_former_mate) & ped$dadID[j] == ID_mate1_former_mate) {
      ped$dadID[j] <- ID_mate2
    }
    if (!is.na(ped$momID[j]) & !is.na(ID_mate1_former_mate) & ped$momID[j] == ID_mate1_former_mate) {
      ped$momID[j] <- ID_mate2
    }
    if (!is.na(ped$dadID[j]) & !is.na(ID_mate2_former_mate) & ped$dadID[j] == ID_mate2_former_mate) {
      ped$dadID[j] <- ID_mate1
    }
    if (!is.na(ped$momID[j]) & !is.na(ID_mate2_former_mate) & ped$momID[j] == ID_mate2_former_mate) {
      ped$momID[j] <- ID_mate1
    }
  }
  return(ped)
}

#' dropLink
#' A function to drop a person from his/her parents in the simulated pedigree \code{data.frame}.
#' The person can be dropped by specifying his/her ID or by specifying the generation which the randomly to-be-dropped person is in.
#' The function can separate one pedigree into two pedigrees. Separating into small pieces should be done by running the function multiple times.
#' This is a supplementary function for \code{simulatePedigree}.
#' @param ped a pedigree simulated from simulatePedigree function or the same format
#' @param ID_drop the ID of the person to be dropped from his/her parents.
#' @param gen_drop the generation in which the randomly dropped person is. Will work if `ID_drop` is not specified.
#' @param sex_drop the biological sex of the randomly dropped person.
#' @param n_drop the number of times the mutation happens.
#' @return a pedigree with the dropped person's `dadID` and `momID` set to NA.
#' @export
dropLink <- function(ped,
                     ID_drop = NA_integer_,
                     gen_drop = 2,
                     sex_drop = NA_character_,
                     n_drop = 1) {
  # check if the ID_drop is specified
  if (is.na(ID_drop)) {
    # check if the sex_drop is specified
    if (is.na(sex_drop)) {
      ID_drop <- resample(ped$ID[ped$gen == gen_drop & !is.na(ped$dadID) & !is.na(ped$momID)], n_drop)
    } else {
      ID_drop <- resample(ped$ID[ped$gen == gen_drop & !is.na(ped$dadID) & !is.na(ped$momID) & ped$sex == sex_drop], n_drop)
    }
    if (!is.na(ID_drop)) {
      ped[ped$ID %in% ID_drop, c("dadID", "momID")] <- NA_integer_
    } else {
      warning("No individual is dropped from their parents.")
    }
  } else {
    ped[ped$ID == ID_drop, c("dadID", "momID")] <- NA_integer_
  }
  return(ped)
}
#' addPersonToTree
#' A function to add a new person to an existing pedigree \code{data.frame}.
#' @param ped A \code{data.frame} representing the existing pedigree.
#' @param name Optional. A character string representing the name of the new person. If not provided, the name will be set to \code{NA}.
#' @param sex A value representing the sex of the new person.
#' @param momID Optional. The ID of the mother of the new person. If not provided, it will be set to \code{NA}.
#' @param dadID Optional. The ID of the father of the new person. If not provided, it will be set to \code{NA}.
#'@param twinID Optional. The ID of the twin of the new person. If not provided, it will be set to \code{NA}.
#'@param personID Optional. The ID of the new person. If not provided, it will be generated as the maximum existing personID + 1.
#'
#' @return A \code{data.frame} with the new person added to the existing pedigree.
#'
#' @export
addPersonToPed <- function(ped, name = NULL, sex = NULL, momID = NA, dadID = NA, twinID = NULL, personID=NULL) {
  stopifnot(is.data.frame(ped))

    # Copy structure from an existing row
    new_row <- ped[1, , drop = FALSE]

    # Blank out all values
    new_row[1, ] <- NA

    # Assign new values
    if (!is.null(personID)) {
      new_row$personID <- personID
    } else {
      # Generate a new personID based on the maximum existing personID
        new_row$personID <- max(ped$personID, na.rm = TRUE) + 1
    }
    if (!is.null(name) && "name" %in% colnames(ped)) {
      new_row$name <- name
    } else if ("name" %in% colnames(ped)) {
      new_row$name <- NA_character_
    }
    if(!is.null(twinID) && "twinID" %in% colnames(ped)) {
      new_row$twinID <- twinID
    } else if ("twinID" %in% colnames(ped)) {
      new_row$twinID <- NA_integer_
    }
    if(!is.null(momID) && "momID" %in% colnames(ped)) {
      new_row$momID <- momID
    } else if ("momID" %in% colnames(ped)) {
      new_row$momID <- NA_integer_
    }
    if(!is.null(dadID) && "dadID" %in% colnames(ped)) {
      new_row$dadID <- dadID
    } else if ("dadID" %in% colnames(ped)) {
      new_row$dadID <- NA_integer_
    }
if(!is.null(sex) && "sex" %in% colnames(ped)) {
  new_row$sex      <- sex
  } else if ("sex" %in% colnames(ped)) {
  new_row$sex <- NA_character_
  }

    # Append to data frame
    rbind(ped, new_row)
}
