#' makeTwins
#' A function to impute twins in the simulated pedigree \code{data.frame}.
#' Twins can be imputed by specifying their IDs or by specifying the generation the twin should be imputed.
#' This is a supplementary function for \code{simulatePedigree}.
#' @param ped A \code{data.frame} in the same format as the output of \code{simulatePedigree}.
#' @param ID_twin1 A vector of \code{ID} of the first twin.
#' @param ID_twin2 A vector of \code{ID} of the second twin.
#' @param verbose logical.  If TRUE, print progress through stages of algorithm
#' @param gen_twin A vector of \code{generation} of the twin to be imputed.
#' @return Returns a \code{data.frame} with MZ twins infomration added as a new column.
#' @export

# A function to impute twins in the simulated pedigree \code{data.frame}.
# Twins can be imputed by specifying their IDs or by specifying the generation the twin should be imputed.
makeTwins <- function(ped, ID_twin1 = NA_integer_, ID_twin2 = NA_integer_, gen_twin = 2, verbose = FALSE) {
  # a support function
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  # Check if the ped is the same format as the output of simulatePedigree
  if (paste0(colnames(ped), collapse = "") != paste0(c(
    "fam", "ID", "gen",
    "dadID", "momID", "spt", "sex"
  ), collapse = "")) {
    ped <- standardize_colnames(ped)
    if (verbose) {
      cat("The input pedigree is not in the same format as the output of simulatePedigree\n")
    }
    # stop("The input pedigree is not in the same format as the output of simulatePedigree")
  }
  ped$MZtwin <- NA_integer_
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
        # randomly loop through all the indivuduals in the generation until find an individual who is the same sex and shares the same dadID and momID with another individual
        for (i in 1:idx) {
          cat("loop", i)
          # check if i is equal to the number of individuals in the generation
          usedID <- c(usedID, ID_twin1)
          # print(usedID)
          if (i < idx) {
            # randomly select one individual from the generation
            ID_twin1 <- resample(ped$ID[ped$gen == gen_twin & !(ped$ID %in% usedID) & !is.na(ped$dadID)], 1)
            # cat("twin1", ID_twin1, "\n")
            # find one same sex sibling who has the same dadID and momID as the selected individual
            twin2_Pool <- ped$ID[ped$ID != ID_twin1 & ped$gen == gen_twin & ped$sex == ped$sex[ped$ID == ID_twin1] & ped$dadID == ped$dadID[ped$ID == ID_twin1] & ped$momID == ped$momID[ped$ID == ID_twin1]]
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
            selectGender <- ped$ID[ped$gen == gen_twin & ped$sex == resample(c("M", "F"), 1) & !is.na(ped$dadID) & !is.na(ped$momID)]
            # print(selectGender)
            # randomly select two individuals from the vector
            ID_DoubleTwin <- sample(selectGender, 2)
            # print(ID_DoubleTwin)
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
  return(ped)
}


#' makeInbreeding
#' A function to create inbred mates in the simulated pedigree \code{data.frame}.
#' Inbred mates can be created by specifying their IDs or by specifying the generation the inbred mate should be created.
#' When specifying the generation, inbreeding between siblings or 1st cousin needs to be specified.
#' This is a supplementary function for \code{simulatePedigree}.
#' @param ped A \code{data.frame} in the same format as the output of \code{simulatePedigree}.
#' @param ID_mate1 A vector of \code{ID} of the first mate. If not provided, the function will randomly select two individuals from the second generation.
#' @param ID_mate2 A vector of \code{ID} of the second mate.
#' @param verbose logical.  If TRUE, print progress through stages of algorithm
#' @param gen_inbred A vector of \code{generation} of the twin to be imputed.
#' @param type_inbred A character vector indicating the type of inbreeding. "sib" for sibling inbreeding and "cousin" for cousin inbreeding.
#' @return Returns a \code{data.frame} with some inbred mates.
#' @export

# A function to create inbred mates in the simulated pedigree.

makeInbreeding <- function(ped,
                           ID_mate1 = NA_integer_,
                           ID_mate2 = NA_integer_,
                           verbose = FALSE,
                           gen_inbred = 2,
                           type_inbred = "sib") {
  # A support function
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  # check if the ped is the same format as the output of simulatePedigree
  if (paste0(colnames(ped), collapse = "") != paste0(c("fam", "ID", "gen", "dadID", "momID", "spt", "sex"), collapse = "")) {
    ped <- standardize_colnames(ped)
    if (verbose) {
      cat("The input pedigree is not in the same format as the output of simulatePedigree\n")
    }
  }
  # check if the two IDs are provided
  if (is.na(ID_mate1) || is.na(ID_mate2)) {
    # Check if the generation is provided
    if (is.na(gen_inbred)) {
      stop("You should provide either the IDs of the inbred mates or the generation of the inbred mates")
    } else {
      # Check if the generation is valid
      if (gen_inbred < 2 || gen_inbred > max(ped$gen)) {
        stop("The generation of the twins should be an integer between 2 and the maximum generation in the pedigree")
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
            ID_pool_mate1 <- ped$ID[ped$gen == gen_inbred & !is.na(ped$dadID) & !is.na(ped$momID) & is.na(ped$spt) & !(ped$ID %in% usedID)]
            # if the pool is empty, find all individuals who have the same dadID and momID as the selected individual but mated
            if (length(ID_pool_mate1) == 0) {
              ID_pool_mate1 <- ped$ID[ped$gen == gen_inbred & !is.na(ped$dadID) & !is.na(ped$momID) & !(ped$ID %in% usedID)]
            }
            ID_mate1 <- resample(ID_pool_mate1, 1)
            usedID <- c(usedID, ID_mate1)
            # try to find one opposite-sex individual who has the same dadID and momID as the selected individual, preferalbly not mated
            ID_pool_mate2 <- ped$ID[ped$gen == gen_inbred & ped$sex != ped$sex[ped$ID == ID_mate1] & ped$dadID == ped$dadID[ped$ID == ID_mate1] & ped$momID == ped$momID[ped$ID == ID_mate1] & is.na(ped$spt)]
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
          cat("cousin inbreeding is not supported yet\n")
        } else {
          stop("The type of inbreeding should be either sib or cousin")
        }
      }
    }
  }
  # save the two individfual's former mates' IDs if they have any
  ID_mate1_former_mate <- ped$spt[ped$ID == ID_mate1]
  cat(ID_mate1, "\n")
  ID_mate2_former_mate <- ped$spt[ped$ID == ID_mate2]
  cat(ID_mate2, "\n")
  # remove two individuals' former mates from the pedigree if they have any
  ped$spt[ped$ID == ID_mate1] <- NA_integer_
  ped$spt[ped$ID == ID_mate2] <- NA_integer_
  # change the spouseID of ID_mate1 and ID_mate2 to each other
  ped$spt[ped$ID == ID_mate1] <- ID_mate2
  ped$spt[ped$ID == ID_mate2] <- ID_mate1
  # change the individuals in next generation whoes dadID and momID are ID_mate1 and ID_mate2's former mates to ID_mate1 and ID_mate2
  for (j in 1:nrow(ped)) {
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
