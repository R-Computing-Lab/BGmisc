#' allGens
#' A function to calculate the number of individuals in each generation. This is a supporting function for \code{simulatePedigree}.
#' @param kpc Number of kids per couple (integer >= 2).
#' @param Ngen Number of generations (integer >= 1).
#' @param marR Mating rate (numeric value ranging from 0 to 1).
#' @return Returns a vector containing the number of individuals in every generation.
#' @export
allGens <- function(kpc, Ngen, marR) {
  # Check if the number of generations is valid
  if (Ngen < 1) {
    stop("The number of generations should be an integer greater or equal than 1")
  }
  if (Ngen == 1) {
    allGens <- 2
  }
  if (Ngen >= 2) {
    allGens <- sizeAllGens(
      kpc = kpc,
      Ngen = Ngen,
      marR = marR
    )
  } else {
    stop()
  }
  return(allGens)
}


#' sizeAllGens
#' An internal supporting function for \code{simulatePedigree}.
#' @inheritParams allGens
#' @return Returns a vector including the number of individuals in every generation.

sizeAllGens <- function(kpc, Ngen, marR) {
  Nmid <- Ngen - 2
  midGens <- numeric(length = Nmid)

  # Calculate the number of individuals for middle generations
  for (i in 2:(Ngen - 1)) {
    midGens[i - 1] <- kpc^(i - 1) * marR^(i - 2) * (1 + marR)
    midGens[i - 1] <- ceiling(midGens[i - 1])
  }

  # Calculate the number of individuals for the last generation
  lastGen <- ceiling(kpc^(Ngen - 1) * marR^(Ngen - 2))
  allGens <- c(2, midGens, lastGen)
  # print(allGens)
  return(allGens)
}


#' famSizeCal
#' A function to calculate the total number of individuals in a pedigree given parameters. This is a supporting function for function \code{simulatePedigree}
#' @inheritParams allGens
#' @return Returns a numeric value indicating the total pedigree size.
#' @export
famSizeCal <- function(kpc, Ngen, marR) {
  if (Ngen < 1) {
    stop("The number of generations should be an integer greater or equal than 1")
  }
  if (Ngen == 1) {
    size <- 2
  }
  if (Ngen >= 2) {
    allGens <- sizeAllGens(
      kpc = kpc,
      Ngen = Ngen,
      marR = marR
    )
    size <- sum(allGens)
  } else {
    stop("You should never see this message.
    If you do, that means that famSizeCal is not working properly.")
  }
  return(size)
}


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
