#' Create Data Frame for Generation
#'
#' This function creates a data frame for a specific generation within the simulated pedigree.
#' It initializes the data frame with default values for family ID, individual ID, generation number,
#' paternal ID, maternal ID, spouse ID, and sex. All individuals are initially set with NA for paternal,
#' maternal, spouse IDs, and sex, awaiting further assignment.
#'
#' @param sizeGens A numeric vector containing the sizes of each generation within the pedigree.
#' @param genIndex An integer representing the current generation index for which the data frame is being created.
#' @param idGen A numeric vector containing the ID numbers to be assigned to individuals in the current generation.
#' @return A data frame representing the initial structure for the individuals in the specified generation
#'         before any relationships (parental, spousal) are defined. The columns include family ID (`fam`),
#'         individual ID (`id`), generation number (`gen`), father's ID (`pat`), mother's ID (`mat`),
#'         spouse's ID (`spt`), and sex (`sex`), with NA values for paternal, maternal, and spouse IDs, and sex.
#' @examples
#' sizeGens <- c(3, 5, 4) # Example sizes for 3 generations
#' genIndex <- 2 # Creating data frame for the 2nd generation
#' idGen <- 101:105 # Example IDs for the 2nd generation
#' df_Ngen <- createGenDataFrame(sizeGens, genIndex, idGen)
#' print(df_Ngen)
#' @export
createGenDataFrame <- function(sizeGens, genIndex, idGen) {
  df_Ngen <- data.frame(
    fam = rep(paste("fam", 1), sizeGens[genIndex], sep = ""),
    id = idGen[1:sizeGens[genIndex]],
    gen = rep(genIndex, sizeGens[genIndex]),
    pat = rep(NA, sizeGens[genIndex]), # father id
    mat = rep(NA, sizeGens[genIndex]), # mother id
    spt = rep(NA, sizeGens[genIndex]), # spouse id
    sex = rep(NA, sizeGens[genIndex])
  )
  return(df_Ngen)
}


#' Determine Sex of Offspring
#'
#' This function assigns sexes to the offspring in a generation based on the specified sex ratio.
#'
#' @param idGen Vector of IDs for the generation.
#' @param sexR Numeric value indicating the sex ratio (proportion of males).
#' @return Vector of sexes ("M" for male, "F" for female) for the offspring.
#' @importFrom stats runif
#' @export
determineSex <- function(idGen, sexR) {
  if (runif(1) > .5) {
    sexVec1 <- rep("M", floor(length(idGen) * sexR))
    sexVec2 <- rep("F", length(idGen) - length(sexVec1))
  } else {
    sexVec1 <- rep("F", floor(length(idGen) * (1 - sexR)))
    sexVec2 <- rep("M", length(idGen) - length(sexVec1))
  }
  sexVec <- sample(c(sexVec1, sexVec2))
  return(sexVec)
}

#' Assign Couple IDs
#'
#' This subfunction assigns a unique couple ID to each mated pair in the generation.
#' Unmated individuals are assigned NA for their couple ID.
#'
#' @param df_Ngen The dataframe for the current generation, including columns for individual IDs and spouse IDs.
#' @return The input dataframe augmented with a 'coupleId' column, where each mated pair has a unique identifier.
assignCoupleIds <- function(df_Ngen) {
  df_Ngen$coupleId <- NA_character_ # Initialize the coupleId column with NAs
  usedCoupleIds <- character() # Initialize an empty character vector to track used IDs

  for (j in seq_len(nrow(df_Ngen))) {
    if (!is.na(df_Ngen$spt[j]) && is.na(df_Ngen$coupleId[j])) {
      # Construct a potential couple ID from sorted individual and spouse IDs
      sortedIds <- sort(c(df_Ngen$id[j], df_Ngen$spt[j]))
      potentialCoupleId <- paste(sortedIds[1], sortedIds[2], sep = "_")

      # Check if the potentialCoupleId has not already been used
      if (!potentialCoupleId %in% usedCoupleIds) {
        # Assign the new couple ID to both partners
        df_Ngen$coupleId[j] <- potentialCoupleId
        spouseIndex <- which(df_Ngen$id == df_Ngen$spt[j])
        df_Ngen$coupleId[spouseIndex] <- potentialCoupleId

        # Add the new couple ID to the list of used IDs
        usedCoupleIds <- c(usedCoupleIds, potentialCoupleId)
      }
    }
  }

  return(df_Ngen)
}

#' Generate or Adjust Number of Kids per Couple Based on Mating Rate
#'
#' This function generates or adjusts the number of kids per couple in a generation
#' based on the specified average and whether the count should be randomly determined.
#'
#' @param nMates Integer, the number of mated pairs in the generation.
#' @inheritParams simulatePedigree
#'
#' @return A numeric vector with the generated or adjusted number of kids per couple.
adjustKidsPerCouple <- function(nMates, kpc, rd_kpc) {
  if (rd_kpc) {
    # cat("number of mates",nMates, "\n")

    diff <- nMates + 1
    while (diff > nMates) {
      random_numbers <- stats::rpois(nMates, kpc)
      # cat("original random numbers", random_numbers, "\n")
      diff <- abs(nMates * kpc - sum(random_numbers))
    }
    # make sure the sum of kids per couple is equal to the number of kids in the i th generation
    if (sum(random_numbers) < nMates * kpc) {
      names(random_numbers) <- seq_along(random_numbers)
      random_numbers <- sort(random_numbers)
      random_numbers[1:diff] <- random_numbers[1:diff] + 1
      random_numbers <- random_numbers[order(names(random_numbers))]
    } else if (sum(random_numbers) > nMates * kpc) {
      names(random_numbers) <- seq_along(random_numbers)
      random_numbers <- sort(random_numbers, decreasing = TRUE)
      random_numbers[1:diff] <- random_numbers[1:diff] - 1
      random_numbers <- random_numbers[order(names(random_numbers))]
    }
  } else {
    random_numbers <- rep(kpc, nMates)
  }

  if (min(random_numbers) < 0) {
    random_numbers[random_numbers == -1] <- 0
    random_numbers[random_numbers == max(random_numbers)] <- max(random_numbers) - 1
  }

  return(random_numbers)
}

#' Mark and Assign children
#'
#' This subfunction marks individuals in a generation as potential sons, daughters, 
#' or parents based on their relationships and assigns unique couple IDs. It processes
#' the assignment of roles and relationships within and between generations in a pedigree simulation.
#'
#' @param df_Ngen A data frame for the current generation being processed.
#'        It must include columns for individual IDs (`id`), spouse IDs (`spt`), sex (`sex`),
#'        and any previously assigned roles (`ifparent`, `ifson`, `ifdau`).
#' @param i Integer, the index of the current generation being processed.
#' @param Ngen Integer, the total number of generations in the simulation.
#' @param sizeGens Numeric vector, containing the size (number of individuals) of each generation.
#' @param CoupleF Integer, IT MIGHT BE the number of couples in the current generation.
#'
#' 
#' @return Modifies `df_Ngen` in place by updating or adding columns related to individual roles
#'         (`ifparent`, `ifson`, `ifdau`) and couple IDs (`coupleId`). The updated data frame is
#'         also returned for integration into the larger pedigree data frame (`df_Fam`).
#'

markPotentialChildren <- function(df_Ngen, i, Ngen, sizeGens,CoupleF) {

     # Step 2.1: mark a group of potential sons and daughters in the i th generation

      # get all couple ids
      coupleID <- unique(df_Ngen$coupleId[!is.na(df_Ngen$coupleId)])
      if (i == Ngen) {
        CoupleF <- 0
      }
      coupleGirl <- sample(coupleID, CoupleF)
      coupleBoy <- coupleID[!coupleID %in% coupleGirl]
      # single person should all be sons or daus
      # change the ifson and ifdau based on coupleGirl and coupleBoy
      for (j in 1:sizeGens[i]) {
        if (is.na(df_Ngen$spt[j])) {
          if (df_Ngen$sex[j] == "F") {
            df_Ngen$ifdau[j] <- TRUE
            # usedIds <- c(usedIds, df_Ngen$id[j])
          } else {
            df_Ngen$ifson[j] <- TRUE
            # usedIds <- c(usedIds, df_Ngen$id[j])
          }
        } else {
          if (df_Ngen$coupleId[j] %in% coupleBoy && df_Ngen$sex[j] == "M") {
            df_Ngen$ifson[j] <- TRUE
          } else if (df_Ngen$coupleId[j] %in% coupleGirl && df_Ngen$sex[j] == "F") {
            df_Ngen$ifdau[j] <- TRUE
          } else {
            next
          }
        }
      }

      df_Ngen <- df_Ngen[order(as.numeric(rownames(df_Ngen))), , drop = FALSE]
      df_Ngen <- df_Ngen[, -ncol(df_Ngen)]

      return(df_Ngen)
}
