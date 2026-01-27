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
#' @param family_id_prefix A character string to prefix the family ID. Default is "fam".
#' @return A data frame representing the initial structure for the individuals in the specified generation
#'         before any relationships (parental, spousal) are defined. The columns include family ID (`fam`),
#'         individual ID (`id`), generation number (`gen`), father's ID (`pat`), mother's ID (`mat`),
#'         spouse's ID (`spID`), and sex (`sex`), with NA values for paternal, maternal, and spouse IDs, and sex.
#' @examples
#' sizeGens <- c(3, 5, 4) # Example sizes for 3 generations
#' genIndex <- 2 # Creating data frame for the 2nd generation
#' idGen <- 101:105 # Example IDs for the 2nd generation
#' df_Ngen <- createGenDataFrame(sizeGens, genIndex, idGen)
#' print(df_Ngen)
createGenDataFrame_beta <- function(sizeGens, genIndex, idGen,
                                    family_id_prefix = "fam") {
  n <- sizeGens[genIndex]
  df_Ngen <- data.frame(
    fam = rep(paste(family_id_prefix, 1), n, sep = ""),
    id = idGen[seq_len(n)],
    gen = rep.int(genIndex, n),
    pat = rep(NA, n), # father id
    mat = rep(NA, n), # mother id
    spID = rep(NA, n), # spouse id
    sex = rep(NA, n),
    stringsAsFactors = FALSE
  )
  return(df_Ngen)
}


#' Determine Sex of Offspring
#'
#' This internal function assigns sexes to the offspring in a generation based on the specified sex ratio.
#'
#' @param idGen Vector of IDs for the generation.
#' @param sexR Numeric value indicating the sex ratio (proportion of males).
#' @param code_male The value to use for males. Default is "M"
#' @param code_female The value to use for females. Default is "F"
#' @return Vector of sexes ("M" for male, "F" for female) for the offspring.
#' @importFrom stats runif

determineSex_beta <- function(idGen, sexR, code_male = "M", code_female = "F") {
  length_idGen <- length(idGen)
  if (runif(1) > .5) {
    sexVec1 <- rep(code_male, floor(length_idGen * sexR))
    sexVec2 <- rep(code_female, length_idGen - length(sexVec1))
  } else {
    sexVec1 <- rep(code_female, floor(length_idGen * (1 - sexR)))
    sexVec2 <- rep(code_male, length_idGen - length(sexVec1))
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
#' @keywords internal
#'
assignCoupleIDs_beta <- function(df_Ngen) {
  df_Ngen$coupleId <- NA_character_ # Initialize the coupleId column with NAs

  sp <- df_Ngen$spID
  id <- df_Ngen$id
  mated <- !is.na(sp)

  if (any(mated)) {
    lo <- pmin(id[mated], sp[mated])
    hi <- pmax(id[mated], sp[mated])
    key <- paste(lo, hi, sep = "_")

    # Assign coupleId for mated rows
    df_Ngen$coupleId[mated] <- key
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
#' @keywords internal
adjustKidsPerCouple_beta <- function(nMates, kpc, rd_kpc = TRUE) {
  if (rd_kpc == TRUE) {
    target <- nMates * kpc
    diff <- nMates + 1
    while (diff > nMates) {
      random_numbers <- stats::rpois(nMates, kpc)
      # cat("original random numbers", random_numbers, "\n")
      sum_random_numbers <- sum(random_numbers)
      diff <- abs(target - sum_random_numbers)
    }

    if (diff > 0) {
      if (sum_random_numbers < target) {
        # Add 1 to the smallest 'diff' entries, preserving original order afterwards
        order_random_numbers <- order(random_numbers) # indices of sorted ascending
        idx <- order_random_numbers[seq_len(diff)]
        random_numbers[idx] <- random_numbers[idx] + 1
      } else if (sum_random_numbers > target) {
        # make sure the sum of kids per couple is equal to the number of kids in the i th generation
        order_random_numbers <- order(random_numbers, decreasing = TRUE)
        idx <- order_random_numbers[seq_len(diff)]
        random_numbers[idx] <- random_numbers[idx] - 1
      }
    }
  } else {
    random_numbers <- rep.int(kpc, nMates)
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
#' @inheritParams determineSex
#' @param df_Ngen A data frame for the current generation being processed.
#'        It must include columns for individual IDs (`id`), spouse IDs (`spID`), sex (`sex`),
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

markPotentialChildren_beta <- function(df_Ngen, i, Ngen, sizeGens, CoupleF, code_male = "M", code_female = "F") {
  # Step 2.1: mark a group of potential sons and daughters in the i th generation

  # get all couple ids
  coupleID <- unique(df_Ngen$coupleId[!is.na(df_Ngen$coupleId)])
  if (i == Ngen) {
    CoupleF <- 0
  }

  if (CoupleF > 0L && length(coupleID) > 0L) {
    CoupleF <- min(CoupleF, length(coupleID))
    coupleGirl <- sample(coupleID, CoupleF)
  } else {
    coupleGirl <- character(0)
  }

  coupleBoy <- coupleID[!coupleID %in% coupleGirl]


  # single person should all be sons or daus
  # change the ifson and ifdau based on coupleGirl and coupleBoy
  is_single <- is.na(df_Ngen$spID)

  # Singles: based only on own sex
  single_f <- is_single & (df_Ngen$sex == code_female)
  single_m <- is_single & (df_Ngen$sex == code_male)
  df_Ngen$ifdau[single_f] <- TRUE
  df_Ngen$ifson[single_m] <- TRUE

  # Mated: based on couple assignment and sex restriction
  is_mated <- !is_single
  mated_son <- is_mated & (df_Ngen$coupleId %in% coupleBoy) & (df_Ngen$sex == code_male)
  mated_dau <- is_mated & (df_Ngen$coupleId %in% coupleGirl) & (df_Ngen$sex == code_female)
  df_Ngen$ifson[mated_son] <- TRUE
  df_Ngen$ifdau[mated_dau] <- TRUE

  df_Ngen <- df_Ngen[order(as.numeric(rownames(df_Ngen))), , drop = FALSE]
  df_Ngen <- df_Ngen[, -ncol(df_Ngen)]

  return(df_Ngen)
}
