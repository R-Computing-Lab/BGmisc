#' Create Data Frame for Generation
#' @rdname createGenDataFrame
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
#' @rdname determineSex
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
#' @rdname assignCoupleIDs
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
#' @rdname adjustKidsPerCouple
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

#' Mark Potential Children in a Generation
#' @rdname markPotentialChildren
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
