#' Process Generations for Pedigree Simulation
#'
#' This function iterates through generations in a pedigree simulation, assigning IDs,
#' creating data frames, determining sexes, and managing pairing within each generation.
#'
#' @inheritParams simulatePedigree
#' @inheritParams createGenDataFrame
#' @return A data frame representing the simulated pedigree, including columns for family ID (`fam`),

buildWithinGenerations <- function(
  beta = FALSE,
  sizeGens, marR, sexR, Ngen, verbose = FALSE,
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  code_male = "M",
  code_female = "F",
  fam_shift = 1L
) {
  # Normalize string aliases to logical values for downstream functions
  use_optimized <- FALSE

  if (beta %in% c("index", "indexed")) {
    stop("The 'index' or 'indexed' option for parameter 'beta' is not yet implemented.")
  } else if (isTRUE(beta) || identical(beta, "optimized")) {
    use_optimized <- TRUE
  } else if (isFALSE(beta) || beta %in% c("base", "original") || is.null(beta)) {
    use_optimized <- FALSE
  } else {
    stop("Invalid value for parameter 'beta'. Accepted values are TRUE, FALSE, 'optimized', 'base', 'original', or 'index'/'indexed'.")
  }

  if (use_optimized) {
    df_Fam <- buildWithinGenerations_optimized(
      sizeGens = sizeGens,
      marR = marR,
      sexR = sexR,
      Ngen = Ngen,
      verbose = verbose,
      personID = personID,
      momID = momID,
      dadID = dadID,
      code_male = code_male,
      code_female = code_female,
      fam_shift = fam_shift,
      beta = TRUE
    )
  } else {
    df_Fam <- buildWithinGenerations_base(
      sizeGens = sizeGens,
      marR = marR,
      sexR = sexR,
      Ngen = Ngen,
      verbose = verbose,
      personID = personID,
      momID = momID,
      dadID = dadID,
      code_male = code_male,
      code_female = code_female,
      fam_shift = fam_shift,
      beta = FALSE
    )
  }
  return(df_Fam)
}


buildWithinGenerations_base <- function(sizeGens, marR, sexR, Ngen, verbose = FALSE,
                                        personID = "ID",
                                        momID = "momID",
                                        dadID = "dadID",
                                        code_male = "M",
                                        code_female = "F",
                                        fam_shift = 1L,
                                        beta = FALSE) {
  idx_width <- nchar(max(sizeGens))
  gen_width <- max(2L, nchar(Ngen))


  # Precompute powers once
  pow_idx <- 10^idx_width
  pow_gen <- 10^(gen_width + idx_width)

  ## Connect male and female into couples in each generations
  marR_crt <- (1 + marR) / 2

  # Initialize a list to store data frames for each generation
  df_list <- vector("list", Ngen)


  for (i in seq_len(Ngen)) {
    # idGen <- as.numeric(paste(100, i, 1:sizeGens[i], sep = ""))

    idGen <- fam_shift * pow_gen + i * pow_idx + seq_len(sizeGens[i])


    ### For each generation, create a separate dataframe
    df_Ngen <- createGenDataFrame(
      sizeGens = sizeGens,
      genIndex = i,
      idGen = idGen,
      beta = beta
    )

    ### Let's deal with the sex in each generation first

    df_Ngen$sex <- determineSex(
      idGen = idGen, sexR = sexR,
      code_male = code_male,
      code_female = code_female,
      beta = beta
    )

    # The first generation
    if (i == 1) {
      df_Ngen$spID[1] <- df_Ngen$id[2]
      df_Ngen$spID[2] <- df_Ngen$id[1]

      df_Ngen$sex[1] <- code_female
      df_Ngen$sex[2] <- code_male
    }


    # reserve the single persons
    if (i != 1 && i != Ngen) {
      # is faster
      isFemale <- df_Ngen$sex == code_female
      isMale <- df_Ngen$sex == code_male

      # get the number
      totalFemale <- sum(isFemale)
      totalMale <- sum(isMale)

      nMarriedFemale <- round(totalFemale * marR_crt)
      nMarriedMale <- round(totalMale * marR_crt)

      # make sure there are same numbers of married males and females
      nMarriedMale <- nMarriedFemale <- min(nMarriedFemale, nMarriedMale)


      #
      if (nMarriedFemale > totalFemale) {
        nMarriedFemale <- totalFemale
      }
      if (nMarriedMale > totalMale) {
        nMarriedMale <- totalMale
      }
      # get the number of single males and females
      nSingleFemale <- max(totalFemale - nMarriedFemale, 0)
      nSingleMale <- max(totalMale - nMarriedMale, 0)


      # sample single ids from male ids and female ids (guard against size > available)
      femaleIds <- df_Ngen$id[isFemale]
      maleIds <- df_Ngen$id[isMale]

      nSingleFemale <- min(nSingleFemale, length(femaleIds))
      nSingleMale <- min(nSingleMale, length(maleIds))

      usedFemaleIds <- if (nSingleFemale > 0) sample(femaleIds, nSingleFemale) else numeric()
      usedMaleIds <- if (nSingleMale > 0) sample(maleIds, nSingleMale) else numeric()

      isUsed <- df_Ngen$id %in% c(usedFemaleIds, usedMaleIds)

      # Create spouses
      nrows_df_Ngen <- nrow(df_Ngen)
      availFemale <- which(isFemale & !isUsed)
      availMale <- which(isMale & !isUsed)

      length_availMale <- length(availMale)
      length_availFemale <- length(availFemale)

      # next unused pointer
      ptrFemale <- 1L
      ptrMale <- 1L


      for (j in seq_len(nrows_df_Ngen)) {
        if (isUsed[j]) {
          next
        }

        if (df_Ngen$sex[j] == code_female) {
          # only runs when the person is not used
          while (ptrMale <= length_availMale && isUsed[availMale[ptrMale]]) {
            ptrMale <- ptrMale + 1L
          }
          # if all used males, skip
          if (ptrMale > length_availMale) {
            next
          }
          k <- availMale[ptrMale]
          ptrMale <- ptrMale + 1L
        } else {
          while (ptrFemale <= length_availFemale && isUsed[availFemale[ptrFemale]]) {
            ptrFemale <- ptrFemale + 1L
          }
          if (ptrFemale > length_availFemale) {
            next
          }
          k <- availFemale[ptrFemale]
          ptrFemale <- ptrFemale + 1L
        }


        df_Ngen$spID[j] <- df_Ngen$id[k]
        df_Ngen$spID[k] <- df_Ngen$id[j]

        isUsed[j] <- TRUE
        isUsed[k] <- TRUE
      }
    }

    df_list[[i]] <- df_Ngen
  }

  df_Fam <- do.call(rbind, df_list)
  rownames(df_Fam) <- NULL
  return(df_Fam)
}

buildWithinGenerations_optimized <- buildWithinGenerations_base
