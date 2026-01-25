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
  if (beta == TRUE || beta == "optimized") {
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
      fam_shift = fam_shift
    )
  } else if (beta %in% c("index", "indexed")) {
    df_Fam <- buildWithinGenerations_index(
      # buildWithinGenerations_base(
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
      fam_shift = fam_shift
    )
  } else if (beta == FALSE || is.null(beta)) {
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
      fam_shift = fam_shift
    )
  } else {
    stop("Invalid value for parameter 'beta'. Accepted values are TRUE, FALSE, 'optimized', or 'index'.")
  }
  return(df_Fam)
}


buildWithinGenerations_base <- function(sizeGens,
                                        marR,
                                        sexR,
                                        Ngen, verbose = FALSE,
                                        personID = "ID",
                                        momID = "momID",
                                        dadID = "dadID",
                                        code_male = "M",
                                        code_female = "F",
                                        fam_shift = 1L) {
  idx_width <- nchar(max(sizeGens))
  gen_width <- max(2L, nchar(Ngen))
  #  fam_shift <- 1L

  for (i in 1:Ngen) {
    # idGen <- as.numeric(paste(100, i, 1:sizeGens[i], sep = ""))
    idGen <- fam_shift * 10^(gen_width + idx_width) + i * 10^(idx_width) + (1:sizeGens[i])
    # idGen <- ifelse(i==1,
    #                 paste(i,"-",1:sizeGens[i]),
    #                 paste(i,"-",sizeGens[i-1]:sizeGens[i]))

    ### For each generation, create a separate dataframe
    df_Ngen <- createGenDataFrame(
      sizeGens = sizeGens,
      genIndex = i,
      idGen = idGen
    )

    ### Let's deal with the sex in each generation first

    df_Ngen$sex <- determineSex(
      idGen = idGen, sexR = sexR,
      code_male = code_male,
      code_female = code_female
    )

    # message(paste("tiger",i))
    # The first generation
    if (i == 1) {
      df_Ngen$spID[1] <- df_Ngen$id[2]
      df_Ngen$spID[2] <- df_Ngen$id[1]

      df_Ngen$sex[1] <- code_female
      df_Ngen$sex[2] <- code_male
    }

    ## Connect male and female into couples in each generations
    marR_crt <- (1 + marR) / 2
    usedFemaleIds <- numeric()
    usedMaleIds <- numeric()
    # reserve the single persons
    if (i != 1 && i != Ngen) {
      nMarriedFemale <- round(sum(df_Ngen$sex == code_female) * marR_crt)
      nMarriedMale <- round(sum(df_Ngen$sex == code_male) * marR_crt)

      # make sure there are same numbers of married males and females
      if (nMarriedFemale >= nMarriedMale) {
        nMarriedFemale <- nMarriedMale
      } else {
        nMarriedMale <- nMarriedFemale
      }
      # get the number of single males and females
      nSingleFemale <- sum(df_Ngen$sex == code_female) - nMarriedFemale

      if (nSingleFemale < 0) {
        nSingleFemale <- 0
        usedFemaleIds <- numeric()
      } else {
        usedFemaleIds <- sample(df_Ngen$id[df_Ngen$sex == code_female], nSingleFemale)
      }

      nSingleMale <- sum(df_Ngen$sex == code_male) - nMarriedMale
      if (nSingleMale < 0) {
        nSingleMale <- 0
        usedMaleIds <- numeric()
      } else {
        usedMaleIds <- sample(df_Ngen$id[df_Ngen$sex == code_male], nSingleMale)
      }
      # sample single ids from male ids and female ids

      usedIds <- c(usedFemaleIds, usedMaleIds)

      # Create spouses
      for (j in seq_len(nrow(df_Ngen))) {
        if (df_Ngen$id[j] %in% usedIds) {
          next
        } else {
          # idx <- j+1
          if (df_Ngen$sex[j] == code_female) {
            for (k in seq_len(nrow(df_Ngen))) {
              idr <- df_Ngen$id[k]
              tgt <- (!(idr %in% usedIds)) & df_Ngen$sex[k] == code_male
              # tgt <- ifelse(is.na(tgt),FALSE,TRUE)
              if (tgt) {
                df_Ngen$spID[j] <- df_Ngen$id[k]
                df_Ngen$spID[k] <- df_Ngen$id[j]
                usedIds <- c(usedIds, df_Ngen$id[j], df_Ngen$id[k])
                break
              } else {
                next
              }
            }
          } else {
            for (k in seq_len(nrow(df_Ngen))) {
              idr <- df_Ngen$id[k]
              tgt <- (!(idr %in% usedIds)) & df_Ngen$sex[k] == code_female
              # tgt <- ifelse(is.na(tgt),FALSE,TRUE)
              if (tgt) {
                df_Ngen$spID[j] <- df_Ngen$id[k]
                df_Ngen$spID[k] <- df_Ngen$id[j]
                usedIds <- c(usedIds, df_Ngen$id[j], df_Ngen$id[k])
                break
              } else {
                next
              }
            }
          }
        }
        # message(usedIds)
      }
    }
    if (i == 1) {
      df_Fam <- df_Ngen
    } else {
      df_Fam <- rbind(df_Fam, df_Ngen)
    }
  }
  return(df_Fam)
}


buildWithinGenerations_optimized <- function(sizeGens, marR, sexR, Ngen, verbose = FALSE,
                                             personID = "ID",
                                             momID = "momID",
                                             dadID = "dadID",
                                             code_male = "M",
                                             code_female = "F",
                                             fam_shift = 1L) {
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
      idGen = idGen
    )

    ### Let's deal with the sex in each generation first

    df_Ngen$sex <- determineSex(
      idGen = idGen, sexR = sexR,
      code_male = code_male,
      code_female = code_female
    )

    # message(paste("tiger",i))
    # The first generation
    if (i == 1) {
      df_Ngen$spID[1] <- df_Ngen$id[2]
      df_Ngen$spID[2] <- df_Ngen$id[1]

      df_Ngen$sex[1] <- code_female
      df_Ngen$sex[2] <- code_male
    }


    usedFemaleIds <- numeric()
    usedMaleIds <- numeric()

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
      if (nMarriedFemale >= nMarriedMale) {
        nMarriedFemale <- min(nMarriedMale, totalFemale)
      } else {
        nMarriedMale <- nMarriedFemale
      }
      if (nMarriedFemale > totalFemale) {
        nMarriedFemale <- totalFemale
      }
      if (nMarriedMale > totalMale) {
        nMarriedMale <- totalMale
      }


      # get the number of single males and females
      nSingleFemale <- max(totalFemale - nMarriedFemale, 0)
      nSingleMale <- max(totalMale - nMarriedMale, 0)


      # sample single ids from male ids and female ids
      usedFemaleIds <- sample(df_Ngen$id[isFemale], nSingleFemale)
      usedMaleIds <- sample(df_Ngen$id[isMale], nSingleMale)

      isUsed <- df_Ngen$id %in% c(usedFemaleIds, usedMaleIds)

      # Create spouses
      nrows_df_Ngen <- nrow(df_Ngen)
      availFemale <- which(isFemale & !isUsed)
      availMale <- which(isMale & !isUsed)

      # next unused pointer
      ptrFemale <- 1L
      ptrMale <- 1L


      for (j in seq_len(nrows_df_Ngen)) {
        if (isUsed[j]) {
          next
        }

        if (df_Ngen$sex[j] == code_female) {
          # only runs when the person is not used
          while (ptrMale <= length(availMale) && isUsed[availMale[ptrMale]]) {
            ptrMale <- ptrMale + 1L
          }
          # if all used males, skip
          if (ptrMale > length(availMale)) {
            next
          }
          k <- availMale[ptrMale]
          ptrMale <- ptrMale + 1L

          #  spouse_sex_code <- code_male
        } else {
          while (ptrFemale <= length(availFemale) && isUsed[availFemale[ptrFemale]]) {
            ptrFemale <- ptrFemale + 1L
          }
          if (ptrFemale > length(availFemale)) {
            next
          }
          k <- availFemale[ptrFemale]
          ptrFemale <- ptrFemale + 1L

          # spouse_sex_code <- code_female
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


buildWithinGenerations_index <- function(sizeGens, marR, sexR, Ngen, verbose = FALSE,
                                         personID = "ID",
                                         momID = "momID",
                                         dadID = "dadID",
                                         code_male = "M",
                                         code_female = "F",
                                         fam_shift = 1L) {
  idx_width <- nchar(max(sizeGens))
  gen_width <- max(2L, nchar(Ngen))


  # Precompute powers once
  pow_idx <- 10^idx_width
  pow_gen <- 10^(gen_width + idx_width)

  ## Connect male and female into couples in each generations
  marR_crt <- (1 + marR) / 2

  for (i in seq_len(Ngen)) {
    # idGen <- as.numeric(paste(100, i, 1:sizeGens[i], sep = ""))

    idGen <- fam_shift * pow_gen + i * pow_idx + seq_len(sizeGens[i])


    ### For each generation, create a separate dataframe
    df_Ngen <- createGenDataFrame(
      sizeGens = sizeGens,
      genIndex = i,
      idGen = idGen
    )

    ### Let's deal with the sex in each generation first

    df_Ngen$sex <- determineSex(
      idGen = idGen, sexR = sexR,
      code_male = code_male,
      code_female = code_female
    )

    # message(paste("tiger",i))
    # The first generation
    if (i == 1) {
      df_Ngen$spID[1] <- df_Ngen$id[2]
      df_Ngen$spID[2] <- df_Ngen$id[1]

      df_Ngen$sex[1] <- code_female
      df_Ngen$sex[2] <- code_male
    }


    usedFemaleIds <- numeric()
    usedMaleIds <- numeric()

    # reserve the single persons
    if (i != 1 && i != Ngen) {
      totalFemale <- sum(df_Ngen$sex == code_female)
      totalMale <- sum(df_Ngen$sex == code_male)
      nMarriedFemale <- round(totalFemale * marR_crt)
      nMarriedMale <- round(totalMale * marR_crt)
      # make sure there are same numbers of married males and females
      if (nMarriedFemale >= nMarriedMale) {
        nMarriedFemale <- nMarriedMale
      } else {
        nMarriedMale <- nMarriedFemale
      }
      # get the number of single males and females
      nSingleFemale <- totalFemale - nMarriedFemale
      nSingleMale <- totalMale - nMarriedMale


      # sample single ids from male ids and female ids
      if (nSingleFemale < 0) {
        nSingleFemale <- 0
        message("Warning: Negative number of single women available; setting to 0")
        usedFemaleIds <- numeric()
      } else {
        usedFemaleIds <- sample(df_Ngen$id[df_Ngen$sex == code_female], nSingleFemale)
      }
      ## message(c("Used F", usedFemaleIds))
      if (nSingleMale < 0) {
        nSingleMale <- 0
        message("Warning: Negative number of single men available; setting to 0")
        usedMaleIds <- numeric()
      } else {
        usedMaleIds <- sample(df_Ngen$id[df_Ngen$sex == code_male], nSingleMale)
      }
      ## message(c("Used M", usedMaleIds))
      #   usedIds <- c(usedFemaleIds, usedMaleIds)
      isUsed <- df_Ngen$id %in% c(usedFemaleIds, usedMaleIds)

      # Create spouses
      for (j in seq_len(nrow(df_Ngen))) {
        if (isUsed[j]) {
          next
        } else {
          if (df_Ngen$sex[j] == code_female) {
            spouse_sex_code <- code_male
          } else {
            spouse_sex_code <- code_female
          }
          for (k in seq_len(nrow(df_Ngen))) {
            tgt <- (!isUsed[k]) & df_Ngen$sex[k] == spouse_sex_code

            if (tgt) {
              df_Ngen$spID[j] <- df_Ngen$id[k]
              df_Ngen$spID[k] <- df_Ngen$id[j]

              isUsed[j] <- TRUE
              isUsed[k] <- TRUE
              break
            } else {
              next
            }
          }
        }
        # message(usedIds)
      }
    }
    if (i == 1) {
      df_Fam <- df_Ngen
    } else {
      df_Fam <- rbind(df_Fam, df_Ngen)
    }
  }
  return(df_Fam)
}
