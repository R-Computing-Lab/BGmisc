#' Process Generations for Pedigree Simulation
#'
#' This function iterates through generations in a pedigree simulation, assigning IDs,
#' creating data frames, determining sexes, and managing pairing within each generation.
#'
#' @inheritParams simulatePedigree
#' @inheritParams createGenDataFrame
#' @return A data frame representing the simulated pedigree, including columns for family ID (`fam`),
buildWithinGenerations <- function(sizeGens, marR, sexR, Ngen, verbose = FALSE,
                                   personID = "ID",
                                   momID = "momID",
                                   dadID = "dadID",
                                   code_male = "M", code_female = "F") {
  for (i in 1:Ngen) {
    idGen <- as.numeric(paste(100, i, 1:sizeGens[i], sep = ""))
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
      nSingleMale <- sum(df_Ngen$sex == code_male) - nMarriedMale


      # sample single ids from male ids and female ids
      usedFemaleIds <- sample(df_Ngen$id[df_Ngen$sex == code_female], nSingleFemale)
      ## message(c("Used F", usedFemaleIds))
      usedMaleIds <- sample(df_Ngen$id[df_Ngen$sex == code_male], nSingleMale)
      ## message(c("Used M", usedMaleIds))

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


#' Process Generation Connections
#'
#' This function processes connections between each two generations in a pedigree simulation.
#' It marks individuals as parents, sons, or daughters based on their generational position and relationships.
#' The function also handles the assignment of couple IDs, manages single and coupled individuals,
#' and establishes parent-offspring links across generations.
#' @param df_Fam A data frame containing the simulated pedigree information up to the current generation.
#'               Must include columns for family ID, individual ID, generation number, spouse ID (spID),
#'               and sex. This data frame is updated in place to include flags for parental status (ifparent),
#'               son status (ifson), and daughter status (ifdau), as well as couple IDs.
#' @inheritParams simulatePedigree
#' @inheritParams createGenDataFrame
#'
#' @details
#' The function iterates through each generation, starting from the second, to establish connections based on mating and parentage.
#' For the first generation, it sets the parental status directly. For subsequent generations, it calculates the number of couples,
#' the expected number of offspring, and assigns offspring to parents. It handles gender-based assignments for sons and daughters,
#' and deals with the nuances of single individuals and couple formation. The function relies on external functions `assignCoupleIds`
#' and `adjustKidsPerCouple` to handle specific tasks related to couple ID assignment and offspring number adjustments, respectively.
#'
#' @return The function updates the `df_Fam` data frame in place, adding or modifying columns related to parental and offspring status,
#'         as well as assigning unique couple IDs. It does not return a value explicitly.
#'

buildBetweenGenerations <- function(df_Fam, Ngen, sizeGens, verbose = FALSE, marR, sexR, kpc,
                                    rd_kpc, personID = "ID",
                                    momID = "momID",
                                    dadID = "dadID",
                                    code_male = "M", code_female = "F") {
  df_Fam$ifparent <- FALSE
  df_Fam$ifson <- FALSE
  df_Fam$ifdau <- FALSE
  for (i in 1:Ngen) {
    # generation 1 doesn't need any mother and father
    if (i == 1) {
      df_Ngen <- df_Fam[df_Fam$gen == i, ]
      df_Ngen$ifparent <- TRUE
      df_Ngen$ifson <- FALSE
      df_Ngen$ifdau <- FALSE
      df_Fam[df_Fam$gen == i, ] <- df_Ngen
    } else {
      # calculate the number of couples in the i-1 th generation
      N_couples <- (sizeGens[i - 1] - sum(is.na(df_Fam$spID[df_Fam$gen == i - 1]))) * 0.5
      # calculate the number of members in the i th generation that have a link to the couples in the i-1 th generation
      N_LinkedMem <- N_couples * kpc
      # decompose the linked members into females and males respectively
      N_LinkedFemale <- round(N_LinkedMem * (1 - sexR))
      N_LinkedMale <- N_LinkedMem - N_LinkedFemale

      # Create a pool for used male children and female children respectively
      usedFemaleIds <- numeric()
      usedMaleIds <- numeric()
      usedIds <- c(usedFemaleIds, usedMaleIds)

      # get the df for the i the generation
      df_Ngen <- df_Fam[df_Fam$gen == i, ]
      df_Ngen$ifparent <- FALSE
      df_Ngen$ifson <- FALSE
      df_Ngen$ifdau <- FALSE
      df_Ngen$coupleId <- NA_character_
      df_Ngen <- df_Ngen[sample(nrow(df_Ngen)), ]

      # Start to connect children with mother and father
      #
      if (verbose == TRUE) {
        message(
          "Step 2.1: mark a group of potential sons and daughters in the i th generation"
        )
      }

      # try to rewrite the code
      # count the number of couples in the i th gen
      countCouple <- (nrow(df_Ngen) - sum(is.na(df_Ngen$spID))) * .5

      # Now, assign couple IDs for the current generation
      df_Ngen <- assignCoupleIds(df_Ngen)

      # get the number of linked female and male children after excluding the single children
      # get a vector of single person id in the ith generation
      IdSingle <- df_Ngen$id[is.na(df_Ngen$spID)]
      SingleF <- sum(df_Ngen$sex == code_female & is.na(df_Ngen$spID))
      CoupleF <- N_LinkedFemale - SingleF
      SingleM <- sum(df_Ngen$sex == code_male & is.na(df_Ngen$spID))
      #     CoupleM <- N_LinkedMale - SingleM

      df_Fam[df_Fam$gen == i, ] <- markPotentialChildren(
        df_Ngen = df_Ngen,
        i = i,
        Ngen = Ngen,
        sizeGens = sizeGens,
        CoupleF = CoupleF,
        code_male = code_male,
        code_female = code_female
      )
      if (verbose == TRUE) {
        message(
          "Step 2.2: mark a group of potential parents in the i-1 th generation"
        )
      }
      df_Ngen <- df_Fam[df_Fam$gen == i - 1, ]
      df_Ngen$ifparent <- FALSE
      df_Ngen$ifson <- FALSE
      df_Ngen$ifdau <- FALSE
      df_Ngen <- df_Ngen[sample(nrow(df_Ngen)), ]
      # Create a pool for the used parents
      usedParentIds <- numeric()

      for (k in 1:sizeGens[i - 1]) {
        # first check if the number of married couples surpass the marriage rate
        if (sum(df_Ngen$ifparent) / nrow(df_Ngen) >= marR) {
          break
        } else {
          # check if the id is used and if the member has married
          if (!(df_Ngen$id[k] %in% usedParentIds) && !is.na(df_Ngen$spID[k])) {
            df_Ngen$ifparent[k] <- TRUE
            df_Ngen$ifparent[df_Ngen$spID == df_Ngen$id[k]] <- TRUE
            usedParentIds <- c(usedParentIds, df_Ngen$id[k], df_Ngen$spID[k])
          } else {
            next
          }
        }
      }

      df_Ngen <- df_Ngen[order(as.numeric(rownames(df_Ngen))), , drop = FALSE]
      df_Fam[df_Fam$gen == i - 1, ] <- df_Ngen
      if (verbose == TRUE) {
        message(
          "Step 2.3: connect the i and i-1 th generation"
        )
      }
      if (i == 1) {
        next
      } else {
        # get the df for i and i-1 th generations
        df_Ngen <- df_Fam[df_Fam$gen %in% c(i, i - 1), ]
        sizeI <- sizeGens[i - 1]
        sizeII <- sizeGens[i]
        # create a vector with ordered ids that should be connected to a parent
        # message(df_Ngen)
        IdSon <- df_Ngen$id[df_Ngen$ifson == TRUE & df_Ngen$gen == i]
        # message(IdSon)
        IdDau <- df_Ngen$id[df_Ngen$ifdau == TRUE & df_Ngen$gen == i]
        # message(IdDau)
        IdOfp <- evenInsert(IdSon, IdDau)

        # generate link kids to the couples
        random_numbers <- adjustKidsPerCouple(
          nMates = sum(df_Ngen$ifparent) / 2, kpc = kpc,
          rd_kpc = rd_kpc
        )

        # cat("final random numbers",random_numbers, "\n")
        # cat("mean",sum(random_numbers)/length(random_numbers), "\n")
        # create two vectors for maId and paId; replicate the ids to match the same length as IdOfp
        IdMa <- numeric()
        IdPa <- numeric()
        usedIds <- numeric()
        idx <- 1

        for (l in 1:sizeI) {
          # check if the id is used
          if (!df_Ngen$id[l] %in% usedIds) {
            # check if the member can be a parent
            if (df_Ngen$ifparent[l] == TRUE && df_Ngen$sex[l] == code_female) {
              usedIds <- c(usedIds, df_Ngen$id[l], df_Ngen$spID[l])
              IdMa <- c(IdMa, rep(df_Ngen$id[l], random_numbers[idx]))
              IdPa <- c(IdPa, rep(df_Ngen$spID[l], random_numbers[idx]))
              idx <- idx + 1
            } else if (df_Ngen$ifparent[l] == TRUE && df_Ngen$sex[l] == code_male) {
              usedIds <- c(usedIds, df_Ngen$id[l], df_Ngen$spID[l])
              IdPa <- c(IdPa, rep(df_Ngen$id[l], random_numbers[idx]))
              IdMa <- c(IdMa, rep(df_Ngen$spID[l], random_numbers[idx]))
              idx <- idx + 1
            } else {
              next
            }
          } else {
            next
          }
        }

        # the length of IdMa and IdPa can be longer than the vector of offspring, so truncated it
        ### making sure sampling out the single people instead of couples
        if (length(IdPa) - length(IdOfp) > 0) {
          if (verbose == TRUE) {
            message("length of IdPa", length(IdPa), "\n")
          }
          IdRm <- sample.int(length(IdPa), size = length(IdPa) - length(IdOfp))
          IdPa <- IdPa[-IdRm]
          IdMa <- IdMa[-IdRm]
        } else if (length(IdPa) - length(IdOfp) < 0) {
          # cat("length of IdOfp", length(IdOfp), "\n")
          # cat("length of IdPa", length(IdPa), "\n")
          # cat("length of IdSingle", length(IdMa), "\n")
          IdRm <- resample(IdSingle, size = length(IdOfp) - length(IdPa))

          IdOfp <- IdOfp[!(IdOfp %in% IdRm)]
        }
        # if (length(IdMa)- length(IdOfp) > 0){
        #       IdRm <- sample.int(length(IdMa),size =length(IdMa)-length(IdOfp))
        #       IdPa <- IdPa[-IdRm]
        #       IdMa <- IdMa[-IdRm]
        # }else if (length(IdMa)-length(IdOfp) < 0) {
        #       IdRm <- sample.int(length(IdOfp),size =length(IdOfp)-length(IdMa))
        #       IdOfp <- IdOfp[-IdRm]
        # }
        # message(matrix(c(IdPa, IdMa), ncol = 2))

        # message(IdPa)
        # message(IdOfp)

        # put the IdMa and IdPa into the dfFam with correspondent OfpId
        for (m in seq_along(IdOfp)) {
          df_Ngen[df_Ngen$id == IdOfp[m], "pat"] <- IdPa[m]
          df_Ngen[df_Ngen$id == IdOfp[m], "mat"] <- IdMa[m]
        }
        # message(df_Ngen)
        df_Fam[df_Fam$gen == i, ] <- df_Ngen[df_Ngen$gen == i, ]
        df_Fam[df_Fam$gen == i - 1, ] <- df_Ngen[df_Ngen$gen == i - 1, ]
      }
    }
  }
  return(df_Fam)
}


#' Simulate Pedigrees
#' This function simulates "balanced" pedigrees based on a group of parameters:
#' 1) k - Kids per couple;
#' 2) G - Number of generations;
#' 3) p - Proportion of males in offspring;
#' 4) r - Mating rate.
#'
#' @importFrom stats runif
#' @param kpc Number of kids per couple. An integer >= 2 that determines how
#' many kids each fertilized mated couple will have in the pedigree. Default
#' value is 3. Returns an error when kpc equals 1.
#' @param Ngen Number of generations. An integer >= 2 that determines how many
#' generations the simulated pedigree will have. The first generation is always
#' a fertilized couple. The last generation has no mated individuals.
#' @param sexR Sex ratio of offspring. A numeric value ranging from 0 to 1 that
#' determines the proportion of males in all offspring in this pedigree. For
#' instance, 0.4 means 40 percent of the offspring will be male.
#' @param marR Mating rate. A numeric value ranging from 0 to 1 which determines
#' the proportion of mated (fertilized) couples in the pedigree within each
#' generation. For instance, marR  = 0.5 suggests 50 percent of the offspring in
#'  a specific generation will be  mated and have their offspring.
#' @param rd_kpc logical. If TRUE, the number of kids per mate will be randomly
#' generated from a poisson distribution with mean kpc. If FALSE, the number of
#' kids per mate will be fixed at kpc.
#' @param balancedSex Not fully developed yet. Always \code{TRUE} in the
#' current version.
#' @param balancedMar Not fully developed yet. Always \code{TRUE} in the
#' current version.
#' @param verbose logical  If TRUE, message progress through stages of algorithm
#' @param code_male The value to use for males. Default is "M"
#' @param code_female The value to use for females. Default is "F"
#' @param ... Additional arguments to be passed to other functions.
#' @inheritParams ped2fam
#' @param spouseID The name of the column that will contain the spouse ID in the output data frame. Default is "spID".
#' @return A \code{data.frame} with each row representing a simulated individual. The columns are as follows:
#' \itemize{
#'   \item{fam: The family id of each simulated individual. It is 'fam1' in a single simulated pedigree.}
#'   \item{ID: The unique personal ID of each simulated individual. The first digit is the fam id; the fourth digit is the generation the individual is in; the following digits represent the order of the individual within his/her pedigree. For example, 100411 suggests this individual has a family id of 1, is in the 4th generation, and is the 11th individual in the 4th generation.}
#'   \item{gen: The generation the simulated individual is in.}
#'   \item{dadID: Personal ID of the individual's father.}
#'   \item{momID: Personal ID of the individual's mother.}
#'   \item{spID: Personal ID of the individual's mate.}
#'   \item{sex: Biological sex of the individual. F - female; M - male.}
#' }
#' @export

simulatePedigree <- function(kpc = 3,
                             Ngen = 4,
                             sexR = .5,
                             marR = 2 / 3,
                             rd_kpc = FALSE,
                             balancedSex = TRUE,
                             balancedMar = TRUE,
                             verbose = FALSE,
                             personID = "ID",
                             momID = "momID",
                             dadID = "dadID",
                             spouseID = "spouseID",
                             code_male = "M",
                             code_female = "F") {
  # SexRatio: ratio of male over female in the offspring setting; used in the between generation combinations
  # SexRatio <- sexR / (1 - sexR)

  # Calculate the expected family size in each generations
  sizeGens <- allGens(kpc = kpc, Ngen = Ngen, marR = marR)
  #  famSizeIndex <- 1:sum(sizeGens)
  if (verbose == TRUE) {
    message(
      "Step 1: Let's build the connection within each generation first"
    )
  }
  df_Fam <- buildWithinGenerations(
    sizeGens = sizeGens,
    Ngen = Ngen,
    sexR = sexR,
    marR = marR,
    verbose = verbose,
    personID = personID,
    momID = momID,
    dadID = dadID,
    code_male = code_male,
    code_female = code_female
  )
  if (verbose == TRUE) {
    message(
      "Step 2: Let's try to build connection between each two generations"
    )
  }

  df_Fam <- buildBetweenGenerations(
    df_Fam = df_Fam,
    Ngen = Ngen,
    sizeGens = sizeGens,
    verbose = verbose,
    marR = marR,
    sexR = sexR,
    kpc = kpc,
    rd_kpc = rd_kpc,
    personID = personID,
    momID = momID,
    dadID = dadID,
    code_male = code_male,
    code_female = code_female
  )

  df_Fam <- df_Fam[, 1:7]
  df_Fam <- df_Fam[!(is.na(df_Fam$pat) & is.na(df_Fam$mat) & is.na(df_Fam$spID)), ]

  colnames(df_Fam)[c(2, 4, 5)] <- c(personID, dadID, momID)

  # connect the detached members
  df_Fam[is.na(df_Fam[[momID]]) & is.na(df_Fam[[dadID]]) & df_Fam$gen > 1, ]
  # if the sex rate is .5, make there is a 50% chance to change male to female and female to male
  # doesn't seem to produce the expected results, sometimes leads to moms being classified as dads
  #  if (sexR == .5 & runif(1) > .5) {
  #   df_Fam$sex[df_Fam$sex == "M"] <- "F1"
  #    df_Fam$sex[df_Fam$sex == "F"] <- "M"
  #    df_Fam$sex[df_Fam$sex == "F1"] <- "F"
  #  }
  # message(df_Fam)
  return(df_Fam)
}

#' @rdname simulatePedigree
#' @export
SimPed <- function(...) { # nolint: object_name_linter.
  warning("The 'SimPed' function is deprecated. Please use 'simulatePedigree' instead.")
  simulatePedigree(...)
}
