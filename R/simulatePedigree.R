#' Simulate Pedigrees
#' This function simulates "balanced" pedigrees based on a group of parameters:
#' 1) k - Kids per couple;
#' 2) G - Number of generations;
#' 3) p - Proportion of males in offspring;
#' 4) r - Mating rate.
#'
#' @importFrom stats runif
#' @param kpc Number of kids per couple. An integer >= 2 that determines how many kids each fertilized mated couple will have in the pedigree. Default value is 3. Returns an error when kpc equals 1.
#' @param Ngen Number of generations. An integer >= 2 that determines how many generations the simulated pedigree will have. The first generation is always a fertilized couple. The last generation has no mated individuals.
#' @param sexR Sex ratio of offspring. A numeric value ranging from 0 to 1 that determines the proportion of males in all offspring in this pedigree. For instance, 0.4 means 40 percent of the offspring will be male.
#' @param marR Mating rate. A numeric value ranging from 0 to 1 which determines the proportion of mated (fertilized) couples in the pedigree within each generation. For instance, marR = 0.5 suggests 50 percent of the offspring in a specific generation will be mated and have their offspring.
#' @param rd_kpc logical. If TRUE, the number of kids per mate will be randomly generated from a poisson distribution with mean kpc. If FALSE, the number of kids per mate will be fixed at kpc.
#' @param balancedSex Not fully developed yet. Always \code{TRUE} in the current version.
#' @param balancedMar Not fully developed yet. Always \code{TRUE} in the current version.
#' @param verbose logical  If TRUE, print progress through stages of algorithm

#' @return A \code{data.frame} with each row representing a simulated individual. The columns are as follows:
#' \itemize{
#'   \item{fam: The family id of each simulated individual. It is 'fam1' in a single simulated pedigree.}
#'   \item{ID: The unique personal ID of each simulated individual. The first digit is the fam id; the fourth digit is the generation the individual is in; the following digits represent the order of the individual within his/her pedigree. For example, 100411 suggests this individual has a family id of 1, is in the 4th generation, and is the 11th individual in the 4th generation.}
#'   \item{gen: The generation the simulated individual is in.}
#'   \item{dadID: Personal ID of the individual's father.}
#'   \item{momID: Personal ID of the individual's mother.}
#'   \item{spt: Personal ID of the individual's mate.}
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
                             verbose = FALSE) {
  # a supporting function 
  resample = function(x, ...){
    #print(length(x))
    if(length(x) == 0) return(NA_integer_)
    x[sample.int(length(x), ...)]
    } 
  # SexRatio: ratio of male over female in the offspring setting; used in the between generation combinations
  SexRatio <- sexR / (1 - sexR)

  # Calculate the expected family size in each generations
  sizeGens <- allGens(kpc = kpc, Ngen = Ngen, marR = marR)
  famSizeIndex <- 1:sum(sizeGens)
  if (verbose) {
    print(
      "Step 1: Let's build the connection within each generation first"
    )
  }
  for (i in 1:Ngen) {
    idGen <- as.numeric(paste(100, i, 1:sizeGens[i], sep = ""))
    # idGen <- ifelse(i==1,
    #                 paste(i,"-",1:sizeGens[i]),
    #                 paste(i,"-",sizeGens[i-1]:sizeGens[i]))
    ### For each generation, create a seperate dataframe
    df_Ngen <- data.frame(
      fam = rep(paste("fam", 1), sizeGens[i], sep = ""),
      id = idGen[1:sizeGens[i]],
      gen = rep(i, sizeGens[i]),
      pat = rep(NA, sizeGens[i]), # father id
      mat = rep(NA, sizeGens[i]), # mother id
      spt = rep(NA, sizeGens[i]), # spouse id
      sex = rep(NA, sizeGens[i])
    )
    ### Let's deal with the sex in each generation first

    if (runif(1) > .5) {
      sexVec1 <- rep(
        "g1",
        floor(length(idGen) * sexR)
      )
      sexVec2 <- rep(
        "g2",
        length(idGen) - length(sexVec1)
      )
      sexVec <- sample(c(sexVec1, sexVec2))

      sexVec[sexVec == "g1"] <- "M"
      sexVec[sexVec == "g2"] <- "F"
    } else {
      sexVec1 <- rep(
        "g1",
        floor(length(idGen) * (1 - sexR))
      )
      sexVec2 <- rep(
        "g2",
        length(idGen) - length(sexVec1)
      )
      sexVec <- sample(c(sexVec1, sexVec2))

      sexVec[sexVec == "g1"] <- "F"
      sexVec[sexVec == "g2"] <- "M"
    }
    df_Ngen$sex <- sexVec
    # print(paste("tiger",i))
    # The first generation
    if (i == 1) {
      df_Ngen$spt[1] <- df_Ngen$id[2]
      df_Ngen$spt[2] <- df_Ngen$id[1]

      df_Ngen$sex[1] <- "F"
      df_Ngen$sex[2] <- "M"
    }

    ## Connect male and female into couples in each generations
    marR_crt <- (1 + marR) / 2
    UsedIdFeMale <- numeric()
    UsedIdMale <- numeric()
    # reserve the single persons
    if (i != 1 & i != Ngen) {
      nMerriedFemale <- round(sum(df_Ngen$sex == "F") * marR_crt)
      nMerriedMale <- round(sum(df_Ngen$sex == "M") * marR_crt)
      # make sure there are same numbers of merried males and females
      if (nMerriedFemale >= nMerriedMale) {
        nMerriedFemale <- nMerriedMale
      } else {
        nMerriedMale <- nMerriedFemale
      }
      # get the number of single males and females
      nSingleFemale <- sum(df_Ngen$sex == "F") - nMerriedFemale
      nSingleMale <- sum(df_Ngen$sex == "M") - nMerriedMale


      # sample single ids from male ids and female ids
      UsedIdFemale <- sample(df_Ngen$id[df_Ngen$sex == "F"], nSingleFemale)
      ## print(c("Used F", UsedIdFemale))
      UsedIdMale <- sample(df_Ngen$id[df_Ngen$sex == "M"], nSingleMale)
      ## print(c("Used M", UsedIdMale))

      UsedId <- c(UsedIdFemale, UsedIdMale)

      # Create spouses
      for (j in 1:nrow(df_Ngen)) {
        if (df_Ngen$id[j] %in% UsedId) {
          next
        } else {
          # idx <- j+1
          if (df_Ngen$sex[j] == "F") {
            for (k in 1:nrow(df_Ngen)) {
              idr <- df_Ngen$id[k]
              tgt <- (!(idr %in% UsedId)) & df_Ngen$sex[k] == "M"
              # tgt <- ifelse(is.na(tgt),FALSE,TRUE)
              if (tgt) {
                df_Ngen$spt[j] <- df_Ngen$id[k]
                df_Ngen$spt[k] <- df_Ngen$id[j]
                UsedId <- c(UsedId, df_Ngen$id[j], df_Ngen$id[k])
                break
              } else {
                next
              }
            }
          } else {
            for (k in 1:nrow(df_Ngen)) {
              idr <- df_Ngen$id[k]
              tgt <- (!(idr %in% UsedId)) & df_Ngen$sex[k] == "F"
              # tgt <- ifelse(is.na(tgt),FALSE,TRUE)
              if (tgt) {
                df_Ngen$spt[j] <- df_Ngen$id[k]
                df_Ngen$spt[k] <- df_Ngen$id[j]
                UsedId <- c(UsedId, df_Ngen$id[j], df_Ngen$id[k])
                break
              } else {
                next
              }
            }
          }
        }
        # print(UsedId)
      }
    }
    if (i == 1) {
      df_Fam <- df_Ngen
    } else {
      df_Fam <- rbind(df_Fam, df_Ngen)
    }
  }

  if (verbose) {
    print(
      "Step 2: Let's try to build connection between each two generations"
    )
  }
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
      N_couples <- (sizeGens[i - 1] - sum(is.na(df_Fam$spt[df_Fam$gen == i - 1]))) * 0.5
      # calculate the number of members in the i th generation that have a link to the couples in the i-1 th generation
      N_LinkedMem <- N_couples * kpc
      # decompose the linked members into females and males respectively
      N_LinkedFemale <- round(N_LinkedMem * (1 - sexR))
      N_LinkedMale <- N_LinkedMem - N_LinkedFemale

      # Create a pool for used male children and female children respectively
      UsedIdFemale <- numeric()
      UsedIdMale <- numeric()
      UsedId <- c(UsedIdFemale, UsedIdMale)

      # get the df for the i the generation
      df_Ngen <- df_Fam[df_Fam$gen == i, ]
      df_Ngen$ifparent <- FALSE
      df_Ngen$ifson <- FALSE
      df_Ngen$ifdau <- FALSE
      df_Ngen$IdCouple <- as.character(NA)
      df_Ngen <- df_Ngen[sample(nrow(df_Ngen)), ]
      # Start to connect children with mother and father
      # Step 2.1: mark a group of potential sons and daughters in the i th generation

      # try to rewrite the code
      # count the number of couples in the i th gen
      countCouple <- (nrow(df_Ngen) - sum(is.na(df_Ngen$spt))) * .5

      # give each member a coupleId
      UsedCoupleId <- character()
      for (j in 1:sizeGens[i]) {
        if (df_Ngen$IdCouple[j] %in% UsedCoupleId) {
          next
        } else {
          if (is.na(df_Ngen$spt[j])) {
            df_Ngen$IdCouple[j] <- as.character(NA)
          } else {
            df_Ngen$IdCouple[j] <- paste(sort(c(df_Ngen$id[j], df_Ngen$spt[j]))[1],
              sort(c(df_Ngen$id[j], df_Ngen$spt[j]))[2],
              sep = "_"
            )
            UsedCoupleId <- c(UsedCoupleId, df_Ngen$IdCouple[j])
          }
        }
      }
      # get the number of linked female and male children after excluding the single children
      # get a vector of single person id in the ith generation
      IdSingle <- df_Ngen$id[is.na(df_Ngen$spt)]
      SingleF <- sum(df_Ngen$sex == "F" & is.na(df_Ngen$spt))
      CoupleF <- N_LinkedFemale - SingleF
      SingleM <- sum(df_Ngen$sex == "M" & is.na(df_Ngen$spt))
      CoupleM <- N_LinkedMale - SingleM
      # get all couple ids
      coupleID <- unique(df_Ngen$IdCouple[!is.na(df_Ngen$IdCouple)])
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
            # UsedId <- c(UsedId, df_Ngen$id[j])
          } else {
            df_Ngen$ifson[j] <- TRUE
            # UsedId <- c(UsedId, df_Ngen$id[j])
          }
        } else {
          if (df_Ngen$IdCouple[j] %in% coupleBoy & df_Ngen$sex[j] == "M") {
            df_Ngen$ifson[j] <- TRUE
          } else if (df_Ngen$IdCouple[j] %in% coupleGirl & df_Ngen$sex[j] == "F") {
            df_Ngen$ifdau[j] <- TRUE
          } else {
            next
          }
        }
      }

      df_Ngen <- df_Ngen[order(as.numeric(rownames(df_Ngen))), , drop = FALSE]
      df_Ngen <- df_Ngen[, -ncol(df_Ngen)]
      df_Fam[df_Fam$gen == i, ] <- df_Ngen
      if (verbose) {
        print(
          "Step 2.2: mark a group of potential parents in the i-1 th generation"
        )
      }
      df_Ngen <- df_Fam[df_Fam$gen == i - 1, ]
      df_Ngen$ifparent <- FALSE
      df_Ngen$ifson <- FALSE
      df_Ngen$ifdau <- FALSE
      df_Ngen <- df_Ngen[sample(nrow(df_Ngen)), ]
      # Create a pool for the used parents
      UsedIdParents <- numeric()

      for (k in 1:sizeGens[i - 1]) {
        # first check if the number of married couples surpass the marriage rate
        if (sum(df_Ngen$ifparent) / nrow(df_Ngen) >= marR) {
          break
        } else {
          # check if the id is used and if the member has married
          if (!(df_Ngen$id[k] %in% UsedIdParents) & !is.na(df_Ngen$spt[k])) {
            df_Ngen$ifparent[k] <- TRUE
            df_Ngen$ifparent[df_Ngen$spt == df_Ngen$id[k]] <- TRUE
            UsedIdParents <- c(UsedIdParents, df_Ngen$id[k], df_Ngen$spt[k])
          } else {
            next
          }
        }
      }

      df_Ngen <- df_Ngen[order(as.numeric(rownames(df_Ngen))), , drop = FALSE]
      df_Fam[df_Fam$gen == i - 1, ] <- df_Ngen
      if (verbose) {
        print(
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
        # print(df_Ngen)
        IdSon <- df_Ngen$id[df_Ngen$ifson == TRUE & df_Ngen$gen == i]
        # print(IdSon)
        IdDau <- df_Ngen$id[df_Ngen$ifdau == TRUE & df_Ngen$gen == i]
        # print(IdDau)
        IdOfp <- evenInsert(IdSon, IdDau)

        if (rd_kpc) {
          # get the number of pairs of mates in the i th generation
          nMates <- sum(df_Ngen$ifparent)/2
          #cat("number of mates",nMates, "\n")
          diff <- nMates + 1
          while(diff > nMates){
            random_numbers = rpois(nMates, kpc)
            #cat("original random numbers", random_numbers, "\n")
            diff = abs(nMates*kpc - sum(random_numbers))
          }

          #diff = abs(length(IdOfp) - sum(random_numbers))
          # make sure the sum of kids per couple is equal to the number of kids in the i th generation
          if (sum(random_numbers) < nMates*kpc){
            names(random_numbers) <- seq(length(random_numbers))
            random_numbers = sort(random_numbers)
            random_numbers[1:diff]= random_numbers[1:diff] + 1
            random_numbers = random_numbers[order(names(random_numbers))]
          } else if (sum(random_numbers) > nMates*kpc) {
            names(random_numbers) <- seq(length(random_numbers))
            random_numbers = sort(random_numbers, decreasing = TRUE)
            random_numbers[1:diff]= random_numbers[1:diff] - 1
            random_numbers = random_numbers[order(names(random_numbers))]
          }
        } else {
           random_numbers <- rep(kpc, sum(df_Ngen$ifparent)/2)
        }
        if (min(random_numbers) < 0) {
          random_numbers[random_numbers == -1] <- 0
          random_numbers[random_numbers == max(random_numbers)] <- max(random_numbers) - 1
        }

        #cat("final random numbers",random_numbers, "\n")
        #cat("mean",sum(random_numbers)/length(random_numbers), "\n")
        # create two vectors for maId and paId; replicate the ids to match the same length as IdOfp
        IdMa <- numeric()
        IdPa <- numeric()
        UsedId <- numeric()
        idx = 1

        for (l in 1:sizeI) {
          # check if the id is used
          if (!df_Ngen$id[l] %in% UsedId) {
            # check if the member can be a parent
            if (df_Ngen$ifparent[l] == TRUE & df_Ngen$sex[l] == "F") {
              UsedId <- c(UsedId, df_Ngen$id[l], df_Ngen$spt[l])
              IdMa <- c(IdMa, rep(df_Ngen$id[l], random_numbers[idx]))
              IdPa <- c(IdPa, rep(df_Ngen$spt[l], random_numbers[idx]))
              idx = idx + 1
            } else if (df_Ngen$ifparent[l] == TRUE & df_Ngen$sex[l] == "M") {
              UsedId <- c(UsedId, df_Ngen$id[l], df_Ngen$spt[l])
              IdPa <- c(IdPa, rep(df_Ngen$id[l], random_numbers[idx]))
              IdMa <- c(IdMa, rep(df_Ngen$spt[l], random_numbers[idx]))
              idx = idx + 1
            } else {
              next
            }
          } else {
            next
          }
        }

        # the length of IdMa and IdPa can be longer than the vector of offspring, so truncated it
        ### making sure sampling out the single people instead of couples
        if (length(IdPa) - length(IdOfp) > 0 ) {
          #cat("length of IdPa", length(IdPa), "\n")
          IdRm <- sample.int(length(IdPa), size = length(IdPa) - length(IdOfp))
          IdPa <- IdPa[-IdRm]
          IdMa <- IdMa[-IdRm]
        } else if (length(IdPa) - length(IdOfp) < 0) {
          #cat("length of IdOfp", length(IdOfp), "\n")
          #cat("length of IdPa", length(IdPa), "\n")
          #cat("length of IdSingle", length(IdMa), "\n")
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
        # print(matrix(c(IdPa, IdMa), ncol = 2))

        # print(IdPa)
        # print(IdOfp)
        
        # put the IdMa and IdPa into the dfFam with correspondent OfpId
        for (m in 1:length(IdOfp)) {
          df_Ngen[df_Ngen$id == IdOfp[m], "pat"] <- IdPa[m]
          df_Ngen[df_Ngen$id == IdOfp[m], "mat"] <- IdMa[m]
        }
        # print(df_Ngen)
        df_Fam[df_Fam$gen == i, ] <- df_Ngen[df_Ngen$gen == i, ]
        df_Fam[df_Fam$gen == i - 1, ] <- df_Ngen[df_Ngen$gen == i - 1, ]
      }
    }
  }



  df_Fam <- df_Fam[, 1:7]
  df_Fam <- df_Fam[!(is.na(df_Fam$pat) & is.na(df_Fam$mat) & is.na(df_Fam$spt)), ]
  colnames(df_Fam)[c(2, 4, 5)] <- c("ID", "dadID", "momID")

  # connect the detached members
  df_Fam[is.na(df_Fam$momID) & is.na(df_Fam$dadID) & df_Fam$gen > 1, ]
  # if the sex rate is .5, make there is a 50% chance to change male to female and female to male
  if (sexR == .5 & runif(1) > .5) {
    df_Fam$sex[df_Fam$sex == "M"] <- "F1"
    df_Fam$sex[df_Fam$sex == "F"] <- "M"
    df_Fam$sex[df_Fam$sex == "F1"] <- "F"
  }
  # print(df_Fam)
  return(df_Fam)
}
