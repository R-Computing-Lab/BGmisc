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
                                    code_male = "M", code_female = "F", beta = FALSE) {
  if (beta == TRUE || beta %in% c("optimized", "index", "indexed")) {
    df_Fam <- buildBetweenGenerations_optimized(
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
  } else if (beta == FALSE || beta %in% c("base", "original") || is.null(beta)) {
    df_Fam <- buildBetweenGenerations_base(
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
  } else {
    stop("Invalid value for beta parameter. Use TRUE/'optimized' or FALSE/'base'.")
  }
  return(df_Fam)
}


buildBetweenGenerations_optimized <- function(df_Fam,
                                              Ngen,
                                              sizeGens,
                                              verbose = FALSE,
                                              marR, sexR, kpc,
                                              rd_kpc, personID = "ID",
                                              momID = "momID",
                                              dadID = "dadID",
                                              code_male = "M",
                                              code_female = "F") {
  # Initialize flags for the full pedigree data frame.
  # These are used throughout linkage and get overwritten per-generation as needed.

  df_Fam$ifparent <- FALSE
  df_Fam$ifson <- FALSE
  df_Fam$ifdau <- FALSE

  # Precompute row indices per generation once.
  # This avoids repeated df_Fam$gen == i scans inside loops.
  gen_rows <- split(seq_len(nrow(df_Fam)), df_Fam$gen)

  # Loop across generations 1..Ngen.

  for (i in seq_len(Ngen)) {
    # -------------------------------------------------------------------------
    # Generation 1: base case
    # Generation 1 individuals are founders and are treated as "parents" by design.
    # They do not have assigned mother/father, so we just set flags and continue.
    # -------------------------------------------------------------------------

    if (i == 1) {
      rows_i <- gen_rows[[as.character(i)]]
      df_Ngen <- df_Fam[rows_i, , drop = FALSE]

      # Mark everyone in generation 1 as parents (founder couple logic occurs earlier).
      df_Ngen$ifparent <- TRUE
      df_Ngen$ifson <- FALSE
      df_Ngen$ifdau <- FALSE
      df_Fam[rows_i, ] <- df_Ngen
      # Write back into the main df_Fam.
    } else {
      # calculate the number of couples in the i-1 th generation
      rows_i <- gen_rows[[as.character(i)]]
      rows_prev <- gen_rows[[as.character(i - 1)]]

      # -------------------------------------------------------------------------
      # Step A: Determine how many couples exist in generation i-1
      #
      # In your representation, each coupled individual has a non-NA spID, and each couple
      # appears twice (one row per spouse). Therefore:
      #   number_of_couples = (number_of_non_single_individuals) / 2
      # where number_of_non_single_individuals = sizeGens[i-1] - count(NA spID)
      # -------------------------------------------------------------------------

      N_couples <- (sizeGens[i - 1] - sum(is.na(df_Fam$spID[rows_prev]))) * 0.5

      # Expected number of offspring linked to those couples (before sex split).

      N_LinkedMem <- N_couples * kpc
      # Split linked offspring into female and male counts using sexR,
      # where sexR is the proportion male, so (1 - sexR) is the proportion female.

      N_LinkedFemale <- round(N_LinkedMem * (1 - sexR))
      N_LinkedMale <- N_LinkedMem - N_LinkedFemale


      # -------------------------------------------------------------------------
      # Step B: Prepare generation i data, assign couple IDs, and mark potential children
      # -------------------------------------------------------------------------

      # get the df for the i the generation
      df_Ngen <- df_Fam[rows_i, , drop = FALSE]


      # Reset per-generation fields that will be recomputed.
      df_Ngen$ifparent <- FALSE
      df_Ngen$ifson <- FALSE
      df_Ngen$ifdau <- FALSE
      df_Ngen$coupleId <- NA_character_

      # Randomly permute generation i rows so selection is not tied to row order.
      df_Ngen <- df_Ngen[sample(nrow(df_Ngen)), , drop = FALSE]

      # Start to connect children with mother and father

      if (verbose == TRUE) {
        message(
          "Step 2.1: mark a group of potential sons and daughters in the i th generation"
        )
      }


      # count the number of couples in the i th gen
      countCouple <- (nrow(df_Ngen) - sum(is.na(df_Ngen$spID))) * .5

      # Assign couple IDs within generation i.
      df_Ngen <- assignCoupleIds(df_Ngen)

      # Identify singles in generation i (no spouse).

      IdSingle <- df_Ngen$id[is.na(df_Ngen$spID)]

      # Count singles by sex; these affect how many "linked" children can come from couples.
      SingleF <- sum(df_Ngen$sex == code_female & is.na(df_Ngen$spID))
      SingleM <- sum(df_Ngen$sex == code_male & is.na(df_Ngen$spID))

      # Number of linked females that must come from couples after excluding single females.
      # This value is passed into markPotentialChildren, which decides who becomes ifson/ifdau.

      CoupleF <- N_LinkedFemale - SingleF

      # Mark potential sons and daughters within generation i.
      # This writes ifson/ifdau into the returned data frame
      df_Fam[rows_i, ] <- markPotentialChildren(
        df_Ngen = df_Ngen,
        i = i,
        Ngen = Ngen,
        sizeGens = sizeGens,
        CoupleF = CoupleF,
        code_male = code_male,
        code_female = code_female
      )

      # -------------------------------------------------------------------------
      # Step C: Mark a subset of generation i-1 couples as parents (ifparent)
      #
      # Goal: choose enough married couples (based on marR) to be parents.
      # We walk through a randomized order of generation i-1, and whenever we select
      # an individual who has a spouse, we mark both spouses as ifparent.
      # -------------------------------------------------------------------------

      if (verbose == TRUE) {
        message(
          "Step 2.2: mark a group of potential parents in the i-1 th generation"
        )
      }
      df_Ngen <- df_Fam[rows_prev, , drop = FALSE]

      # Reset flags within i-1 before reselecting parent couples.
      df_Ngen$ifparent <- FALSE
      df_Ngen$ifson <- FALSE
      df_Ngen$ifdau <- FALSE

      # Randomize order so parent selection is not tied to row ordering.
      df_Ngen <- df_Ngen[sample(nrow(df_Ngen)), , drop = FALSE]


      # Boolean vector that tracks which rows in df_prev are selected as parents.
      # Start all FALSE.
      isUsedParent <- df_Ngen$ifparent

      # Loop over up to sizeGens[i-1] positions.
      # Stop early once the parent selection proportion reaches marR.
      nrow_df_Ngen <- nrow(df_Ngen)

      for (k in seq_len(sizeGens[i - 1])) {
        # Proportion of individuals currently marked as parents in df_prev.
        # Since we always mark spouses together, this moves in steps of 2.
        if (sum(isUsedParent) / nrow_df_Ngen >= marR) {
          df_Ngen$ifparent <- isUsedParent
          break
        } else {
          # Only select someone as a parent if:
          # 1) they are not already used as a parent, and
          # 2) they have a spouse (spID not NA), because singles cannot form a parent couple.


          if (!(isUsedParent[k]) && !is.na(df_Ngen$spID[k])) {        # Mark this individual as parent.

            isUsedParent[k] <- TRUE
            # Mark their spouse row as parent too.
            # This works because spouse IDs are unique within a generation in this simulation.
            isUsedParent[df_Ngen$spID == df_Ngen$id[k]] <- TRUE
          } else {
            next
          }
        }
      }

      df_Ngen$ifparent <- isUsedParent

      # Restore original row order for df_prev before writing back into df_Fam.

      df_Ngen <- df_Ngen[order(as.numeric(rownames(df_Ngen))), , drop = FALSE]

      df_Fam[rows_prev, ] <- df_Ngen

      if (verbose == TRUE) {
        message(
          "Step 2.3: connect the i and i-1 th generation"
        )
      }


      if (i == 1) {
        next
      } else {
        # Pull the two generations together.
        df_Ngen <- df_Fam[df_Fam$gen %in% c(i, i - 1), , drop = FALSE]

        sizeI <- sizeGens[i - 1]
        sizeII <- sizeGens[i]

        # Collect IDs of marked sons and daughters in generation i.
        IdSon <- df_Ngen$id[df_Ngen$ifson == TRUE & df_Ngen$gen == i]
        IdDau <- df_Ngen$id[df_Ngen$ifdau == TRUE & df_Ngen$gen == i]

        # Interleave sons and daughters to get an offspring list.
        IdOfp <- evenInsert(IdSon, IdDau)

        # nMates is number of parent couples selected (ifparent rows are individuals).
        nMates <- sum(df_Ngen$ifparent) / 2

        # If no mates or no offspring were selected for linkage, skip linkage.
        if (nMates <= 0 || length(IdOfp) == 0) {
          df_Fam[rows_i, ] <- df_Ngen[df_Ngen$gen == i, ]
          df_Fam[rows_prev, ] <- df_Ngen[df_Ngen$gen == i - 1, ]
          next
        }

        # generate link kids to the couples
        random_numbers <- adjustKidsPerCouple(
          nMates = sum(df_Ngen$ifparent) / 2, kpc = kpc,
          rd_kpc = rd_kpc
        )

        # Guard: adjustKidsPerCouple returned nothing usable
        if (length(random_numbers) == 0 || all(is.na(random_numbers))) {
          df_Fam[rows_i, ] <- df_Ngen[df_Ngen$gen == i, ]
          df_Fam[rows_prev, ] <- df_Ngen[df_Ngen$gen == i - 1, ]
          next
        }

        # -------------------------------------------------------------------------
        # Step E: Build parent assignment vectors IdMa and IdPa
        #
        # The goal is to expand couples into per-child vectors of mother IDs and father IDs,
        # where each couple contributes random_numbers[couple_index] children.
        #
        # Important: df_Ngen contains both generations. We only want parent generation rows.
        # -------------------------------------------------------------------------

        # Identify rows in df_Ngen that belong to generation i-1 (parent generation).
        rows_prev_in_pair <- which(df_Ngen$gen == (i - 1))

        # Extract parent generation into a smaller frame to make operations faster and clearer.
        prev <- df_Ngen[rows_prev_in_pair, , drop = FALSE]

        # Keep only those rows that are marked ifparent and are actually paired (non-NA spID).
        parent_rows <- which(prev$ifparent == TRUE & !is.na(prev$spID))

        # If no usable parent couples remain, skip linkage.
        if (length(parent_rows) == 0) {
          df_Fam[rows_i, ] <- df_Ngen[df_Ngen$gen == i, ]
          df_Fam[rows_prev, ] <- df_Ngen[df_Ngen$gen == i - 1, ]
          next
        }
        # Create a symmetric couple key so we can keep only one row per couple.
        a <- pmin(prev$id, prev$spID)
        b <- pmax(prev$id, prev$spID)
        couple_key <- paste(a, b, sep = "_")

        # Keep only the first row for each couple among the parent rows.
        parent_rows <- parent_rows[!duplicated(couple_key[parent_rows])]

        # Determine whether each kept row corresponds to the female member of the couple.
        # If the kept row is female: mother = id, father = spID
        # If the kept row is male:   father = id, mother = spID
        is_female_row <- prev$sex[parent_rows] == code_female
        # One mother ID per couple.
        ma_ids <- ifelse(is_female_row, prev$id[parent_rows], prev$spID[parent_rows])

        # One father ID per couple.
        pa_ids <- ifelse(is_female_row, prev$spID[parent_rows], prev$id[parent_rows])

        # Align lengths between couples and random_numbers.
        # If random_numbers is longer than couples, truncate random_numbers.
        # If random_numbers is shorter than couples, drop extra couples.
        nCouples <- length(parent_rows)

        if (length(random_numbers) > nCouples) {
          random_numbers <- random_numbers[seq_len(nCouples)]
        } else if (length(random_numbers) < nCouples) {
          keep <- seq_len(length(random_numbers))
          ma_ids <- ma_ids[keep]
          pa_ids <- pa_ids[keep]
        }

        # Expand from "one mother/father per couple" to "one mother/father per child".
        # rep.int is used to avoid extra overhead.
        IdMa <- rep.int(ma_ids, times = random_numbers)
        IdPa <- rep.int(pa_ids, times = random_numbers)

        # -------------------------------------------------------------------------
        # Step F: Ensure IdMa/IdPa length matches the number of offspring IdOfp
        #
        # Two mismatch cases:
        # 1) Too many parent slots relative to offspring: drop excess parent slots.
        # 2) Too many offspring relative to parent slots: drop some offspring.
        #
        # drop singles first (IdSingle) when reducing offspring.
        # -------------------------------------------------------------------------



        if (length(IdPa) - length(IdOfp) > 0) {
          if (verbose == TRUE) {
            message("length of IdPa", length(IdPa), "\n")
          }
          # Excess parent slots: randomly remove that many entries from IdPa and IdMa.

          excess <- length(IdPa) - length(IdOfp)
          if (length(IdPa) > 0 && excess > 0) {
            IdRm <- sample.int(length(IdPa), size = excess)
            IdPa <- IdPa[-IdRm]
            IdMa <- IdMa[-IdRm]
          }
        } else if (length(IdPa) - length(IdOfp) < 0) {
          if (verbose == TRUE) {
            message("length of IdOfp", length(IdOfp), "\n")
            message("length of IdPa", length(IdPa), "\n")
            message("length of IdSingle", length(IdMa), "\n")
          }


          # harden the resample call when IdSingle is empty:
          # Need to drop some offspring because we do not have enough parent slots.
          need_drop <- length(IdOfp) - length(IdPa)

          if (need_drop > 0) {
            if (length(IdSingle) > 0) {
              # Preferentially remove offspring IDs that correspond to singles.
              # resample is expected to return a vector of IDs to remove.

              IdRm <- resample(IdSingle, size = need_drop)
              IdOfp <- IdOfp[!(IdOfp %in% IdRm)]
            } else {
              # If there are no singles to target, drop arbitrary offspring indices.
              drop_idx <- sample.int(length(IdOfp), size = need_drop)
              IdOfp <- IdOfp[-drop_idx]
            }
          }
        }

        # -------------------------------------------------------------------------
        # Step G: Assign pat/mat into df_Ngen for the selected offspring.
        #
        # Replaces the old loop:
        #   for (m in seq_along(IdOfp)) df_Ngen[df_Ngen$id == IdOfp[m], "pat"] <- ...
        # Using match avoids repeated scanning over df_Ngen$id.
        # -------------------------------------------------------------------------

        # Find row positions in df_Ngen corresponding to offspring IDs.
        child_rows <- match(IdOfp, df_Ngen$id)
        # Only keep rows that matched successfully.

        ok <- !is.na(child_rows)

        if (any(ok)) {
          # Assign father IDs and mother IDs to offspring rows.

          df_Ngen$pat[child_rows[ok]] <- IdPa[ok]
          df_Ngen$mat[child_rows[ok]] <- IdMa[ok]
        }
        # -------------------------------------------------------------------------
        # Step H: Write the two generations back into df_Fam using the precomputed indices.
        # -------------------------------------------------------------------------

        df_Fam[rows_i, ] <- df_Ngen[df_Ngen$gen == i, ]
        df_Fam[rows_prev, ] <- df_Ngen[df_Ngen$gen == i - 1, ]
      }
    }
  }
  return(df_Fam)
}

buildBetweenGenerations_base <- function(df_Fam,
                                         Ngen, sizeGens,
                                         verbose = FALSE, marR,
                                         sexR, kpc,
                                         rd_kpc, personID = "ID",
                                         momID = "momID",
                                         dadID = "dadID",
                                         code_male = "M", code_female = "F") {
  # create an index for each generation to speed up the process
  df_Fam$ifparent <- FALSE
  df_Fam$ifson <- FALSE
  df_Fam$ifdau <- FALSE
  gen_rows <- split(seq_len(nrow(df_Fam)), df_Fam$gen)

  for (i in seq_len(Ngen)) {
    # generation 1 doesn't need any mother and father
    if (i == 1) {
      rows_i <- gen_rows[[as.character(i)]]
      df_Ngen <- df_Fam[rows_i, ]
      df_Ngen$ifparent <- TRUE
      df_Ngen$ifson <- FALSE
      df_Ngen$ifdau <- FALSE
      df_Fam[rows_i, ] <- df_Ngen
    } else {
      # calculate the number of couples in the i-1 th generation
      rows_i <- gen_rows[[as.character(i)]]
      rows_prev <- gen_rows[[as.character(i - 1)]]

      # calculate the number of couples in the i-1 th generation
      N_couples <- (sizeGens[i - 1] - sum(is.na(df_Fam$spID[rows_prev]))) * 0.5

      # calculate the number of members in the i th generation that have a link to the couples in the i-1 th generation
      N_LinkedMem <- N_couples * kpc
      # decompose the linked members into females and males respectively
      N_LinkedFemale <- round(N_LinkedMem * (1 - sexR))
      N_LinkedMale <- N_LinkedMem - N_LinkedFemale


      # get the df for the i the generation
      df_Ngen <- df_Fam[rows_i, ]
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


      df_Fam[rows_i, ] <- markPotentialChildren(
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
      df_Ngen <- df_Fam[rows_prev, ]

      df_Ngen$ifparent <- FALSE
      df_Ngen$ifson <- FALSE
      df_Ngen$ifdau <- FALSE

      df_Ngen <- df_Ngen[sample(nrow(df_Ngen)), ]
      # Create a pool for the used parents
      usedParentIds <- numeric()

      nrow_df_Ngen <- nrow(df_Ngen)
      # assign parents until reaching the marriage rate, or finishing the list
      # good place to optimize

      isUsedParent <- df_Ngen$ifparent
      isUsedParent <- df_Ngen$id %in% usedParentIds | isUsedParent

      for (k in seq_len(sizeGens[i - 1])) {
        # first check if the number of married couples surpass the marriage rate
        if (sum(isUsedParent) / nrow_df_Ngen >= marR) {
          df_Ngen$ifparent <- isUsedParent
          usedParentIds <- unique(c(usedParentIds, df_Ngen$id[isUsedParent]))
          break
        } else {
          # check if the id is used and if the member has married

          if (!(isUsedParent[k]) && !is.na(df_Ngen$spID[k])) {
            isUsedParent[k] <- TRUE
            isUsedParent[df_Ngen$spID == df_Ngen$id[k]] <- TRUE
          } else {
            next
          }
        }
      }
      df_Ngen$ifparent <- isUsedParent
      df_Ngen <- df_Ngen[order(as.numeric(rownames(df_Ngen))), , drop = FALSE]

      df_Fam[rows_prev, ] <- df_Ngen

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

        nMates <- sum(df_Ngen$ifparent) / 2

        # If no mates or no offspring to link, skip linkage for this generation
        if (nMates <= 0 || length(IdOfp) == 0) {
          df_Fam[rows_i, ] <- df_Ngen[df_Ngen$gen == i, ]
          df_Fam[rows_prev, ] <- df_Ngen[df_Ngen$gen == i - 1, ]
          next
        }

        # generate link kids to the couples
        random_numbers <- adjustKidsPerCouple(
          nMates = sum(df_Ngen$ifparent) / 2, kpc = kpc,
          rd_kpc = rd_kpc
        )

        # Guard: adjustKidsPerCouple returned nothing usable
        if (length(random_numbers) == 0 || all(is.na(random_numbers))) {
          df_Fam[rows_i, ] <- df_Ngen[df_Ngen$gen == i, ]
          df_Fam[rows_prev, ] <- df_Ngen[df_Ngen$gen == i - 1, ]
          next
        }

        IdMa <- numeric()
        IdPa <- numeric()
        usedIds <- numeric()
        idx <- 1

        isUsedParent <- df_Ngen$id %in% usedIds

        # can be optimized further
        for (l in seq_len(sizeI)) {
          # check if the id is used
          if (!df_Ngen$id[l] %in% usedIds) {
            # check if the member can be a parent
            if (df_Ngen$ifparent[l] == TRUE && df_Ngen$sex[l] == code_female) {
              if (idx > length(random_numbers) || is.na(random_numbers[idx]) || random_numbers[idx] < 0) {
                break
              }

              usedIds <- c(usedIds, df_Ngen$id[l], df_Ngen$spID[l])
              IdMa <- c(IdMa, rep(df_Ngen$id[l], random_numbers[idx]))
              IdPa <- c(IdPa, rep(df_Ngen$spID[l], random_numbers[idx]))
              idx <- idx + 1
            } else if (df_Ngen$ifparent[l] == TRUE && df_Ngen$sex[l] == code_male) {
              if (idx > length(random_numbers) || is.na(random_numbers[idx]) || random_numbers[idx] < 0) {
                break
              }

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

          excess <- length(IdPa) - length(IdOfp)
          if (length(IdPa) > 0 && excess > 0) {
            IdRm <- sample.int(length(IdPa), size = excess)
            IdPa <- IdPa[-IdRm]
            IdMa <- IdMa[-IdRm]
          }
        } else if (length(IdPa) - length(IdOfp) < 0) {
          if (verbose == TRUE) {
            message("length of IdOfp", length(IdOfp), "\n")
            message("length of IdPa", length(IdPa), "\n")
            message("length of IdSingle", length(IdMa), "\n")
          }


          # harden the resample call when IdSingle is empty:
          need_drop <- length(IdOfp) - length(IdPa)

          if (need_drop > 0) {
            if (length(IdSingle) > 0) {
              IdRm <- resample(IdSingle, size = need_drop)
              IdOfp <- IdOfp[!(IdOfp %in% IdRm)]
            } else {
              # no singles available to drop; drop offspring indices directly
              drop_idx <- sample.int(length(IdOfp), size = need_drop)
              IdOfp <- IdOfp[-drop_idx]
            }
          }
        }

        for (m in seq_along(IdOfp)) {
          df_Ngen[df_Ngen$id == IdOfp[m], "pat"] <- IdPa[m]
          df_Ngen[df_Ngen$id == IdOfp[m], "mat"] <- IdMa[m]
        }
        # message(df_Ngen)
        df_Fam[rows_i, ] <- df_Ngen[df_Ngen$gen == i, ]
        df_Fam[rows_prev, ] <- df_Ngen[df_Ngen$gen == i - 1, ]
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
#' @param fam_shift An integer to shift the person ID. Default is 1L.
#' This is useful when simulating multiple pedigrees to avoid ID conflicts.
#' @param beta logical. If TRUE, use the optimized version of the algorithm.
#' @param ... Additional arguments to be passed to other functions.
#' @inheritParams ped2fam
#' @param spouseID The name of the column that will contain the spouse ID in the output data frame. Default is "spID".
#' @return A \code{data.frame} with each row representing a simulated individual. The columns are as follows:
#' \itemize{
#'   \item{fam: The family id of each simulated individual. It is 'fam1' in a single simulated pedigree.}
#'   \item{ID: The unique personal ID of each simulated individual. The first digit is the fam id; the fourth digit is the generation the individual is in; the following digits represent the order of the individual within their  pedigree. For example, 100411 suggests this individual has a family id of 1, is in the 4th generation, and is the 11th individual in the 4th generation.}
#'   \item{gen: The generation the simulated individual is in.}
#'   \item{dadID: Personal ID of the individual's father.}
#'   \item{momID: Personal ID of the individual's mother.}
#'   \item{spID: Personal ID of the individual's mate.}
#'   \item{sex: Biological sex of the individual. F - female; M - male.}
#' }
#' @export
#' @examples
#' set.seed(5)
#' df_ped <- simulatePedigree(
#'   kpc = 4,
#'   Ngen = 4,
#'   sexR = .5,
#'   marR = .7
#' )
#' summary(df_ped)
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
                             code_female = "F",
                             fam_shift = 1L,
                             beta = FALSE) {
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
    code_female = code_female,
    fam_shift = fam_shift,
    beta = beta
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
    code_female = code_female,
    beta = beta
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
