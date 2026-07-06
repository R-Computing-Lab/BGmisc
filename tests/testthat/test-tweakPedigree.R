# Test for makeTwins function
test_that("makeTwins - Twins specified by IDs", {
  set.seed(1234)
  ped <- data.frame(
    famID = c(1, 1, 2, 2),
    ID = c(1, 2, 3, 4),
    gen = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID = c(NA, NA, NA, NA),
    sex = c("M", "F", "M", "F")
  )
  expected_result <- data.frame(
    famID = c(1, 1, 2, 2),
    ID = c(1, 2, 3, 4),
    gen = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID = c(NA, NA, NA, NA),
    sex = c("M", "F", "M", "F"),
    twinID = c(2, 1, NA, NA),
    zygosity = c("MZ", "MZ", NA, NA)
  )
  result <- makeTwins(ped, ID_twin1 = 1, ID_twin2 = 2)

  expect_equal(result, expected_result)

  # does it handle weird variable names? "fam" = "famID"

  names(ped)[1] <- "fam"

  result_badfam <- makeTwins(ped, ID_twin1 = 1, ID_twin2 = 2, verbose = TRUE)

  expect_equal(result_badfam, expected_result)


  result2 <- makeTwins(ped,
    ID_twin1 = 1,
    ID_twin2 = 2,
    verbose = TRUE, zygosity = "DZ"
  )

  expected_result$zygosity[expected_result$zygosity == "MZ"] <- "DZ"
  expect_equal(result2, expected_result)
  # hp <- makeTwins(potter, ID_twin1 = 12, ID_twin2 = 13, verbose = TRUE)
  result3 <- makeTwins(ped,
    ID_twin1 = 1,
    ID_twin2 = 2,
    verbose = TRUE, zygosity = "SS"
  )
  expected_result$zygosity[expected_result$zygosity == "DZ"] <- "SS"
  expect_equal(result3, expected_result)
})

test_that("makeTwins - mz Twins specified by generation", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_twin <- 2
  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  #
  result <- makeTwins(ped, gen_twin = gen_twin)
  expect_equal(names(result), c("famID", "ID", "gen", "dadID", "momID", "spID", "sex", "twinID", "zygosity"))
  # do we have the same people?
  expect_equal(result$ID, ped$ID)
  # did it make one pair of twins?
  expect_equal(sum(!is.na(result$twinID)), 2)
  # did it make the pair in the correct generation?
  expect_equal(mean(result$gen[!is.na(result$twinID)]), gen_twin)
  # are they the same sex?
  expect_equal(length(unique(result$sex[!is.na(result$twinID)])), 1)
  # are they from the same family?
  expect_equal(length(unique(result$fam[!is.na(result$twinID)])), 1)
  # do they have the same mom?
  expect_equal(length(unique(result$momID[!is.na(result$twinID)])), 1)
  # do they have the same dad?
  expect_equal(length(unique(result$dadID[!is.na(result$twinID)])), 1)
})

test_that("makeTwins - dz Twins specified by generation", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_twin <- 2
  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  resultdz <- makeTwins(ped, gen_twin = gen_twin, zygosity = "DZ")

  expect_equal(names(resultdz), c("famID", "ID", "gen", "dadID", "momID", "spID", "sex", "twinID", "zygosity"))
  # do we have the same people?
  expect_equal(resultdz$ID, ped$ID)
  # did it make one pair of twins?
  expect_equal(sum(!is.na(resultdz$twinID)), 2)
  # did it make the pair in the correct generation?
  expect_equal(mean(resultdz$gen[!is.na(resultdz$twinID)]), gen_twin)
  # how many sexes do we have?
  sexes <- length(unique(resultdz$sex[!is.na(resultdz$twinID)]))
  expect_lte(sexes, 2)
  expect_gte(sexes, 1)
  # are they from the same family?
  expect_equal(length(unique(resultdz$fam[!is.na(resultdz$twinID)])), 1)
  # do they have the same mom?
  expect_equal(length(unique(resultdz$momID[!is.na(resultdz$twinID)])), 1)
  # do they have the same dad?
  expect_equal(length(unique(resultdz$dadID[!is.na(resultdz$twinID)])), 1)
})
test_that("makeTwins - os Twins specified by generation", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_twin <- 2
  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  resultss <- makeTwins(ped, gen_twin = gen_twin, zygosity = "SS")
  expect_equal(names(resultss), c("famID", "ID", "gen", "dadID", "momID", "spID", "sex", "twinID", "zygosity"))
  # do we have the same people?
  expect_equal(resultss$ID, ped$ID)
  # did it make one pair of twins?
  expect_equal(sum(!is.na(resultss$twinID)), 2)
  # did it make the pair in the correct generation?
  expect_equal(mean(resultss$gen[!is.na(resultss$twinID)]), gen_twin)
  # are they the same sex?
  expect_equal(length(unique(resultss$sex[!is.na(resultss$twinID)])), 1)
  # are they from the same family?
  expect_equal(length(unique(resultss$fam[!is.na(resultss$twinID)])), 1)
  # do they have the same mom?
  expect_equal(length(unique(resultss$momID[!is.na(resultss$twinID)])), 1)
  # do they have the same dad?
  expect_equal(length(unique(resultss$dadID[!is.na(resultss$twinID)])), 1)
})

# Test for makeInbreeding function
test_that("makeInbreeding - Inbred mates specified by IDs", {
  ped <- data.frame(
    famID = c(1, 1, 2, 2),
    ID = c(1, 2, 3, 4),
    gen = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID = c(NA, NA, NA, NA),
    sex = c("M", "F", "M", "F")
  )
  expected_result <- data.frame(
    famID = c(1, 1, 2, 2),
    ID = c(1, 2, 3, 4),
    gen = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID = c(2, 1, NA, NA),
    sex = c("M", "F", "M", "F")
  )
  result <- makeInbreeding(ped, ID_mate1 = 1, ID_mate2 = 2)
  expect_equal(result, expected_result)
})

test_that("makeInbreeding - Inbred mates specified by generation and sibling", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_inbred <- 2
  type_inbred <- "sibling"
  prefer_unmated <- c(TRUE, FALSE)


  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  #
  for (prefer in prefer_unmated) {
    result <- makeInbreeding(ped,
      gen_inbred = gen_inbred, type_inbred = type_inbred,
      prefer_unmated = prefer,
      verbose = TRUE
    )
    expect_equal(names(result), c("famID", "ID", "gen", "dadID", "momID", "spID", "sex"))

    # do we have the same people?
    expect_equal(result$ID, ped$ID)

    # did we get more spID values than we started with?
    expect_gt(sum(!is.na(result$spID)), sum(!is.na(ped$spID)))
  }
})

test_that("makeInbreeding - Inbred mates specified by generation and cousin", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_inbred <- 2
  type_inbred <- "cousin"
  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)

  expect_error(makeInbreeding(ped,
    gen_inbred = gen_inbred,
    type_inbred = type_inbred, verbose = TRUE
  ), regexp = "Cousin inbreedin")
})

test_that("makeInbreeding - Inbred mates specified by generation and non inplemented relations", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_inbred <- 2
  type_inbred <- "not_implemented"
  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)

  expect_error(makeInbreeding(ped,
    gen_inbred = gen_inbred,
    type_inbred = type_inbred, verbose = TRUE
  ), regexp = "The type of inbreeding should be either 'sib' or 'cousin'")
})

test_that("dropLink - Drop specified by ID", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  ID_drop <- 10201

  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  result <- dropLink(ped, ID_drop = ID_drop)


  # are the dataframes the same in both the undropped and dropepd relationships for all but the dropped ID?
  expect_equal(colnames(result), c("famID", "ID", "gen", "dadID", "momID", "spID", "sex"))
  names(ped) <- c("famID", "ID", "gen", "dadID", "momID", "spID", "sex")

  expect_equal(result[result$ID != ID_drop, ], ped[ped$ID != ID_drop, ])

  # are the families of the dropped ID in the original?
  expect_true(!is.na(ped$dadID[ped$ID == ID_drop]) & !is.na(ped$momID[ped$ID == ID_drop]))

  # are the families dropped from the dropped
  expect_true(is.na(result$dadID[result$ID == ID_drop]) & is.na(result$momID[result$ID == ID_drop]))
})


test_that("dropLink - Drop specified by ID", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  ID_drop <- 10201

  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  result <- dropLink(ped, ID_drop = ID_drop)

  expect_equal(colnames(result), c("famID", "ID", "gen", "dadID", "momID", "spID", "sex"))
  names(ped) <- c("famID", "ID", "gen", "dadID", "momID", "spID", "sex")

  # are the dataframes the same in both the undropped and dropped relationships for all but the dropped ID?
  expect_equal(result[result$ID != ID_drop, ], ped[ped$ID != ID_drop, ])

  # are the families of the dropped ID in the original?
  expect_true(!is.na(ped$dadID[ped$ID == ID_drop]) & !is.na(ped$momID[ped$ID == ID_drop]))

  # are the families dropped from the dropped
  expect_true(is.na(result$dadID[result$ID == ID_drop]) & is.na(result$momID[result$ID == ID_drop]))
})

test_that("dropLink - Drop specified by generation", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_drop <- 2

  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  result <- dropLink(ped, gen_drop = gen_drop)
  expect_equal(colnames(result), c("famID", "ID", "gen", "dadID", "momID", "spID", "sex"))
  names(ped) <- c("famID", "ID", "gen", "dadID", "momID", "spID", "sex")
  # are the dataframes the same in both the undropped and dropped relationships for all but the dropped gen?
  expect_equal(result[result$gen != gen_drop, ], ped[ped$gen != gen_drop, ])

  # are there more missing dads?
  expect_lt(sum(is.na(ped$dadID[ped$gen == gen_drop])), sum(is.na(result$dadID[result$gen == gen_drop])))
  # are there more missing moms?
  expect_lt(sum(is.na(ped$momID[ped$gen == gen_drop])), sum(is.na(result$momID[result$gen == gen_drop])))
})


test_that("addPersonToPed works as expected", {
  # Initial pedigree data frame
  ped <- data.frame(
    personID = c(1, 2),
    name = c("Alice", "Bob"),
    sex = c("F", "M"),
    momID = c(NA, NA),
    dadID = c(NA, NA),
    twinID = c(NA_integer_, NA_integer_),
    stringsAsFactors = FALSE
  )

  # Add person with all fields specified
  updated <- addPersonToPed(
    ped,
    name = "Charlie",
    sex = "M",
    momID = 1,
    dadID = 2,
    twinID = NA,
    personID = 10,
    overwrite = FALSE
  )

  expect_equal(nrow(updated), 3)
  expect_equal(updated$personID[3], 10)
  expect_equal(updated$name[3], "Charlie")
  expect_equal(updated$sex[3], "M")
  expect_equal(updated$momID[3], 1)
  expect_equal(updated$dadID[3], 2)
  expect_true(is.na(updated$twinID[3]))

  # Add person with generated ID
  updated2 <- addPersonToPed(ped, name = "Dana", sex = "F")
  expect_equal(nrow(updated2), 3)
  expect_equal(updated2$name[3], "Dana")
  expect_equal(updated2$sex[3], "F")
  expect_equal(updated2$personID[3], max(ped$personID, na.rm = TRUE) + 1)

  # Add person with missing optional fields
  updated3 <- addPersonToPed(ped)
  expect_equal(nrow(updated3), 3)
  expect_true(is.na(updated3$name[3]))
  expect_true(is.na(updated3$sex[3]))
  expect_true(is.na(updated3$twinID[3]))
  expect_true(is.na(updated3$momID[3]))
  expect_true(is.na(updated3$dadID[3]))

  expect_equal(updated3$personID[3], max(ped$personID, na.rm = TRUE) + 1)

  # Add person with overwrite = TRUE
  updated4 <- addPersonToPed(ped, name = "New", sex = "F", personID = 1, overwrite = TRUE)
  expect_equal(nrow(updated4), 2)
  expect_equal(updated4$name[1], "New")
  expect_equal(updated4$sex[1], "F")
  expect_equal(updated4$personID[1], 1)
  expect_true(is.na(updated4$momID[1]))
  expect_true(is.na(updated4$dadID[1]))
  expect_true(is.na(updated4$twinID[1]))
  expect_equal(updated4$momID[2], NA)
  expect_equal(updated4$dadID[2], NA)
  expect_equal(updated4$personID[2], 2)
  expect_equal(updated4$name[2], "Bob")
  expect_equal(updated4$sex[2], "M")
  expect_true(is.na(updated4$twinID[2]))
  expect_equal(updated4$twinID[1], NA_integer_)
})

test_that("addPersonToPed works as expected with zygosity", {
  # Initial pedigree data frame
  ped <- data.frame(
    personID = c(1, 2),
    name = c("Alice", "Bob"),
    sex = c("F", "M"),
    momID = c(NA, NA),
    dadID = c(NA, NA),
    twinID = c(NA_integer_, NA_integer_),
    zygosity = c(NA_character_, NA_character_),
    url = NA_character_,
    stringsAsFactors = FALSE
  )

  # Add person with all fields specified
  updated <- addPersonToPed(
    ped,
    name = "Charlie",
    sex = "M",
    momID = 1,
    dadID = 2,
    twinID = NA,
    personID = 10,
    zygosity = NA,
    overwrite = FALSE
  )

  expect_equal(nrow(updated), 3)
  expect_equal(updated$personID[3], 10)
  expect_equal(updated$name[3], "Charlie")
  expect_equal(updated$sex[3], "M")
  expect_equal(updated$momID[3], 1)
  expect_equal(updated$dadID[3], 2)
  expect_true(is.na(updated$twinID[3]))
  expect_true(is.na(updated$zygosity[3]))
  expect_true(is.na(updated$url[3]))

  # Add person with generated ID
  updated2 <- addPersonToPed(ped, name = "Dana", sex = "F", url = "http://example.com")
  expect_equal(nrow(updated2), 3)
  expect_equal(updated2$name[3], "Dana")
  expect_equal(updated2$sex[3], "F")
  expect_equal(updated2$personID[3], max(ped$personID, na.rm = TRUE) + 1)
  expect_true(is.na(updated2$zygosity[3]))
  expect_true(!is.na(updated2$url[3]))

  # Add person with missing optional fields
  updated3 <- addPersonToPed(updated2)
  expect_equal(nrow(updated3), 4)
  expect_true(is.na(updated3$name[4]))
  expect_true(is.na(updated3$sex[4]))
  expect_true(is.na(updated3$twinID[4]))
  expect_true(is.na(updated3$momID[4]))
  expect_true(is.na(updated3$dadID[4]))
  expect_true(is.na(updated3$zygosity[4]))

  expect_equal(updated3$personID[4], max(ped$personID, na.rm = TRUE) + 2)
})

# Tests for single-ID specification (auto-find the other)

test_that("makeTwins - specify only ID_twin1, auto-find twin2", {
  ped <- data.frame(
    famID = c(1, 1, 1, 1),
    ID = c(1, 2, 3, 4),
    gen = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID = c(NA, NA, NA, NA),
    sex = c("M", "F", "M", "F")
  )
  # Person 3 (M) and 4 (F) are siblings. With DZ zygosity, either could be auto-selected.
  result <- makeTwins(ped, ID_twin1 = 3, zygosity = "DZ")
  expect_equal(sum(!is.na(result$twinID)), 2)
  # Twin1 should be person 3
  expect_equal(result$twinID[result$ID == 3], 4)
  expect_equal(result$twinID[result$ID == 4], 3)
})

test_that("makeTwins - specify only ID_twin2, auto-find twin1", {
  ped <- data.frame(
    famID = c(1, 1, 1, 1),
    ID = c(1, 2, 3, 4),
    gen = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID = c(NA, NA, NA, NA),
    sex = c("M", "F", "M", "F")
  )
  result <- makeTwins(ped, ID_twin2 = 4, zygosity = "DZ")
  expect_equal(sum(!is.na(result$twinID)), 2)
  expect_equal(result$twinID[result$ID == 4], 3)
  expect_equal(result$twinID[result$ID == 3], 4)
})

test_that("makeInbreeding - specify only ID_mate1, auto-find mate2", {
  ped <- data.frame(
    famID = c(1, 1, 1, 1),
    ID = c(1, 2, 3, 4),
    gen = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID = c(NA, NA, NA, NA),
    sex = c("M", "F", "M", "F")
  )
  # Person 3 (M) should auto-find person 4 (F) as opposite-sex sibling
  result <- makeInbreeding(ped, ID_mate1 = 3)
  expect_equal(result$spID[result$ID == 3], 4)
  expect_equal(result$spID[result$ID == 4], 3)
})

# ─── makeTwins edge cases ────────────────────────────────────────────────────

test_that("makeTwins - invalid gen_twin below 2 issues warning and returns unchanged ped", {
  set.seed(1)
  ped <- simulatePedigree(kpc = 4, Ngen = 4, sexR = .5, marR = .7)
  # gen_twin = 1 is invalid (< 2)
  expect_warning(
    result <- makeTwins(ped, gen_twin = 1),
    regexp = "generation of the twins"
  )
  # The returned pedigree should not have twinID or zygosity columns
  expect_false("twinID" %in% colnames(result))
  expect_false("zygosity" %in% colnames(result))
  # Row count unchanged
  expect_equal(nrow(result), nrow(ped))
})

test_that("makeTwins - invalid gen_twin above max generation issues warning", {
  set.seed(1)
  ped <- simulatePedigree(kpc = 4, Ngen = 4, sexR = .5, marR = .7)
  max_gen <- max(ped$gen)
  expect_warning(
    result <- makeTwins(ped, gen_twin = max_gen + 1),
    regexp = "generation of the twins"
  )
  expect_false("twinID" %in% colnames(result))
})

test_that("makeTwins - verbose prints twin IDs when both specified", {
  ped <- data.frame(
    famID = c(1, 1, 1, 1),
    ID    = c(1, 2, 3, 4),
    gen   = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID  = c(NA, NA, NA, NA),
    sex   = c("M", "F", "M", "F")
  )
  # verbose = TRUE should not error
  expect_no_error(makeTwins(ped, ID_twin1 = 3, ID_twin2 = 4, verbose = TRUE))
})

test_that("makeTwins - twinID column is updated in-place when it already exists", {
  set.seed(2)
  ped <- simulatePedigree(kpc = 4, Ngen = 3, sexR = .5, marR = .7)
  # Create first pair of twins (adds twinID column)
  ped_t1 <- makeTwins(ped, gen_twin = 2)
  expect_true("twinID" %in% colnames(ped_t1))
  # Second call should reuse the existing twinID column (not create a new MZtwin column)
  ped_t2 <- expect_no_error(makeTwins(ped_t1, gen_twin = 2))
  expect_true("twinID" %in% colnames(ped_t2))
  expect_false("MZtwin" %in% colnames(ped_t2))
  # At minimum the original twin pair is still recorded
  expect_gte(sum(!is.na(ped_t2$twinID)), 2)
})

# ─── makeInbreeding – auto-find mate1 when only ID_mate2 provided ────────────

test_that("makeInbreeding - specify only ID_mate2, auto-find mate1", {
  ped <- data.frame(
    famID = c(1, 1, 1, 1),
    ID    = c(1, 2, 3, 4),
    gen   = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID  = c(NA, NA, NA, NA),
    sex   = c("M", "F", "M", "F")
  )
  # Person 4 (F) specified; person 3 (M) should be auto-selected as opposite-sex sibling
  result <- makeInbreeding(ped, ID_mate2 = 4)
  expect_equal(result$spID[result$ID == 3], 4)
  expect_equal(result$spID[result$ID == 4], 3)
})

test_that("makeInbreeding - prefer_unmated=TRUE with single ID_mate1 runs without error", {
  set.seed(42)
  ped <- simulatePedigree(kpc = 4, Ngen = 4, sexR = .5, marR = .7)
  # Pick a generation-2 individual with an opposite-sex sibling
  gen2_ids <- ped$ID[ped$gen == 2 & !is.na(ped$dadID)]
  for (cand in gen2_ids) {
    cand_sex <- ped$sex[ped$ID == cand]
    cand_dad <- ped$dadID[ped$ID == cand]
    cand_mom <- ped$momID[ped$ID == cand]
    opp_pool <- ped$ID[
      ped$ID != cand & ped$gen == 2 &
        !is.na(ped$dadID) & ped$dadID == cand_dad &
        !is.na(ped$momID) & ped$momID == cand_mom &
        ped$sex != cand_sex
    ]
    if (length(opp_pool) > 0) {
      result <- expect_no_error(
        makeInbreeding(ped, ID_mate1 = cand, prefer_unmated = TRUE)
      )
      # The candidate's spID should have been set to one of the eligible siblings
      selected_mate <- result$spID[result$ID == cand]
      expect_true(!is.na(selected_mate))
      expect_true(selected_mate %in% opp_pool)
      # The relationship should be symmetric
      expect_equal(result$spID[result$ID == selected_mate], cand)
      break
    }
  }
})

test_that("makeInbreeding - prefer_unmated=TRUE with only ID_mate2", {
  ped <- data.frame(
    famID = c(1, 1, 1, 1),
    ID    = c(1, 2, 3, 4),
    gen   = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID  = c(NA, NA, NA, NA),
    sex   = c("M", "F", "M", "F")
  )
  result <- expect_no_error(
    makeInbreeding(ped, ID_mate2 = 4, prefer_unmated = TRUE)
  )
  expect_equal(result$spID[result$ID == 4], 3)
  expect_equal(result$spID[result$ID == 3], 4)
})

# ─── dropLink – sex_drop filter ──────────────────────────────────────────────

test_that("dropLink - drop only males in a generation via sex_drop", {
  set.seed(15)
  ped <- simulatePedigree(kpc = 4, Ngen = 4, sexR = .5, marR = .7)
  names(ped)[names(ped) == "fam"] <- "famID"

  result <- dropLink(ped, gen_drop = 2, sex_drop = "M")

  # Some male in gen 2 should now have NA parents
  males_gen2 <- result[result$gen == 2 & result$sex == "M", ]
  expect_true(any(is.na(males_gen2$dadID) | is.na(males_gen2$momID)))

  # Females in gen 2 should be completely unchanged
  females_gen2_orig <- ped[ped$gen == 2 & ped$sex == "F", ]
  females_gen2_res <- result[result$gen == 2 & result$sex == "F", ]
  expect_equal(females_gen2_res$dadID, females_gen2_orig$dadID)
  expect_equal(females_gen2_res$momID, females_gen2_orig$momID)
})

test_that("dropLink - warning when target pool is empty", {
  # Generation 1 founders have no dadID/momID, so the pool is always empty
  set.seed(15)
  ped <- simulatePedigree(kpc = 4, Ngen = 4, sexR = .5, marR = .7)
  expect_warning(
    result <- dropLink(ped, gen_drop = 1),
    regexp = "No individual is dropped"
  )
  # Pedigree should be returned unchanged
  expect_equal(nrow(result), nrow(ped))
})

# ─── addPersonToPed – additional paths ───────────────────────────────────────

test_that("addPersonToPed - error when overwrite=TRUE and personID does not exist", {
  ped <- data.frame(
    personID = c(1L, 2L),
    name = c("Alice", "Bob"),
    sex = c("F", "M"),
    momID = c(NA, NA),
    dadID = c(NA, NA),
    twinID = c(NA_integer_, NA_integer_),
    stringsAsFactors = FALSE
  )
  expect_error(
    addPersonToPed(ped, personID = 99, overwrite = TRUE),
    regexp = "does not exist in the pedigree"
  )
})

test_that("addPersonToPed - notes column is handled when present in ped", {
  ped <- data.frame(
    personID = c(1L, 2L),
    name = c("Alice", "Bob"),
    sex = c("F", "M"),
    momID = c(NA, NA),
    dadID = c(NA, NA),
    twinID = c(NA_integer_, NA_integer_),
    notes = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  updated <- addPersonToPed(ped,
    name = "Charlie", sex = "M",
    momID = 1, dadID = 2,
    notes = "test note", personID = 10
  )
  expect_equal(nrow(updated), 3)
  expect_equal(updated$notes[3], "test note")

  # When notes not supplied it should be NA
  updated2 <- addPersonToPed(ped, name = "Dana", sex = "F")
  expect_true(is.na(updated2$notes[3]))
})

test_that("addPersonToPed - non-data.frame input raises error", {
  expect_error(
    addPersonToPed(list(personID = 1), personID = 2)
    # stopifnot(is.data.frame(ped)) fires for non-data.frame input
  )
})

# ─── makePool ────────────────────────────────────────────────────────────────

test_that("makePool returns opposite-sex siblings with shared parents", {
  ped <- data.frame(
    famID = c(1, 1, 1, 1),
    ID    = c(1, 2, 3, 4),
    gen   = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID  = c(NA, NA, NA, NA),
    sex   = c("M", "F", "M", "F")
  )
  # Person 3 is male; person 4 should be the pool
  pool <- BGmisc:::makePool(
    ped = ped,
    mate_id = 3,
    mate_sex = "M",
    mate_dad = 1,
    mate_mom = 2,
    prefer_unmated = FALSE
  )
  expect_equal(pool, 4)
})

test_that("makePool with prefer_unmated=FALSE returns all qualifying siblings", {
  ped <- data.frame(
    famID = c(1, 1, 1, 1, 1),
    ID    = c(1, 2, 3, 4, 5),
    gen   = c(1, 1, 2, 2, 2),
    dadID = c(NA, NA, 1, 1, 1),
    momID = c(NA, NA, 2, 2, 2),
    spID  = c(NA, NA, NA, 5, NA), # person 4 is mated, person 6 is not
    sex   = c("M", "F", "M", "F", "F")
  )
  # Both female siblings (4 and 5) should appear with prefer_unmated=FALSE
  pool <- BGmisc:::makePool(
    ped = ped,
    mate_id = 3,
    mate_sex = "M",
    mate_dad = 1,
    mate_mom = 2,
    prefer_unmated = FALSE
  )
  expect_true(4 %in% pool)
  expect_true(5 %in% pool)
})

test_that("makePool with gen_inbred filters by generation", {
  ped <- data.frame(
    famID = c(1, 1, 1, 1, 1, 1),
    ID    = c(1, 2, 3, 4, 5, 6),
    gen   = c(1, 1, 2, 2, 3, 3),
    dadID = c(NA, NA, 1, 1, NA, NA),
    momID = c(NA, NA, 2, 2, NA, NA),
    spID  = c(NA, NA, NA, NA, NA, NA),
    sex   = c("M", "F", "M", "F", "M", "F")
  )
  # With gen_inbred=2 the pool should be restricted to gen 2
  pool <- BGmisc:::makePool(
    ped = ped,
    mate_id = 3,
    mate_sex = "M",
    mate_dad = 1,
    mate_mom = 2,
    prefer_unmated = FALSE,
    gen_inbred = 2
  )
  # Only person 4 is in gen 2, opposite sex, same parents
  expect_equal(pool, 4)
})
