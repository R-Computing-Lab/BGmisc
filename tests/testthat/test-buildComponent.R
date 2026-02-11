test_that("MZ twins coded at relatedness 1 via twinID column", {
  # Simple pedigree: two parents and two MZ twin children
  ped <- data.frame(
    ID = c(1, 2, 3, 4),
    momID = c(NA, NA, 2, 2),
    dadID = c(NA, NA, 1, 1),
    sex = c("M", "F", "M", "M"),
    twinID = c(NA, NA, 4, 3),
    zygosity = c(NA, NA, "MZ", "MZ")
  )

  # Without mz_twins: siblings get 0.5
  r_no_mz <- ped2add(ped, mz_twins = FALSE, sparse = FALSE)
  expect_equal(r_no_mz["3", "4"], 0.5)
  expect_equal(r_no_mz["4", "3"], 0.5)

  # With mz_twins: MZ twins get 1.0
  r_mz <- ped2add(ped, mz_twins = TRUE, sparse = FALSE)
  expect_equal(r_mz["3", "4"], 1.0)
  expect_equal(r_mz["4", "3"], 1.0)

  # Self-relatedness should still be 1
  expect_equal(r_mz["3", "3"], 1.0)
  expect_equal(r_mz["4", "4"], 1.0)

  # Parent-child relatedness unchanged
  expect_equal(r_mz["3", "1"], 0.5)
  expect_equal(r_mz["4", "1"], 0.5)
  expect_equal(r_mz["3", "2"], 0.5)
  expect_equal(r_mz["4", "2"], 0.5)
})

test_that("MZ twins without zygosity column assumes all twinID pairs are MZ", {
  ped <- data.frame(
    ID = c(1, 2, 3, 4),
    momID = c(NA, NA, 2, 2),
    dadID = c(NA, NA, 1, 1),
    sex = c("M", "F", "M", "M"),
    twinID = c(NA, NA, 4, 3)
  )

  r_mz <- ped2add(ped, mz_twins = TRUE, sparse = FALSE)
  expect_equal(r_mz["3", "4"], 1.0)
  expect_equal(r_mz["4", "3"], 1.0)
})

test_that("DZ twins with zygosity column are NOT modified", {
  ped <- data.frame(
    ID = c(1, 2, 3, 4),
    momID = c(NA, NA, 2, 2),
    dadID = c(NA, NA, 1, 1),
    sex = c("M", "F", "M", "F"),
    twinID = c(NA, NA, 4, 3),
    zygosity = c(NA, NA, "DZ", "DZ")
  )

  r_mz <- ped2add(ped, mz_twins = TRUE, sparse = FALSE)
  # DZ twins remain at sibling relatedness = 0.5
  expect_equal(r_mz["3", "4"], 0.5)
  expect_equal(r_mz["4", "3"], 0.5)
})

test_that("MZ twins: downstream child relatedness is correct", {
  # 3-generation pedigree: parents -> MZ twins -> twin2 has a child
  ped <- data.frame(
    ID = c(1, 2, 3, 4, 5, 6),
    momID = c(NA, NA, 2, 2, NA, 4),
    dadID = c(NA, NA, 1, 1, NA, 5),
    sex = c("M", "F", "M", "M", "F", "M"),
    twinID = c(NA, NA, 4, 3, NA, NA),
    zygosity = c(NA, NA, "MZ", "MZ", NA, NA)
  )

  r_mz <- ped2add(ped, mz_twins = TRUE, sparse = FALSE)

  # MZ twins at 1.0
  expect_equal(r_mz["3", "4"], 1.0)

  # Child of twin2 (ID=4) should be 0.5 to twin2 (parent)
  expect_equal(r_mz["6", "4"], 0.5)

  # Child of twin2 should ALSO be 0.5 to twin1 (genetically identical to parent)
  expect_equal(r_mz["6", "3"], 0.5)

  # Diagonal for both twins should be clean (no inflation)
  expect_equal(r_mz["3", "3"], 1.0)
  expect_equal(r_mz["4", "4"], 1.0)
})

test_that(".assignParentValue works", {
  expect_equal(.assignParentValue("generation"), .5)
  expect_equal(.assignParentValue("additive"), .5)
  expect_equal(.assignParentValue("common nuclear"), 1)
  expect_equal(.assignParentValue("mitochondrial"), 1)
  expect_equal(.assignParentValue("mtdna"), 1)
  expect_equal(.assignParentValue("mitochondria"), 1)

  expect_error(
    .assignParentValue("unknown component"),
    "Don't know how to set parental value"
  )
})


test_that("ped2gen produces correct correct generations for hazzard and
deviantions all make sense", {
  data(hazard)
  df_hazard <- hazard
  df_hazard$gen_og <- df_hazard$gen

  # Recalculate generations
  df_hazard$gen <- ped2gen(df_hazard, sparse = FALSE)

  expect_true(is.numeric(df_hazard$gen))

  # all differences should be 0 except for founders because the og data uses spouses to set generation for founders

  df_hazard$gen_diff <- abs(df_hazard$gen_og - df_hazard$gen)
  expect_true(all(df_hazard$gen[is.na(df_hazard$momID) & is.na(df_hazard$dadID)] == 1)) # Founders all be 1
  expect_true(all(df_hazard$gen_diff[!(is.na(df_hazard$momID) & is.na(df_hazard$dadID))] == 0)) # Non-founders match original

  # check min generation for children of founders
  founders <- df_hazard$ID[is.na(df_hazard$momID) & is.na(df_hazard$dadID)]
  expect_true(all(df_hazard$gen[df_hazard$momID %in% founders | df_hazard$dadID %in% founders] > 1)) # Children of founders > 1

  # get min generation for children of founders by founder
  for (f in founders) {
    children <- df_hazard$ID[df_hazard$momID == f | df_hazard$dadID == f]
    if (length(children) > 0) {
      df_hazard$min_gen_children[df_hazard$ID == f] <- min(df_hazard$gen[df_hazard$ID %in% children])
    }
  }

  expect_true(all(df_hazard$min_gen_children[df_hazard$ID %in% founders] > 1)) # Children of founders should be generation 2
  expect_true(all(is.na(df_hazard$min_gen_children[!df_hazard$ID %in% founders])))


  # reconstruct the orginal generation values from generated values of children
  expect_true(all(df_hazard$min_gen_children[df_hazard$ID %in% founders] - 1 == df_hazard$gen_og[df_hazard$ID %in% founders]))
})
