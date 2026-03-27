test_that("MZ twins coded at relatedness 1 via twinID column", {
  # Simple pedigree: two parents and two MZ twin children
  ped <- potter

  mz_method_opts <- c("addtwins", "merging")

  for (mz_method in mz_method_opts) {
    #  mz_method <- "merging" # "addtwins"
    # Without mz_twins: siblings get 0.5
    r_no_mz <- ped2add(ped, mz_twins = FALSE, sparse = FALSE, mz_method = mz_method)
    expect_equal(r_no_mz["12", "13"], 0.5)
    expect_equal(r_no_mz["13", "12"], 0.5)

    # With mz_twins: MZ twins get 1.0
    r_mz <- ped2add(ped, mz_twins = TRUE, sparse = FALSE, mz_method = mz_method)
    expect_equal(r_mz["12", "13"], 1.0)
    expect_equal(r_mz["13", "12"], 1.0)

    # Self-relatedness should still be 1
    expect_equal(r_mz["12", "12"], 1.0)
    expect_equal(r_mz["13", "13"], 1.0)

    # Parent-child relatedness unchanged
    expect_equal(r_mz["12", "9"], 0.5)
    expect_equal(r_mz["13", "9"], 0.5)
    expect_equal(r_mz["12", "10"], 0.5)
    expect_equal(r_mz["13", "10"], 0.5)
  }
  ped_kids <- potter

  # Add a child to one of the MZ twins
  ped_kids <- addPersonToPed(ped_kids, sex = 0, momID = NA, dadID = NA, personID = 31)
  ped_kids <- addPersonToPed(ped_kids, sex = 0, momID = NA, dadID = NA, personID = 32)
  ped_kids <- addPersonToPed(ped_kids, sex = 0, momID = 31, dadID = 12, personID = 33)
  ped_kids <- addPersonToPed(ped_kids, sex = 0, momID = 32, dadID = 13, personID = 34)
  ped_kids <- addPersonToPed(ped_kids, sex = 0, momID = 31, dadID = 13, personID = 35)

  for (mz_method in mz_method_opts) {
    r_kids <- ped2add(ped_kids, mz_twins = TRUE, sparse = FALSE, mz_method = mz_method)
    # Child of twin1 (ID=31) should be 0.5 to twin1 (parent)
    expect_equal(r_kids["33", "12"], 0.5)
    # Child of twin1 should ALSO be 0.5 to twin2 (genetically identical to parent)
    expect_equal(r_kids["33", "13"], 0.5)
    # Child of twin2 (ID=32) should be 0.5 to twin
    expect_equal(r_kids["34", "13"], 0.5)
    # Child of twin2 should ALSO be 0.5 to twin1 (genetically identical to parent)
    expect_equal(r_kids["34", "12"], 0.5)

    # different moms should be 0.25 with different mz twin dads
    expect_equal(r_kids["34", "33"], 0.25)
    expect_equal(r_kids["34", "35"], 0.25)

    # same mom, different mz twin dads should be 0.5
    expect_equal(r_kids["33", "35"], 0.5)
  }

  r_mz1 <- ped2add(ped_kids, mz_twins = TRUE, sparse = FALSE, mz_method = mz_method_opts[1])

  r_mz2 <- ped2add(ped_kids, mz_twins = TRUE, sparse = FALSE, mz_method = mz_method_opts[2])

  expect_equal(r_mz1, r_mz2)
})


test_that("MZ twins coded at relatedness 1 via twinID column (complex pedigree)", {
  set.seed(1667)
  Ngen <- 5
  kpc <- 4
  sexR <- .50 # sometimes fails above .5
  marR <- 1

  gen_twin <- 3 # max(c(floor(Ngen / 2) - 1, 2))

  # create base pedigree with twins at gen_twin

  df_midgen_base <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR, beta = TRUE) |>
    makeTwins(gen_twin = gen_twin)

  twinIDS <- findMZtwins(df_midgen_base,
    returnRows = FALSE,
    returnIDs = TRUE, returnAsList = FALSE
  )

  moms_of_twins <- df_midgen_base$momID[df_midgen_base$ID %in% c(twinIDS$twin1_id, twinIDS$twin2_id)] %>% unique()
  dads_of_twins <- df_midgen_base$dadID[df_midgen_base$ID %in% c(twinIDS$twin1_id, twinIDS$twin2_id)] %>% unique()
  parents_of_twins <- unique(c(moms_of_twins, dads_of_twins))

  grandmothers_of_twins <- df_midgen_base$momID[df_midgen_base$ID %in% parents_of_twins] %>% unique()
  grandfathers_of_twins <- df_midgen_base$dadID[df_midgen_base$ID %in% parents_of_twins] %>% unique()
  grandfathers_of_twins <- grandfathers_of_twins[!is.na(grandfathers_of_twins)]
  grandmothers_of_twins <- grandmothers_of_twins[!is.na(grandmothers_of_twins)]
  grandparents_of_twins <- unique(c(grandmothers_of_twins, grandfathers_of_twins))

  female_children_of_twins <- df_midgen_base$ID[(df_midgen_base$momID %in% c(twinIDS$twin1_id, twinIDS$twin2_id) | df_midgen_base$dadID %in% c(twinIDS$twin1_id, twinIDS$twin2_id)) & df_midgen_base$sex == "F"] %>% unique()

  male_children_of_twins <- df_midgen_base$ID[(df_midgen_base$momID %in% c(twinIDS$twin1_id, twinIDS$twin2_id) | df_midgen_base$dadID %in% c(twinIDS$twin1_id, twinIDS$twin2_id)) & df_midgen_base$sex == "M"] %>% unique()

  children_of_twins <- c(male_children_of_twins, female_children_of_twins)

  df_midgen_below <- df_midgen_base |> makeInbreeding(ID_mate1 = male_children_of_twins[1], ID_mate2 = female_children_of_twins[length(female_children_of_twins)])

  df_midgen_above <- df_midgen_base

  df_midgen_above$momID[df_midgen_above$ID %in% parents_of_twins] <- grandmothers_of_twins[1]
  df_midgen_above$dadID[df_midgen_above$ID %in% parents_of_twins] <- grandfathers_of_twins[1]

  if (FALSE) {
    df_midgen_below %>%
      rename(personID = ID) %>%
      ggpedigree::ggPedigreeInteractive(config = list(
        code_male = "M", focal_fill_personID = twinIDS$twin1_id,
        focal_fill_include = TRUE,
        sex_color_include = FALSE
      ))
  }
  for (df_midgen in list(df_midgen_base, df_midgen_below, df_midgen_above)) {
    r_mz1 <- df_midgen |>
      ped2add(mz_method = "merging", mz_twins = TRUE)
    r_mz2 <- df_midgen |>
      ped2add(mz_method = "addtwins", mz_twins = TRUE)

    r_mz3 <- df_midgen |>
      ped2add(mz_twins = FALSE)

    # which rows are the twins
    twin_rows <- which(!is.na(df_midgen$twinID))
    child_rows <- which(df_midgen$momID %in% df_midgen$ID[twin_rows] | df_midgen$dadID %in% df_midgen$ID[twin_rows])

    family_rows <- unique(c(twin_rows, child_rows))

    expect_equal(sum(as.matrix(r_mz1[family_rows, family_rows]) - as.matrix(r_mz2[family_rows, family_rows])), 0)

    expect_gt(sum(as.matrix(r_mz1[family_rows, family_rows])), sum(as.matrix(r_mz3[family_rows, family_rows])))

    expect_gt(sum(as.matrix(r_mz2[family_rows, family_rows])), sum(as.matrix(r_mz3[family_rows, family_rows])))


    r_mz1_ordered <- r_mz1[order(rownames(r_mz1)), order(colnames(r_mz1))]
    r_mz2_ordered <- r_mz2[order(rownames(r_mz2)), order(colnames(r_mz2))]

    expect_equal(sum(r_mz1_ordered - r_mz2_ordered), 0)

    expect_equal(length(r_mz1@i), length(r_mz2@i))
    expect_equal(length(r_mz1@x), length(r_mz2@x))
    expect_equal(length(r_mz1@p), length(r_mz2@p))

    expect_equal(length(r_mz1@i), length(r_mz3@i))
    expect_equal(length(r_mz2@i), length(r_mz3@i))

    expect_equal(length(r_mz1@x), length(r_mz3@x))
    expect_equal(length(r_mz2@x), length(r_mz3@x))

    expect_equal(length(r_mz1@p), length(r_mz3@p))
    expect_equal(length(r_mz2@p), length(r_mz3@p))
  }
})

test_that("MZ twins coded at relatedness 1 via twinID column (minimal data.frame)", {
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

# ── ram_checkpoint ────────────────────────────────────────────────────────────

test_that("resume loads ram_checkpoint and skips RAM loop when mid-loop files absent", {
  data(hazard)
  save_path <- file.path(tempdir(), "test_ram_checkpoint")
  dir.create(save_path, showWarnings = FALSE)
  on.exit(unlink(save_path, recursive = TRUE))

  # First run: save all checkpoints
  r_full <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    saveable = TRUE, resume = FALSE,
    save_rate_gen = 1, save_path = save_path
  )

  # Remove mid-loop files so only ram_checkpoint (and earlier) remain
  mid_loop_files <- c(
    "r_checkpoint.rds", "gen_checkpoint.rds",
    "mtSum_checkpoint.rds", "newIsPar_checkpoint.rds",
    "count_checkpoint.rds"
  )
  file.remove(file.path(save_path, mid_loop_files))

  expect_true(file.exists(file.path(save_path, "ram_checkpoint.rds")))
  expect_false(file.exists(file.path(save_path, "r_checkpoint.rds")))

  # Second run: should load ram_checkpoint, skip loop, and produce same result
  r_resumed <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    saveable = FALSE, resume = TRUE,
    save_path = save_path
  )

  expect_equal(r_full, r_resumed)
})

test_that("ram_checkpoint resume produces identical result to fresh run", {
  data(hazard)
  save_path <- file.path(tempdir(), "test_ram_idempotent")
  dir.create(save_path, showWarnings = FALSE)
  on.exit(unlink(save_path, recursive = TRUE))

  r_fresh <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    saveable = TRUE, resume = FALSE,
    save_rate_gen = 1, save_path = save_path
  )

  file.remove(file.path(save_path, "r_checkpoint.rds"))
  file.remove(file.path(save_path, "tcrossprod_checkpoint.rds"))

  r_resumed <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    saveable = FALSE, resume = TRUE,
    save_path = save_path
  )

  expect_equal(r_fresh, r_resumed, tolerance = 1e-10)
})

# ── keep_ids ──────────────────────────────────────────────────────────────────

test_that("keep_ids subset produces correct relatedness values", {
  data(hazard)
  keep <- as.character(hazard$ID[5:10])

  r_full <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    keep_ids = NULL
  )
  r_sub <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    keep_ids = keep
  )

  expect_equal(dim(r_sub), c(length(keep), length(keep)))
  expect_equal(rownames(r_sub), keep)

  # values in the subset must match the corresponding entries of the full matrix
  expect_equal(r_sub, r_full[keep, keep], tolerance = 1e-10)


  r_full <- ped2com(hazard,
    component = "additive", sparse = TRUE,
    keep_ids = NULL
  )
  r_sub <- ped2com(hazard,
    component = "additive", sparse = TRUE,
    keep_ids = keep
  )

  expect_equal(dim(r_sub), c(length(keep), length(keep)))
  expect_equal(rownames(r_sub), keep)

  # values in the subset must match the corresponding entries of the full matrix
  expect_equal(r_sub, r_full[keep, keep], tolerance = 1e-10)
})

test_that("keep_ids subset produces correct relatedness values across all families", {
  data(inbreeding)

  r_full_nosparse <- ped2com(inbreeding,
    component = "additive", sparse = FALSE,
    keep_ids = NULL
  )

  r_full_sparse <- ped2com(inbreeding,
    component = "additive", sparse = TRUE,
    keep_ids = NULL
  )
  for (i in unique(inbreeding$famID)) {
    keep <- as.character(sample(inbreeding$ID[inbreeding$famID == i], 6))

    r_sub <- ped2com(inbreeding,
      component = "additive", sparse = FALSE,
      keep_ids = keep
    )

    expect_equal(dim(r_sub), c(length(keep), length(keep)))
    expect_equal(rownames(r_sub), keep)
    r_full <- r_full_nosparse
    # values in the subset must match the corresponding entries of the full matrix
    expect_equal(r_sub, r_full[keep, keep], tolerance = 1e-10)

    r_full <- r_full_sparse
    r_sub <- ped2com(inbreeding,
      component = "additive", sparse = TRUE,
      keep_ids = keep
    )

    expect_equal(dim(r_sub), c(length(keep), length(keep)))
    expect_equal(rownames(r_sub), keep)

    # values in the subset must match the corresponding entries of the full matrix
    expect_equal(r_sub, r_full[keep, keep], tolerance = 1e-10)
  }
})


test_that("keep_ids subset produces correct relatedness values across all families", {
  library(ggpedigree)
  data(ASOIAF)
  df <- ASOIAF |> dplyr::rename(ID = id)

  r_full_sparse <- ped2com(df,
    component = "additive", sparse = TRUE,
    keep_ids = NULL,
    mz_twins = FALSE
  )
  for (i in unique(df$famID)) {
    n_rows <- sum(df$famID == i)
    if (n_rows < 3) {
      next
    }
    keep <- as.character(sample(df$ID[df$famID == i], min(15, n_rows)))


    r_full <- r_full_sparse
    r_sub <- ped2com(df,
      component = "additive", sparse = TRUE,
      keep_ids = keep,
      mz_twins = FALSE
    )

    expect_equal(dim(r_sub), c(length(keep), length(keep)))
    expect_equal(rownames(r_sub), keep)

    # values in the subset must match the corresponding entries of the full matrix
    expect_equal(r_sub, r_full[keep, keep], tolerance = 1e-10, info = paste("Family", i))

    # entirely random subset of IDs across the whole dataset (not just within family)
    keep <- as.character(sample(df$ID, 15))


    r_full <- r_full_sparse
    r_sub <- ped2com(df,
      component = "additive", sparse = TRUE,
      keep_ids = keep,
      mz_twins = FALSE
    )

    expect_equal(dim(r_sub), c(length(keep), length(keep)))
    expect_equal(rownames(r_sub), keep)

    # values in the subset must match the corresponding entries of the full matrix
    expect_equal(r_sub, r_full[keep, keep], tolerance = 1e-10, info = paste(keep))
  }
})



test_that("keep_ids with unknown IDs warns and drops missing entries", {
  data(hazard)
  keep <- c(as.character(hazard$ID[1:3]), "BOGUS_ID")

  expect_warning(
    r_sub <- ped2com(hazard,
      component = "additive", sparse = FALSE,
      keep_ids = keep
    ),
    "keep_ids not found"
  )
  expect_equal(nrow(r_sub), 3L)
})

test_that("keep_ids = NULL returns full pedigree matrix", {
  data(hazard)
  r_full <- ped2com(hazard,
    component = "additive", sparse = FALSE,
    keep_ids = NULL
  )
  expect_equal(nrow(r_full), nrow(hazard))
})
