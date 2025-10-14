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
