
test_that("computeParentAdjacency matches across all methods for additive component", {
  data(hazard)
  tolerance <- 1e-10
  save_path <- tempdir()
  config <- list(nr = nrow(hazard))
  init_list <- vector("list", nrow(hazard))
  init_lens <- integer(nrow(hazard))
  checkpoint_files <- list(
    parList = file.path(save_path, "parList.rds"),
    lens = file.path(save_path, "lens.rds")
  )

  adj_loop <- computeParentAdjacency(
    ped = hazard, component = "additive", adjacency_method = "loop",
    saveable = FALSE, resume = FALSE, save_path = save_path,
    verbose = FALSE, lastComputed = 0, checkpoint_files = checkpoint_files,
    update_rate = 10, parList = init_list, lens = init_lens, save_rate_parlist = 20,
    config = config
  ) %>%
    as.data.frame() %>% # sort by iss and jss to ensure consistent ordering
    dplyr::arrange(iss, jss)

  adj_indexed <- computeParentAdjacency(
    ped = hazard, component = "additive", adjacency_method = "indexed",
    saveable = FALSE, resume = FALSE, save_path = save_path,
    verbose = FALSE, lastComputed = 0, checkpoint_files = checkpoint_files,
    update_rate = 10, parList = init_list, lens = init_lens, save_rate_parlist = 20,
    config = config
  ) %>%
    as.data.frame() %>% # sort by iss and jss to ensure consistent ordering
    dplyr::arrange(iss, jss)

  adj_direct <- computeParentAdjacency(
    ped = hazard, component = "additive", adjacency_method = "direct",
    saveable = FALSE, resume = FALSE, save_path = save_path,
    verbose = FALSE, lastComputed = 0, checkpoint_files = checkpoint_files,
    update_rate = 10, parList = init_list, lens = init_lens, save_rate_parlist = 20,
    config = config
  ) %>%
    as.data.frame() %>% # sort by iss and jss to ensure consistent ordering
    dplyr::arrange(iss, jss)

  expect_equal(adj_loop, adj_indexed, tolerance = tolerance)
  expect_equal(adj_loop, adj_direct, tolerance = tolerance)
  expect_equal(adj_indexed, adj_direct, tolerance = tolerance)
})

test_that("adjBeta matches .adjDirect for common nuclear component", {
  data(hazard)
  tolerance <- 1e-10
  config <- list(nr = nrow(hazard))

  beta_5 <- .adjBeta(
    ped = hazard, component = "common nuclear", adjBeta_method = 5,
    parList = NULL, lens = NULL, lastComputed = 0, saveable = FALSE, resume = FALSE,
    save_path = NULL, verbose = FALSE, save_rate_parlist = NULL, update_rate = NULL,
    checkpoint_files = NULL, config = config
  ) %>%
    as.data.frame() %>% # sort by iss and jss to ensure consistent ordering
    dplyr::arrange(iss, jss)

  direct <- .adjDirect(
    ped = hazard, component = "common nuclear", saveable = FALSE, resume = FALSE,
    save_path = NULL, verbose = FALSE, lastComputed = 0, checkpoint_files = NULL,
    update_rate = NULL, parList = NULL, lens = NULL, save_rate_parlist = NULL,
    config = config
  )  %>%
    as.data.frame() %>% # sort by iss and jss to ensure consistent ordering
    dplyr::arrange(iss, jss)

  expect_equal(beta_5, direct, tolerance = tolerance)
})

test_that("computeParentAdjacency returns empty if already computed", {
  data(hazard)
  config <- list(nr = nrow(hazard))
  out <- computeParentAdjacency(
    ped = hazard, component = "additive", adjacency_method = "loop",
    saveable = FALSE, resume = FALSE, save_path = NULL, verbose = FALSE,
    lastComputed = nrow(hazard), checkpoint_files = list(parList = "", lens = ""),
    update_rate = 10, parList = list(), lens = integer(nrow(hazard)),
    save_rate_parlist = 20, config = config
  )
  expect_null(out)
})

test_that("adjBeta method 1 produces expected structure and symmetric indices", {
  data(hazard)
  config <- list(nr = nrow(hazard))
  out <- .adjBeta(
    ped = hazard, component = "common nuclear", adjBeta_method = 1,
    config = config
  )
  expect_true(is.list(out))
  expect_true(all(c("iss", "jss") %in% names(out)))
  expect_length(out$iss, length(out$jss))
  # symmetric sibling graph
  expect_equal(sort(out$iss), sort(out$jss))
})

test_that("adjDirect handles mitochondrial component correctly", {
  data(hazard)
  config <- list(nr = nrow(hazard))
  out <- .adjDirect(
    ped = hazard, component = "mitochondrial", saveable = FALSE, resume = FALSE,
    save_path = NULL, verbose = FALSE, lastComputed = 0, checkpoint_files = NULL,
    update_rate = NULL, parList = NULL, lens = NULL, save_rate_parlist = NULL,
    config = config
  )
  expect_true(all(hazard$ID[out$iss] %in% hazard$ID))
  expect_true(all(hazard$ID[out$jss] %in% hazard$momID))
})
