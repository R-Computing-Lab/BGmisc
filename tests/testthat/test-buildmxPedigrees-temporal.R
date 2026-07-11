# Tests for the temporal branch of buildmxPedigrees.R (temporal = TRUE) and the
# temporal wrapper functions in buildTemporalPedigreeModel.R.
# All functions require the OpenMx package; tests are skipped when it's absent.

# Helper: a minimal 3-person additive relatedness matrix
make_add3 <- function() {
  matrix(c(
    1, 0.5, 0.25,
    0.5, 1, 0.125,
    0.25, 0.125, 1
  ), nrow = 3)
}

# Helper: a 3-person observed data row
make_dat3 <- function(obs_ids = c("y1", "y2", "y3")) {
  matrix(c(0.1, -0.2, 0.3), nrow = 1, dimnames = list(NULL, obs_ids))
}

# ─── buildPedigreeModelCovariance(temporal = TRUE) ──────────────────────────

test_that("buildPedigreeModelCovariance(temporal = TRUE) builds B_* matrices for requested components", {
  skip_if_not_installed("OpenMx")
  mod <- expect_no_error(
    buildPedigreeModelCovariance(temporal = TRUE, components = c("a", "e"))
  )
  expect_true(inherits(mod, "MxModel"))
  expect_false(is.null(mod$B_a))
  expect_false(is.null(mod$B_e))
  expect_null(mod$B_cn)
})

test_that("buildPedigreeModelCovariance(temporal = TRUE) supports the am component", {
  skip_if_not_installed("OpenMx")
  mod <- buildPedigreeModelCovariance(temporal = TRUE, components = c("am", "e"))
  expect_false(is.null(mod$B_am))
})

test_that("buildPedigreeModelCovariance(temporal = TRUE) supports d, cn, ce, and mt components", {
  skip_if_not_installed("OpenMx")
  mod <- buildPedigreeModelCovariance(temporal = TRUE, components = c("d", "cn", "ce", "mt"))
  expect_false(is.null(mod$B_d))
  expect_false(is.null(mod$B_cn))
  expect_false(is.null(mod$B_ce))
  expect_false(is.null(mod$B_mt))
})

test_that("buildPedigreeModelCovariance(temporal = TRUE) works with all components enabled", {
  skip_if_not_installed("OpenMx")
  mod <- expect_no_error(
    buildPedigreeModelCovariance(
      temporal = TRUE,
      components = c("a", "d", "cn", "ce", "mt", "am", "e")
    )
  )
  for (comp in c("a", "d", "cn", "ce", "mt", "am", "e")) {
    expect_false(is.null(mod[[paste0("B_", comp)]]),
      label = paste("Expected component B_", comp, "to be present in model")
    )
  }
})

test_that("buildPedigreeModelCovariance(temporal = TRUE) builds G_* matrices when p_hist > 0", {
  skip_if_not_installed("OpenMx")
  mod <- buildPedigreeModelCovariance(temporal = TRUE, components = c("a", "e"), p_hist = 2)
  expect_false(is.null(mod$G_a))
  expect_equal(nrow(mod$G_a@values), 2)
})

test_that("buildPedigreeModelCovariance(temporal = TRUE) does not build G_* when p_hist = 0", {
  skip_if_not_installed("OpenMx")
  mod <- buildPedigreeModelCovariance(temporal = TRUE, components = c("a", "e"), p_hist = 0)
  expect_null(mod$G_a)
})

test_that("buildPedigreeModelCovariance(temporal = TRUE) sizes B_* rows to time_point_max + 1", {
  skip_if_not_installed("OpenMx")
  mod1 <- buildPedigreeModelCovariance(temporal = TRUE, components = "a", time_point_max = 1)
  expect_equal(nrow(mod1$B_a@values), 2)
  mod2 <- buildPedigreeModelCovariance(temporal = TRUE, components = "a", time_point_max = 5)
  expect_equal(nrow(mod2$B_a@values), 6)
})

test_that("buildPedigreeModelCovariance(temporal = TRUE) rejects a negative time_point_max", {
  skip_if_not_installed("OpenMx")
  expect_error(
    buildPedigreeModelCovariance(temporal = TRUE, components = "a", time_point_max = -1),
    regexp = "non-negative"
  )
})

# ─── buildOneFamilyGroup(temporal = TRUE) ───────────────────────────────────

test_that("buildOneFamilyGroup(temporal = TRUE) returns an mxModel with Ka/Eta_a/L_a algebras", {
  skip_if_not_installed("OpenMx")
  mod <- expect_no_error(
    buildOneFamilyGroup(
      group_name = "fam1",
      Addmat = make_add3(),
      full_df_row = make_dat3(),
      obs_ids = c("y1", "y2", "y3"),
      temporal = TRUE,
      birth_year = c(-1, 0, 1)
    )
  )
  expect_true(inherits(mod, "MxModel"))
  expect_false(is.null(mod$Ka))
  expect_false(is.null(mod$Eta_a))
  expect_false(is.null(mod$L_a))
})

test_that("buildOneFamilyGroup(temporal = TRUE) supports Amimat (am component)", {
  skip_if_not_installed("OpenMx")
  mod <- buildOneFamilyGroup(
    group_name = "famAM",
    Addmat = make_add3(),
    Amimat = make_add3(),
    full_df_row = make_dat3(),
    obs_ids = c("y1", "y2", "y3"),
    temporal = TRUE,
    birth_year = c(-1, 0, 1)
  )
  expect_false(is.null(mod$Am))
  expect_false(is.null(mod$Kam))
})

test_that("buildOneFamilyGroup(temporal = TRUE) supports Dmgmat (d component)", {
  skip_if_not_installed("OpenMx")
  mod <- buildOneFamilyGroup(
    group_name = "famD",
    Dmgmat = make_add3(),
    full_df_row = make_dat3(),
    obs_ids = c("y1", "y2", "y3"),
    temporal = TRUE,
    birth_year = c(-1, 0, 1)
  )
  expect_false(is.null(mod$D))
  expect_false(is.null(mod$Kd))
  expect_false(is.null(mod$Eta_d))
})

test_that("buildOneFamilyGroup(temporal = TRUE) supports Nucmat (cn component)", {
  skip_if_not_installed("OpenMx")
  mod <- buildOneFamilyGroup(
    group_name = "famCn",
    Nucmat = make_add3(),
    full_df_row = make_dat3(),
    obs_ids = c("y1", "y2", "y3"),
    temporal = TRUE,
    birth_year = c(-1, 0, 1)
  )
  expect_false(is.null(mod$Cn))
  expect_false(is.null(mod$Kcn))
  expect_false(is.null(mod$Eta_cn))
})

test_that("buildOneFamilyGroup(temporal = TRUE) supports Extmat (ce component)", {
  skip_if_not_installed("OpenMx")
  mod <- buildOneFamilyGroup(
    group_name = "famCe",
    Extmat = make_add3(),
    full_df_row = make_dat3(),
    obs_ids = c("y1", "y2", "y3"),
    temporal = TRUE,
    birth_year = c(-1, 0, 1)
  )
  expect_false(is.null(mod$Ce))
  expect_false(is.null(mod$Kce))
  expect_false(is.null(mod$Eta_ce))
})

test_that("buildOneFamilyGroup(temporal = TRUE) supports Mtdmat (mt component)", {
  skip_if_not_installed("OpenMx")
  mod <- buildOneFamilyGroup(
    group_name = "famMt",
    Mtdmat = make_add3(),
    full_df_row = make_dat3(),
    obs_ids = c("y1", "y2", "y3"),
    temporal = TRUE,
    birth_year = c(-1, 0, 1)
  )
  expect_false(is.null(mod$Mt))
  expect_false(is.null(mod$Kmt))
  expect_false(is.null(mod$Eta_mt))
})

test_that("buildOneFamilyGroup(temporal = TRUE) works with all components enabled", {
  skip_if_not_installed("OpenMx")
  A3 <- make_add3()
  mod <- expect_no_error(
    buildOneFamilyGroup(
      group_name = "famAll",
      Addmat = A3, Dmgmat = A3, Nucmat = A3, Extmat = A3, Mtdmat = A3, Amimat = A3,
      full_df_row = make_dat3(),
      obs_ids = c("y1", "y2", "y3"),
      temporal = TRUE,
      birth_year = c(-1, 0, 1)
    )
  )
  for (K in c("Ka", "Kd", "Kcn", "Kce", "Kmt", "Kam", "Ke")) {
    expect_false(is.null(mod[[K]]), label = paste("Expected", K, "to be present in model"))
  }
  # unique environment (Ke) is always included in the covariance algebra
  covariance_txt <- deparse(mod$V$formula, width.cutoff = 500L)

  expect_true(grepl("Ke", covariance_txt, fixed = TRUE) |
    grepl("Cov_e", covariance_txt, fixed = TRUE))
})

test_that("buildOneFamilyGroup(temporal = TRUE) errors on mismatched birth_year length", {
  skip_if_not_installed("OpenMx")
  expect_error(
    buildOneFamilyGroup(
      group_name = "fam1",
      Addmat = make_add3(),
      full_df_row = make_dat3(),
      obs_ids = c("y1", "y2", "y3"),
      temporal = TRUE,
      birth_year = c(-1, 0) # wrong length
    ),
    regexp = "birth_year"
  )
})

test_that("buildOneFamilyGroup(temporal = TRUE) includes G_* algebra when H is supplied", {
  skip_if_not_installed("OpenMx")
  H <- matrix(c(0, 1, 1), ncol = 1)
  mod <- buildOneFamilyGroup(
    group_name = "famH",
    Addmat = make_add3(),
    full_df_row = make_dat3(),
    obs_ids = c("y1", "y2", "y3"),
    temporal = TRUE,
    birth_year = c(-1, 0, 1),
    H = H
  )
  expect_false(is.null(mod$H))
  formula_txt <- deparse(mod$Eta_a$formula, width.cutoff = 500L)
  expect_true(grepl("ModelOne.G_a", formula_txt, fixed = TRUE))
})

test_that("buildOneFamilyGroup temporal = FALSE (default) is unaffected by temporal-only args", {
  skip_if_not_installed("OpenMx")
  mod <- buildOneFamilyGroup(
    group_name = "famStatic",
    Addmat = make_add3(),
    full_df_row = make_dat3(),
    obs_ids = c("y1", "y2", "y3")
  )
  expect_true(inherits(mod, "MxModel"))
  expect_false(is.null(mod$A))
  expect_null(mod$Ka)
})

test_that("buildOneFamilyGroup(temporal = TRUE) applies condenseMatrixSlots without erroring", {
  skip_if_not_installed("OpenMx")
  mod <- expect_no_error(
    buildOneFamilyGroup(
      group_name = "famCond",
      Addmat = make_add3(),
      full_df_row = make_dat3(),
      obs_ids = c("y1", "y2", "y3"),
      temporal = TRUE,
      birth_year = c(-1, 0, 1),
      condenseMatrixSlots = TRUE
    )
  )
  expect_true(inherits(mod, "MxModel"))
})

# ─── buildFamilyGroups(temporal = TRUE) ─────────────────────────────────────

test_that("buildFamilyGroups(temporal = TRUE) builds one temporal group per family", {
  skip_if_not_installed("OpenMx")
  Addmat <- make_add3()
  dat <- matrix(
    c(0.1, -0.2, 0.3, 0.2, -0.1, 0.0),
    nrow = 2, byrow = TRUE,
    dimnames = list(NULL, c("y1", "y2", "y3"))
  )
  birth_year_list <- list(c(-1, 0, 1), c(-2, 0, 2))
  groups <- expect_no_error(
    buildFamilyGroups(
      dat = dat, obs_ids = c("y1", "y2", "y3"), Addmat = Addmat,
      temporal = TRUE, birth_year_list = birth_year_list
    )
  )
  expect_equal(length(groups), 2)
  expect_false(is.null(groups[[1]]$Ka))
  expect_false(is.null(groups[[2]]$Ka))
})

# ─── buildFamilyGroups_list ──────────────────────────────────────────────────

test_that("buildFamilyGroups_list(temporal = TRUE) supports per-family relatedness matrices and sizes", {
  skip_if_not_installed("OpenMx")
  A3 <- make_add3()
  A2 <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  groups <- expect_no_error(
    buildFamilyGroups_list(
      dat_list = list(c(0.1, -0.2, 0.3), c(0.4, -0.1)),
      obs_ids_list = list(c("y1", "y2", "y3"), c("z1", "z2")),
      Addmat_list = list(A3, A2),
      temporal = TRUE,
      birth_year_list = list(c(-1, 0, 1), c(-1, 1))
    )
  )
  expect_equal(length(groups), 2)
  expect_equal(nrow(groups[[1]]$A@values), 3)
  expect_equal(nrow(groups[[2]]$A@values), 2)
})

test_that("buildFamilyGroups_list(temporal = FALSE) supports heterogeneous static families", {
  skip_if_not_installed("OpenMx")
  A3 <- make_add3()
  A2 <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  groups <- expect_no_error(
    buildFamilyGroups_list(
      dat_list = list(c(0.1, -0.2, 0.3), c(0.4, -0.1)),
      obs_ids_list = list(c("y1", "y2", "y3"), c("z1", "z2")),
      Addmat_list = list(A3, A2)
    )
  )
  expect_equal(length(groups), 2)
  expect_false(is.null(groups[[1]]$A))
  expect_null(groups[[1]]$Ka)
  expect_equal(nrow(groups[[2]]$A@values), 2)
})

# ─── buildPedigreeMx(temporal = TRUE) ────────────────────────────────────────

test_that("buildPedigreeMx(temporal = TRUE) assembles a multigroup mxModel with CI names", {
  skip_if_not_installed("OpenMx")
  group <- buildOneFamilyGroup(
    group_name = "fam1", Addmat = make_add3(), full_df_row = make_dat3(),
    obs_ids = c("y1", "y2", "y3"), temporal = TRUE, birth_year = c(-1, 0, 1)
  )
  mod <- expect_no_error(
    buildPedigreeMx(
      model_name = "TempPedMx", group_models = list(group),
      temporal = TRUE, p_hist = 0, components = c("a", "e"), ci = TRUE
    )
  )
  expect_true(inherits(mod, "MxModel"))
  expect_true(length(mod@intervals) > 0)
})

# ─── fitPedigreeModel(temporal = TRUE) end-to-end ───────────────────────────

test_that("fitPedigreeModel(temporal = TRUE) fits an AE model end-to-end", {
  skip_if_not_installed("OpenMx")
  set.seed(42)
  Addmat <- make_add3()
  result <- expect_no_error(
    fitPedigreeModel(
      model_name = "TempFitTest",
      temporal = TRUE,
      dat_list = list(c(0.1, -0.1, 0.05), c(0.2, -0.2, 0.1)),
      obs_ids_list = list(c("y1", "y2", "y3"), c("z1", "z2", "z3")),
      birth_year_list = list(c(-1, 0, 1), c(-1, 0, 1)),
      Addmat_list = list(Addmat, Addmat),
      components = c("a", "e"),
      intervals = FALSE,
      tryhard = FALSE
    )
  )
  expect_true(inherits(result, "MxModel"))
})

test_that("fitPedigreeModel(temporal = TRUE) errors without dat_list/obs_ids_list/birth_year_list", {
  skip_if_not_installed("OpenMx")
  expect_error(
    fitPedigreeModel(temporal = TRUE),
    regexp = "dat_list"
  )
})

# ─── temporal wrappers match the generalized functions directly ────────────

test_that("buildOneTemporalFamilyGroup matches buildOneFamilyGroup(temporal = TRUE)", {
  skip_if_not_installed("OpenMx")
  Addmat <- make_add3()
  dat <- make_dat3()
  direct <- buildOneFamilyGroup(
    group_name = "fam1", Addmat = Addmat, full_df_row = dat,
    obs_ids = c("y1", "y2", "y3"), temporal = TRUE, birth_year = c(-1, 0, 1)
  )
  wrapped <- buildOneTemporalFamilyGroup(
    group_name = "fam1", Addmat = Addmat, full_df_row = dat,
    obs_ids = c("y1", "y2", "y3"), birth_year = c(-1, 0, 1)
  )
  expect_equal(direct$A@values, wrapped$A@values)
  expect_equal(deparse(direct$V$formula), deparse(wrapped$V$formula))
})

test_that("buildTemporalPedigreeMx matches buildPedigreeMx(temporal = TRUE)", {
  skip_if_not_installed("OpenMx")
  group <- buildOneFamilyGroup(
    group_name = "fam1", Addmat = make_add3(), full_df_row = make_dat3(),
    obs_ids = c("y1", "y2", "y3"), temporal = TRUE, birth_year = c(-1, 0, 1)
  )
  direct <- buildPedigreeMx(
    model_name = "M", group_models = list(group),
    temporal = TRUE, p_hist = 0, components = c("a", "e")
  )
  wrapped <- buildTemporalPedigreeMx(
    model_name = "M", group_models = list(group),
    p_hist = 0, components = c("a", "e")
  )
  expect_equal(names(direct$ModelOne@matrices), names(wrapped$ModelOne@matrices))
})

test_that("fitTemporalPedigreeModel matches fitPedigreeModel(temporal = TRUE)", {
  skip_if_not_installed("OpenMx")
  set.seed(1)
  Addmat <- make_add3()
  args <- list(
    dat_list = list(c(0.1, -0.1, 0.05), c(0.2, -0.2, 0.1)),
    obs_ids_list = list(c("y1", "y2", "y3"), c("z1", "z2", "z3")),
    birth_year_list = list(c(-1, 0, 1), c(-1, 0, 1)),
    Addmat_list = list(Addmat, Addmat),
    components = c("a", "e"), intervals = FALSE, tryhard = FALSE
  )
  direct <- do.call(fitPedigreeModel, c(list(model_name = "A", temporal = TRUE), args))
  wrapped <- do.call(fitTemporalPedigreeModel, c(list(model_name = "A"), args))
  expect_equal(OpenMx::omxGetParameters(direct), OpenMx::omxGetParameters(wrapped))
})
