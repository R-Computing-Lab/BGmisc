# Tests for buildmxPedigrees.R
# All functions in this file require the OpenMx package.
# Tests are skipped automatically when OpenMx is not installed.

# Helper: a minimal 2-person additive relatedness matrix (parent-child)
make_add2 <- function() matrix(c(1, 0.5, 0.5, 1), nrow = 2)

# Helper: a 2-person observed data row
make_dat2 <- function(obs_ids = c("y1", "y2")) {
  matrix(c(1.5, 2.5), nrow = 1, dimnames = list(NULL, obs_ids))
}

# ─── buildPedigreeModelCovariance ────────────────────────────────────────────

test_that("buildPedigreeModelCovariance returns an mxModel with default components", {
  skip_if_not_installed("OpenMx")
  vars <- list(
    ad2 = 0.5, dd2 = 0.3, cn2 = 0.2, ce2 = 0.4,
    mt2 = 0.1, am2 = 0.25, ee2 = 0.6
  )
  mod <- expect_no_error(
    buildPedigreeModelCovariance(vars = vars)
  )
  expect_true(inherits(mod, "MxModel"))
  # Default flags: Vad, Vcn, Vce, Vmt, Ver are TRUE; Vdd and Vam are FALSE
  expect_false(is.null(mod$Vad))
  expect_false(is.null(mod$Vcn))
  expect_false(is.null(mod$Vce))
  expect_false(is.null(mod$Vmt))
  expect_false(is.null(mod$Ver))
  expect_null(mod$Vdd)
  expect_null(mod$Vam)
})

test_that("buildPedigreeModelCovariance includes dominance component when Vdd = TRUE", {
  skip_if_not_installed("OpenMx")
  vars <- list(
    ad2 = 0.5, dd2 = 0.3, cn2 = 0.2, ce2 = 0.4,
    mt2 = 0.1, am2 = 0.25, ee2 = 0.6
  )
  mod <- buildPedigreeModelCovariance(vars = vars, Vdd = TRUE)
  expect_false(is.null(mod$Vdd))
})

test_that("buildPedigreeModelCovariance includes A×mt interaction when Vam = TRUE", {
  skip_if_not_installed("OpenMx")
  vars <- list(
    ad2 = 0.5, dd2 = 0.3, cn2 = 0.2, ce2 = 0.4,
    mt2 = 0.1, am2 = 0.25, ee2 = 0.6
  )
  mod <- buildPedigreeModelCovariance(vars = vars, Vam = TRUE)
  expect_false(is.null(mod$Vam))
})

test_that("buildPedigreeModelCovariance works with all components enabled", {
  skip_if_not_installed("OpenMx")
  vars <- list(
    ad2 = 0.5, dd2 = 0.3, cn2 = 0.2, ce2 = 0.4,
    mt2 = 0.1, am2 = 0.25, ee2 = 0.6
  )
  mod <- expect_no_error(
    buildPedigreeModelCovariance(
      vars = vars,
      Vad = TRUE, Vdd = TRUE, Vcn = TRUE,
      Vce = TRUE, Vmt = TRUE, Vam = TRUE, Ver = TRUE
    )
  )
  for (comp in c("Vad", "Vdd", "Vcn", "Vce", "Vmt", "Vam", "Ver")) {
    expect_false(is.null(mod[[comp]]),
      label = paste("Expected component", comp, "to be present in model")
    )
  }
})

test_that("buildPedigreeModelCovariance works with minimal components (Vad + Ver only)", {
  skip_if_not_installed("OpenMx")
  vars <- list(
    ad2 = 0.5, dd2 = 0.3, cn2 = 0.2, ce2 = 0.4,
    mt2 = 0.1, am2 = 0.25, ee2 = 0.6
  )
  mod <- expect_no_error(
    buildPedigreeModelCovariance(
      vars = vars,
      Vad = TRUE, Vdd = FALSE, Vcn = FALSE,
      Vce = FALSE, Vmt = FALSE, Vam = FALSE, Ver = TRUE
    )
  )
  expect_false(is.null(mod$Vad))
  expect_false(is.null(mod$Ver))
  expect_null(mod$Vcn)
  expect_null(mod$Vce)
})

# ─── buildOneFamilyGroup ─────────────────────────────────────────────────────

test_that("buildOneFamilyGroup errors when no relatedness matrix is provided", {
  skip_if_not_installed("OpenMx")
  dat <- make_dat2()
  expect_error(
    buildOneFamilyGroup(
      group_name = "fam1",
      Addmat = NULL, Nucmat = NULL, Extmat = NULL,
      Mtdmat = NULL, Amimat = NULL, Dmgmat = NULL,
      full_df_row = dat,
      obs_ids = c("y1", "y2")
    ),
    regexp = "At least one relatedness matrix must be provided"
  )
})

test_that("buildOneFamilyGroup returns an mxModel with an additive matrix", {
  skip_if_not_installed("OpenMx")
  Addmat <- make_add2()
  dat <- make_dat2()
  mod <- expect_no_error(
    buildOneFamilyGroup(
      group_name = "fam1",
      Addmat = Addmat,
      full_df_row = dat,
      obs_ids = c("y1", "y2")
    )
  )
  expect_true(inherits(mod, "MxModel"))
  expect_equal(mod$name, "fam1")
  expect_false(is.null(mod$A))
})

test_that("buildOneFamilyGroup returns an mxModel with nuclear family matrix", {
  skip_if_not_installed("OpenMx")
  Nucmat <- make_add2()
  dat <- make_dat2()
  mod <- expect_no_error(
    buildOneFamilyGroup(
      group_name = "fam2",
      Nucmat = Nucmat,
      full_df_row = dat,
      obs_ids = c("y1", "y2")
    )
  )
  expect_true(inherits(mod, "MxModel"))
  expect_false(is.null(mod$Cn))
})

test_that("buildOneFamilyGroup determines family size from any provided matrix", {
  skip_if_not_installed("OpenMx")
  # Use Extmat to size the model
  Extmat <- matrix(c(1, 1, 1, 1), nrow = 2)
  dat <- make_dat2()
  mod <- expect_no_error(
    buildOneFamilyGroup(
      group_name = "famExt",
      Extmat = Extmat,
      full_df_row = dat,
      obs_ids = c("y1", "y2")
    )
  )
  # Extmat values are used directly as matrix Ce in the model
  expect_false(is.null(mod$Ce))
})

# ─── buildFamilyGroups ───────────────────────────────────────────────────────

test_that("buildFamilyGroups returns one group model per row of data", {
  skip_if_not_installed("OpenMx")
  Addmat <- make_add2()
  # Two families, each with 2 observed variables
  dat <- matrix(c(1.0, 2.0, 3.0, 4.0),
    nrow = 2,
    dimnames = list(NULL, c("y1", "y2"))
  )
  groups <- expect_no_error(
    buildFamilyGroups(dat = dat, obs_ids = c("y1", "y2"), Addmat = Addmat)
  )
  expect_true(is.list(groups))
  expect_equal(length(groups), nrow(dat))
})

test_that("buildFamilyGroups names group models with supplied prefix", {
  skip_if_not_installed("OpenMx")
  Addmat <- make_add2()
  dat <- matrix(c(1.0, 2.0), nrow = 1, dimnames = list(NULL, c("y1", "y2")))
  groups <- buildFamilyGroups(
    dat = dat, obs_ids = c("y1", "y2"),
    Addmat = Addmat, prefix = "family"
  )
  expect_equal(groups[[1]]$name, "family1")
})

test_that("buildFamilyGroups default prefix is 'fam'", {
  skip_if_not_installed("OpenMx")
  Addmat <- make_add2()
  dat <- matrix(c(1.0, 2.0), nrow = 1, dimnames = list(NULL, c("y1", "y2")))
  groups <- buildFamilyGroups(
    dat = dat, obs_ids = c("y1", "y2"), Addmat = Addmat
  )
  expect_equal(groups[[1]]$name, "fam1")
})

# ─── buildPedigreeMx ─────────────────────────────────────────────────────────

test_that("buildPedigreeMx returns a multigroup mxModel", {
  skip_if_not_installed("OpenMx")
  vars <- list(
    ad2 = 0.5, dd2 = 0.3, cn2 = 0.2, ce2 = 0.4,
    mt2 = 0.1, am2 = 0.25, ee2 = 0.6
  )
  Addmat <- make_add2()
  dat <- matrix(c(1.0, 2.0, 3.0, 4.0),
    nrow = 2,
    dimnames = list(NULL, c("y1", "y2"))
  )
  group_models <- buildFamilyGroups(
    dat = dat, obs_ids = c("y1", "y2"), Addmat = Addmat
  )
  mod <- expect_no_error(
    buildPedigreeMx(
      model_name   = "TestPedigreeMx",
      vars         = vars,
      group_models = group_models
    )
  )
  expect_true(inherits(mod, "MxModel"))
  expect_equal(mod$name, "TestPedigreeMx")
})

# ─── fitPedigreeModel ────────────────────────────────────────────────────────

test_that("fitPedigreeModel errors without OpenMx", {
  # This test is meaningful only when OpenMx is absent; skip otherwise.
  skip_if(requireNamespace("OpenMx", quietly = TRUE),
    message = "OpenMx is installed; skipping no-OpenMx error test"
  )
  expect_error(
    fitPedigreeModel(
      data = matrix(c(1, 2), nrow = 1, dimnames = list(NULL, c("y1", "y2")))
    ),
    regexp = "OpenMx"
  )

  expect_error(
    .require_openmx()
  )
})

test_that("fitPedigreeModel runs end-to-end with a trivial dataset", {
  skip_if_not_installed("OpenMx")
  set.seed(42)
  # Two families, each with 2 (simulated) observed scores
  dat <- matrix(
    c(0.1, -0.1, 0.2, -0.2),
    nrow = 2,
    dimnames = list(NULL, c("y1", "y2"))
  )
  Addmat <- make_add2()
  group_models <- buildFamilyGroups(
    dat = dat, obs_ids = c("y1", "y2"), Addmat = Addmat
  )
  vars <- list(
    ad2 = 0.4, dd2 = 0.1, cn2 = 0.1, ce2 = 0.1,
    mt2 = 0.05, am2 = 0.05, ee2 = 0.3
  )
  result <- expect_no_error(
    fitPedigreeModel(
      model_name   = "FitTest",
      vars         = vars,
      data         = dat,
      group_models = group_models,
      tryhard      = FALSE
    )
  )
  expect_true(inherits(result, "MxModel"))
})

test_that("fitPedigreeModel generates group_models from data and relatedness matrices", {
  skip_if_not_installed("OpenMx")
  set.seed(42)
  # Two families, each with 2 (simulated) observed scores
  dat <- matrix(
    c(0.1, -0.1, 0.2, -0.2),
    nrow = 2,
    dimnames = list(NULL, c("y1", "y2"))
  )
  Addmat <- make_add2()
  vars <- list(
    ad2 = 0.4, dd2 = 0.1, cn2 = 0.1, ce2 = 0.1,
    mt2 = 0.05, am2 = 0.05, ee2 = 0.3
  )
  result <- expect_no_error(
    fitPedigreeModel(
      model_name   = "FitTestAutoGroup",
      vars         = vars,
      data         = dat,
      group_models = NULL, # Will be auto-generated
      Addmat       = Addmat,
      tryhard      = FALSE
    )
  )
  expect_true(inherits(result, "MxModel"))
})

test_that("fitPedigreeModel errors when group_models and data are both NULL", {
  skip_if_not_installed("OpenMx")
  vars <- list(
    ad2 = 0.4, dd2 = 0.1, cn2 = 0.1, ce2 = 0.1,
    mt2 = 0.05, am2 = 0.05, ee2 = 0.3
  )
  expect_error(
    fitPedigreeModel(
      model_name   = "FitTest",
      vars         = vars,
      data         = NULL,
      group_models = NULL
    ),
    regexp = "Either 'group_models' or 'data' must be provided"
  )
})

# ─── alignPhenToMatrix ────────────────────────────────────────────────────────

test_that("alignPhenToMatrix returns a 1-row matrix with correct values", {
  ped <- data.frame(ID = c(1L, 2L, 3L), pheno = c(1.1, 2.2, 3.3))
  result <- alignPhenToMatrix(ped, phenotype = "pheno", keep_ids = c(1L, 2L, 3L))
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 1L)
  expect_equal(ncol(result), 3L)
  expect_equal(as.numeric(result), c(1.1, 2.2, 3.3))
})

test_that("alignPhenToMatrix subsets to only the requested IDs", {
  ped <- data.frame(ID = c(1L, 2L, 3L, 4L), pheno = c(10.0, 20.0, 30.0, 40.0))
  result <- alignPhenToMatrix(ped, phenotype = "pheno", keep_ids = c(2L, 4L))
  expect_equal(ncol(result), 2L)
  expect_equal(as.numeric(result), c(20.0, 40.0))
})

test_that("alignPhenToMatrix preserves the order of keep_ids", {
  ped <- data.frame(ID = c(1L, 2L, 3L), pheno = c(10.0, 20.0, 30.0))
  result <- alignPhenToMatrix(ped, phenotype = "pheno", keep_ids = c(3L, 1L, 2L))
  expect_equal(as.numeric(result), c(30.0, 10.0, 20.0))
})

test_that("alignPhenToMatrix column names are valid R names", {
  # IDs starting with a digit are not valid R names; make.names() should fix them
  ped <- data.frame(ID = c("1a", "2b"), pheno = c(5.5, 6.6))
  result <- alignPhenToMatrix(ped, phenotype = "pheno", keep_ids = c("1a", "2b"))
  expect_true(all(make.names(colnames(result)) == colnames(result)))
})

test_that("alignPhenToMatrix returns NA for IDs not present in the pedigree", {
  ped <- data.frame(ID = c(1L, 2L), pheno = c(1.0, 2.0))
  result <- alignPhenToMatrix(ped, phenotype = "pheno", keep_ids = c(1L, 99L))
  expect_equal(ncol(result), 2L)
  ref_mat <- matrix(c(1.0, NA), nrow = 1, dimnames = list(NULL, c("X1", "X99")))
  expect_equal(result[1, 1], ref_mat[1, 1])
  expect_true(is.na(result[1, 2]))
})

test_that("alignPhenToMatrix respects a custom personID column", {
  ped <- data.frame(personID = c("A", "B", "C"), score = c(7.0, 8.0, 9.0))
  result <- alignPhenToMatrix(ped,
    phenotype = "score",
    keep_ids = c("B", "C"),
    personID = "personID"
  )
  expect_equal(ncol(result), 2L)
  expect_equal(as.numeric(result), c(8.0, 9.0))
})

test_that("alignPhenToMatrix coerces phenotype values to double", {
  ped <- data.frame(ID = c(1L, 2L), pheno = c(1L, 2L)) # integer phenotype
  result <- alignPhenToMatrix(ped, phenotype = "pheno", keep_ids = c(1L, 2L))
  expect_true(is.double(result))
})
