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
  # # Extmat signals "include Vce"; the algebra always uses U (unit matrix)
  expect_false(is.null(mod$U))
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

# ─── alignPhenToOrdinal ─────────────────────────────────────────────────────

test_that("alignPhenToOrdinal returns a 1-row data.frame of ordered factors", {
  ped <- data.frame(ID = c(1L, 2L, 3L), pheno = c(0, 1, 0))
  result <- alignPhenToOrdinal(ped, phenotype = "pheno", keep_ids = c(1L, 2L, 3L), levels = c(0, 1))
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1L)
  expect_equal(ncol(result), 3L)
  expect_true(all(vapply(result, is.ordered, logical(1))))
})

test_that("alignPhenToOrdinal preserves order of keep_ids", {
  ped <- data.frame(ID = c(1L, 2L, 3L), pheno = c(0, 1, 0))
  result <- alignPhenToOrdinal(ped, phenotype = "pheno", keep_ids = c(3L, 1L), levels = c(0, 1))
  expect_equal(ncol(result), 2L)
  expect_equal(as.character(result[[1]]), "0")
  expect_equal(as.character(result[[2]]), "0")
})

test_that("alignPhenToOrdinal respects custom personID", {
  ped <- data.frame(pid = c("A", "B"), trait = c(1, 0))
  result <- alignPhenToOrdinal(ped, phenotype = "trait", keep_ids = c("A", "B"),
                               levels = c(0, 1), personID = "pid")
  expect_equal(as.character(result[[1]]), "1")
  expect_equal(as.character(result[[2]]), "0")
})

test_that("alignPhenToOrdinal handles multi-level ordinal data", {
  ped <- data.frame(ID = c(1L, 2L, 3L), severity = c("mild", "severe", "moderate"))
  result <- alignPhenToOrdinal(ped, phenotype = "severity", keep_ids = c(1L, 2L, 3L),
                               levels = c("mild", "moderate", "severe"))
  expect_true(all(vapply(result, is.ordered, logical(1))))
  expect_equal(levels(result[[1]]), c("mild", "moderate", "severe"))
})

test_that("alignPhenToOrdinal returns NA factor for missing IDs", {
  ped <- data.frame(ID = c(1L, 2L), pheno = c(0, 1))
  result <- alignPhenToOrdinal(ped, phenotype = "pheno", keep_ids = c(1L, 99L), levels = c(0, 1))
  expect_true(is.na(result[[2]]))
})

# ─── buildOneFamilyGroup: binary ────────────────────────────────────────────

# Helper: a 1-row binary data.frame for 2 people
make_binary_dat2 <- function(obs_ids = c("y1", "y2")) {
  data.frame(
    y1 = ordered(1, levels = c(0, 1)),
    y2 = ordered(0, levels = c(0, 1))
  )
}

test_that("buildOneFamilyGroup builds a binary (threshold) model", {
  skip_if_not_installed("OpenMx")
  Addmat <- make_add2()
  dat <- make_binary_dat2()
  mod <- expect_no_error(
    buildOneFamilyGroup(
      group_name = "fam_bin",
      Addmat = Addmat,
      full_df_row = dat,
      obs_ids = c("y1", "y2"),
      type = "binary"
    )
  )
  expect_true(inherits(mod, "MxModel"))
  # Should have threshold matrix Th, correlation algebra R, and standardization iSD
  expect_false(is.null(mod$Th))
  expect_false(is.null(mod$R))
  expect_false(is.null(mod$iSD))
  # Binary: always 1 threshold row
  expect_equal(nrow(mod$Th$values), 1L)
})

test_that("buildOneFamilyGroup binary model has fixed means at zero", {
  skip_if_not_installed("OpenMx")
  Addmat <- make_add2()
  dat <- make_binary_dat2()
  mod <- buildOneFamilyGroup(
    group_name = "fam_bin_mean",
    Addmat = Addmat,
    full_df_row = dat,
    obs_ids = c("y1", "y2"),
    type = "binary"
  )
  # Means should be fixed (free = FALSE) and set to 0
  expect_true(all(mod$M$free == FALSE))
  expect_true(all(mod$M$values == 0))
})

test_that("buildOneFamilyGroup ordinal model supports multiple thresholds", {
  skip_if_not_installed("OpenMx")
  Addmat <- make_add2()
  dat <- data.frame(
    y1 = ordered(2, levels = c(0, 1, 2)),
    y2 = ordered(1, levels = c(0, 1, 2))
  )
  mod <- expect_no_error(
    buildOneFamilyGroup(
      group_name = "fam_ord",
      Addmat = Addmat,
      full_df_row = dat,
      obs_ids = c("y1", "y2"),
      type = "ordinal",
      nthresh = 2
    )
  )
  expect_equal(nrow(mod$Th$values), 2L)
  # Threshold values should be monotonically increasing
  expect_true(mod$Th$values[1, 1] < mod$Th$values[2, 1])
})

test_that("buildOneFamilyGroup binary uses equated threshold labels by default", {
  skip_if_not_installed("OpenMx")
  Addmat <- make_add2()
  dat <- make_binary_dat2()
  mod <- buildOneFamilyGroup(
    group_name = "fam_eq",
    Addmat = Addmat,
    full_df_row = dat,
    obs_ids = c("y1", "y2"),
    type = "binary"
  )
  # equate_thresholds = TRUE by default: labels should end with _eq
  expect_true(all(grepl("_eq$", mod$Th$labels)))
})

test_that("buildOneFamilyGroup binary with equate_thresholds = FALSE uses non-equated labels", {
  skip_if_not_installed("OpenMx")
  Addmat <- make_add2()
  dat <- make_binary_dat2()
  mod <- buildOneFamilyGroup(
    group_name = "fam_noeq",
    Addmat = Addmat,
    full_df_row = dat,
    obs_ids = c("y1", "y2"),
    type = "binary",
    equate_thresholds = FALSE
  )
  expect_false(any(grepl("_eq$", mod$Th$labels)))
})

test_that("buildOneFamilyGroup binary accepts custom thresh_start", {
  skip_if_not_installed("OpenMx")
  Addmat <- make_add2()
  dat <- make_binary_dat2()
  mod <- buildOneFamilyGroup(
    group_name = "fam_ts",
    Addmat = Addmat,
    full_df_row = dat,
    obs_ids = c("y1", "y2"),
    type = "binary",
    thresh_start = 0.5
  )
  expect_equal(as.numeric(mod$Th$values[1, 1]), 0.5)
})

test_that("buildOneFamilyGroup ordinal accepts a vector of thresh_start values", {
  skip_if_not_installed("OpenMx")
  Addmat <- make_add2()
  dat <- data.frame(
    y1 = ordered(2, levels = c(0, 1, 2)),
    y2 = ordered(0, levels = c(0, 1, 2))
  )
  mod <- buildOneFamilyGroup(
    group_name = "fam_vec_ts",
    Addmat = Addmat,
    full_df_row = dat,
    obs_ids = c("y1", "y2"),
    type = "ordinal",
    nthresh = 2,
    thresh_start = c(-0.5, 0.5)
  )
  expect_equal(as.numeric(mod$Th$values[1, 1]), -0.5)
  expect_equal(as.numeric(mod$Th$values[2, 1]), 0.5)
})

# ─── buildFamilyGroups: binary ──────────────────────────────────────────────

test_that("buildFamilyGroups builds binary family groups from ordinal data frame", {
  skip_if_not_installed("OpenMx")
  Addmat <- make_add2()
  dat <- data.frame(
    y1 = ordered(c(1, 0), levels = c(0, 1)),
    y2 = ordered(c(0, 1), levels = c(0, 1))
  )
  groups <- expect_no_error(
    buildFamilyGroups(
      dat = dat, obs_ids = c("y1", "y2"),
      Addmat = Addmat, type = "binary"
    )
  )
  expect_equal(length(groups), 2L)
  # Each group should have threshold structure
  expect_false(is.null(groups[[1]]$Th))
  expect_false(is.null(groups[[2]]$Th))
})

# ─── fitPedigreeModel: binary end-to-end with real pedigree ─────────────────

test_that("fitPedigreeModel fits a binary threshold model from hazard pedigree", {
  skip_if_not_installed("OpenMx")

  data(hazard, package = "BGmisc")

  # Process both families: compute relatedness, align binary phenotype
  fam_ids <- unique(hazard$famID)
  group_models <- list()

  for (i in seq_along(fam_ids)) {
    fam_i <- subset(hazard, famID == fam_ids[i])

    add_i <- ped2add(fam_i, sparse = FALSE,
      famID = "famID", personID = "ID",
      momID = "momID", dadID = "dadID", sex = "sex"
    )
    cn_i <- ped2cn(fam_i, sparse = FALSE,
      famID = "famID", personID = "ID",
      momID = "momID", dadID = "dadID", sex = "sex"
    )

    id_order <- rownames(add_i)
    pheno_vals <- fam_i$DA2[match(id_order, as.character(fam_i$ID))]
    observed <- !is.na(pheno_vals)

    raw_obs_i <- id_order[observed]
    obs_ids_i <- make.names(raw_obs_i)

    # Subset matrices to observed individuals
    add_obs <- add_i[raw_obs_i, raw_obs_i]
    cn_obs <- cn_i[raw_obs_i, raw_obs_i]
    rownames(add_obs) <- colnames(add_obs) <- obs_ids_i
    rownames(cn_obs) <- colnames(cn_obs) <- obs_ids_i

    # Build ordered-factor phenotype data for threshold model
    pheno_df <- alignPhenToOrdinal(
      fam_i, phenotype = "DA2", keep_ids = as.integer(raw_obs_i),
      levels = c(0, 1), personID = "ID"
    )

    group_models[[i]] <- buildOneFamilyGroup(
      group_name = paste0("fam", fam_ids[i]),
      Addmat     = add_obs,
      Nucmat     = cn_obs,
      full_df_row = pheno_df,
      obs_ids    = obs_ids_i,
      type       = "binary"
    )
  }

  vars <- list(
    ad2 = 0.3, dd2 = 0, cn2 = 0.1, ce2 = 0,
    mt2 = 0, am2 = 0, ee2 = 0.5
  )

  result <- expect_no_error(
    fitPedigreeModel(
      model_name   = "HazardBinary",
      vars         = vars,
      group_models = group_models,
      tryhard      = TRUE
    )
  )
  expect_true(inherits(result, "MxModel"))
  # Variance components should be finite and positive
  vad_est <- result$ModelOne$Vad$values[1, 1]
  ver_est <- result$ModelOne$Ver$values[1, 1]
  expect_true(is.finite(vad_est))
  expect_true(is.finite(ver_est))
  expect_true(vad_est > 0)
  expect_true(ver_est > 0)
})

# ─── fitPedigreeModel: ordinal end-to-end with simulated pedigrees ─────────

test_that("fitPedigreeModel fits an ordinal threshold model from simulated pedigrees", {
  skip_if_not_installed("OpenMx")
  skip_if_not_installed("mvtnorm")
  set.seed(2024)

  # Use small pedigrees (2 kids, 3 generations) so ordinal blocks stay
  # under the maxOrdinalPerBlock limit and optimization converges
  n_families <- 5
  group_models <- list()

  for (i in seq_len(n_families)) {
    ped_i <- simulatePedigree(kpc = 2, Ngen = 3, marR = 0.5)

    A_i <- as.matrix(ped2add(ped_i, sparse = FALSE))
    Cn_i <- as.matrix(ped2cn(ped_i, sparse = FALSE))
    n_i <- nrow(A_i)

    # Simulate a latent liability and cut into 3 ordinal categories
    V_i <- 0.4 * A_i + 0.1 * Cn_i + 0.5 * diag(1, n_i)
    y_latent <- mvtnorm::rmvnorm(1, sigma = V_i)[1, ]
    y_ord <- cut(y_latent, breaks = c(-Inf, -0.5, 0.5, Inf), labels = c(0, 1, 2))

    ids_i <- make.names(rownames(A_i))
    rownames(A_i) <- colnames(A_i) <- ids_i
    rownames(Cn_i) <- colnames(Cn_i) <- ids_i

    pheno_df <- as.data.frame(
      setNames(
        lapply(as.character(y_ord), function(x) ordered(x, levels = c(0, 1, 2))),
        ids_i
      ),
      stringsAsFactors = FALSE
    )

    group_models[[i]] <- buildOneFamilyGroup(
      group_name  = paste0("ped", i),
      Addmat      = A_i,
      Nucmat      = Cn_i,
      full_df_row = pheno_df,
      obs_ids     = ids_i,
      type        = "ordinal",
      nthresh     = 2
    )
  }

  vars <- list(
    ad2 = 0.3, dd2 = 0, cn2 = 0.1, ce2 = 0,
    mt2 = 0, am2 = 0, ee2 = 0.5
  )

  result <- expect_no_error(
    fitPedigreeModel(
      model_name   = "OrdinalSimPedigree",
      vars         = vars,
      group_models = group_models,
      tryhard      = TRUE
    )
  )
  expect_true(inherits(result, "MxModel"))
  # Estimated variance components should be finite and positive
  vad_est <- result$ModelOne$Vad$values[1, 1]
  ver_est <- result$ModelOne$Ver$values[1, 1]
  expect_true(is.finite(vad_est))
  expect_true(is.finite(ver_est))
  expect_true(vad_est > 0)
  expect_true(ver_est > 0)
})
