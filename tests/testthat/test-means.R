test_that("buildOneTemporalFamilyGroup() works with and without ph", {

fsize <- 3
ids <- c("a", "b", "c")
A <- matrix(0.5, fsize, fsize); diag(A) <- 1
d <- as.data.frame(matrix(c(60, 62, 58), nrow = 1)); colnames(d) <- ids

for (ph in c(0, 1)) {
  H <- if (ph > 0) matrix(c(0, 1, 1), ncol = 1) else NULL
  g <- buildOneTemporalFamilyGroup(
    group_name = paste0("g", ph), Addmat = A,
    full_df_row = d, obs_ids = ids,
    param_year = c(-1, 0, 1), H = H,
    use_exp_loadings = FALSE, clean_ids = FALSE
  )
# cat("p_hist =", ph, "-> M formula: ", deparse(g$M$formula), "\n")
if(ph > 0){
    expect_equal(paste0(g$M$formula)[1], paste0("t"))
  expect_equal(paste0(g$M$formula)[2], paste0("Tpoly %*% ModelOne.B_mean + H %*% ModelOne.G_mean"))
}

if(ph == 0){
  expect_equal(paste0(g$M$formula)[1], paste0("t"))
  expect_equal(paste0(g$M$formula)[2], paste0("Tpoly %*% ModelOne.B_mean"))
}
}

m <- .pedigreeMeanPart(fsize = fsize, obs_ids = ids, label = "meanLI", mean_basis = NULL)

expect_equal(class(m)[1],"FullMatrix")
expect_equal(length(m$labels),fsize)

})

# ─── buildPedigreeModelCovariance(temporal = TRUE): default is unchanged ────

test_that("G_mean is fixed at zero by default (back-compatible)", {
  skip_if_not_installed("OpenMx")
  mod <- buildPedigreeModelCovariance(
    temporal = TRUE, components = c("a", "e"), p_hist = 2
  )
  expect_false(is.null(mod$G_mean))
  expect_true(all(!mod$G_mean$free))
  expect_true(all(mod$G_mean$values == 0))
})

test_that("B_mean frees only the intercept when mean_degree = 0 (default)", {
  skip_if_not_installed("OpenMx")
  mod <- buildPedigreeModelCovariance(
    temporal = TRUE, components = c("a", "e"), time_point_max = 3
  )
  expect_equal(as.vector(mod$B_mean$free), c(TRUE, FALSE, FALSE, FALSE))
})

# ─── mean_hist_free frees G_mean ─────────────────────────────────────────────

test_that("mean_hist_free = TRUE frees every historical mean coefficient", {
  skip_if_not_installed("OpenMx")
  mod <- buildPedigreeModelCovariance(
    temporal = TRUE, components = c("a", "e"), p_hist = 3, mean_hist_free = TRUE
  )
  expect_true(all(mod$G_mean$free))
  expect_length(mod$G_mean$free, 3)
})

test_that("mean_hist_free accepts a per-moderator logical vector", {
  skip_if_not_installed("OpenMx")
  mod <- buildPedigreeModelCovariance(
    temporal = TRUE, components = c("a", "e"), p_hist = 3,
    mean_hist_free = c(TRUE, FALSE, TRUE)
  )
  expect_equal(as.vector(mod$G_mean$free), c(TRUE, FALSE, TRUE))
})

test_that("start_mean_hist sets the starting value of freed G_mean elements", {
  skip_if_not_installed("OpenMx")
  mod <- buildPedigreeModelCovariance(
    temporal = TRUE, components = c("a", "e"), p_hist = 2,
    mean_hist_free = TRUE, start_mean_hist = -1.5
  )
  expect_true(all(mod$G_mean$values == -1.5))
})

test_that("mean_hist_free does not disturb the G_mean labels", {
  skip_if_not_installed("OpenMx")
  mod <- buildPedigreeModelCovariance(
    temporal = TRUE, components = c("a", "e"), p_hist = 2, mean_hist_free = TRUE
  )
  expect_equal(as.vector(mod$G_mean$labels), c("g_mean_1", "g_mean_2"))
})

test_that("mean_degree still controls how much of the mean polynomial is free", {
  skip_if_not_installed("OpenMx")
  mod <- buildPedigreeModelCovariance(
    temporal = TRUE, components = c("a", "e"), p_hist = 1,
    mean_degree = 3, mean_hist_free = TRUE, time_point_max = 3
  )
  expect_true(all(mod$B_mean$free))
  expect_equal(mod$B_mean$labels[1], "mean_y")
})

# ─── validation ───────────────────────────────────────────────────────────────

test_that("mean_hist_free rejects a non-logical or missing input", {
  skip_if_not_installed("OpenMx")
  expect_error(
    buildPedigreeModelCovariance(temporal = TRUE, p_hist = 2, mean_hist_free = 1),
    "logical"
  )
  expect_error(
    buildPedigreeModelCovariance(temporal = TRUE, p_hist = 2, mean_hist_free = NA),
    "logical"
  )
})

test_that("mean_hist_free rejects a length mismatched against p_hist", {
  skip_if_not_installed("OpenMx")
  expect_error(
    buildPedigreeModelCovariance(
      temporal = TRUE, p_hist = 3, mean_hist_free = c(TRUE, FALSE)
    ),
    "length"
  )
})

test_that("mean_degree validates as a non-negative whole number", {
  skip_if_not_installed("OpenMx")
  expect_error(
    buildPedigreeModelCovariance(temporal = TRUE, mean_degree = -1),
    "non-negative"
  )
  expect_error(
    buildPedigreeModelCovariance(temporal = TRUE, mean_degree = 1.5),
    "non-negative"
  )
  expect_error(
    buildPedigreeModelCovariance(temporal = TRUE, mean_degree = 5, time_point_max = 3),
    "time_point_max"
  )
})

test_that("mean_hist_free with p_hist = 0 warns and is ignored", {
  skip_if_not_installed("OpenMx")
  mod <- expect_warning(
    buildPedigreeModelCovariance(
      temporal = TRUE, components = c("a", "e"), p_hist = 0, mean_hist_free = TRUE
    ),
    "p_hist"
  )
  expect_null(mod$G_mean)
})

# ─── threading: the flag reaches G_mean through every public entry point ────

test_that("buildPedigreeMx(temporal = TRUE) threads mean_hist_free to ModelOne$G_mean", {
  skip_if_not_installed("OpenMx")
  # buildPedigreeMx() assembles an mxFitFunctionMultigroup over group_models, so
  # it needs at least one real family group with a fit function -- an empty list
  # fails before ModelOne is ever reached. Defined locally rather than reusing
  # make_add3()/make_dat3() from test-buildmxPedigrees-temporal.R: testthat runs
  # test files independently, so top-level helpers in one file are not
  # guaranteed visible from another.
  add3 <- matrix(c(
    1, 0.5, 0.25,
    0.5, 1, 0.125,
    0.25, 0.125, 1
  ), nrow = 3)
  dat3 <- matrix(c(0.1, -0.2, 0.3), nrow = 1, dimnames = list(NULL, c("y1", "y2", "y3")))

  grp <- buildOneTemporalFamilyGroup(
    group_name = "g1", Addmat = add3,
    full_df_row = dat3, obs_ids = c("y1", "y2", "y3"),
    param_year = c(-1, 0, 1), use_exp_loadings = FALSE, clean_ids = FALSE
  )
  mx <- buildPedigreeMx(model_name = "a",
    temporal = TRUE, components = c("a", "e"), p_hist = 2,
    group_models = list(grp), mean_hist_free = TRUE
  )
  one <- if ("ModelOne" %in% names(mx)) mx$ModelOne else mx
  expect_true(all(one$G_mean$free))
})

test_that("buildTemporalPedigreeModelCovariance() threads mean_hist_free", {
  skip_if_not_installed("OpenMx")
  mod <- buildTemporalPedigreeModelCovariance(
    components = c("a", "e"), p_hist = 2, mean_hist_free = TRUE
  )
  expect_true(all(mod$G_mean$free))
})

test_that("fitTemporalPedigreeModel() exposes mean_degree, start_mean, mean_hist_free, and start_mean_hist", {
  # This is a signature check, not a fit: fitTemporalPedigreeModel() previously
  # exposed none of these four arguments, so the mean structure was unreachable
  # from the top-level fit function regardless of what buildPedigreeModelCovariance()
  # supported. A user driving the model this way could not free G_mean at all.
  fmls <- names(formals(fitTemporalPedigreeModel))
  expect_true(all(c("mean_degree", "start_mean", "mean_hist_free", "start_mean_hist") %in% fmls))
})
