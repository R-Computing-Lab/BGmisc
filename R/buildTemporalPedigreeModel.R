# Temporal pedigree model builders
#
# These are thin, temporal-flavored wrappers around the generalized,
# temporal=-aware functions in R/buildmxPedigrees.R (buildPedigreeModelCovariance,
# buildOneFamilyGroup, buildFamilyGroups, buildFamilyGroups_list, buildPedigreeMx,
# fitPedigreeModel). They carry no independent model-building logic; they exist so
# that the temporal-specific argument names (param_year_list, H_list, p_hist,
# components, ...) have a discoverable, temporal-only entry point.

#' Build a Temporal Covariance Sub-model
#'
#' Wrapper around \code{\link{buildPedigreeModelCovariance}} with \code{temporal = TRUE}.
#'
#' @inheritParams buildPedigreeModelCovariance
#' @param p_hist Integer. Number of historical moderator columns.
#' @return An OpenMx model containing the \code{B_*}/\code{G_*} parameter matrices.
#' @export
buildTemporalPedigreeModelCovariance <- function(
  p_hist,
  components = c("a", "d", "cn", "ce", "mt", "e"),
  start_beta0 = 0.5,
  start_beta_time = 0,
  start_gamma = 0,
  time_point_max = NULL,
  mean_degree = 0,
  start_mean = 0
) {
  buildPedigreeModelCovariance(
    temporal = TRUE,
    p_hist = p_hist,
    components = components,
    start_beta0 = start_beta0,
    start_beta_time = start_beta_time,
    start_gamma = start_gamma,
    time_point_max = time_point_max,
    mean_degree = mean_degree,
    start_mean = start_mean
  )
}

#' Build One Temporal Family Group Model
#'
#' Wrapper around \code{\link{buildOneFamilyGroup}} with \code{temporal = TRUE}.
#'
#' @inheritParams buildOneFamilyGroup
#' @return An OpenMx model for the specified family group.
#' @export
buildOneTemporalFamilyGroup <- function(
  group_name,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  full_df_row,
  obs_ids,
  param_year,
  H = NULL,
  use_exp_loadings = TRUE,
  condenseMatrixSlots = TRUE,
  time_point_max = NULL,
  clean_ids = FALSE,
  retain_eta = TRUE,
  retain_loadings = TRUE,
  retain_loading_covariances = TRUE,
  retain_component_covariances = TRUE,
  residual_covariance_form = c("outer_product", "diagonal")
) {
  buildOneFamilyGroup(
    group_name = group_name,
    Addmat = Addmat,
    Nucmat = Nucmat,
    Extmat = Extmat,
    Mtdmat = Mtdmat,
    Amimat = Amimat,
    Dmgmat = Dmgmat,
    full_df_row = full_df_row,
    obs_ids = obs_ids,
    condenseMatrixSlots = condenseMatrixSlots,
    temporal = TRUE,
    param_year = param_year,
    H = H,
    use_exp_loadings = use_exp_loadings,
    time_point_max = time_point_max,
    retain_eta = retain_eta,
    retain_loadings = retain_loadings,
    retain_loading_covariances = retain_loading_covariances,
    retain_component_covariances = retain_component_covariances,
    residual_covariance_form = residual_covariance_form,
    clean_ids = clean_ids
  )
}

#' Build Temporal Family Group Models
#'
#' Wrapper around \code{\link{buildFamilyGroups}} with \code{temporal = TRUE}. All families
#' share the same relatedness matrices; only phenotype data, birth year, and historical
#' moderators vary by family. For families with different structure or size, see
#' \code{\link{buildTemporalFamilyGroups_list}}.
#'
#' @inheritParams buildFamilyGroups
#' @return A list of OpenMx models for each family group.
#' @export
buildTemporalFamilyGroups <- function(
  dat,
  obs_ids,
  param_year_list,
  H_list = NULL,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  prefix = "fam",
  use_exp_loadings = FALSE,
  condenseMatrixSlots = TRUE,
  time_point_max = NULL,
  retain_eta = TRUE,
  retain_loadings = TRUE,
  retain_loading_covariances = TRUE,
  retain_component_covariances = TRUE,
  residual_covariance_form = c("outer_product", "diagonal"),
  clean_ids = FALSE
) {
  buildFamilyGroups(
    dat = dat,
    obs_ids = obs_ids,
    Addmat = Addmat,
    Nucmat = Nucmat,
    Extmat = Extmat,
    Mtdmat = Mtdmat,
    Amimat = Amimat,
    Dmgmat = Dmgmat,
    prefix = prefix,
    condenseMatrixSlots = condenseMatrixSlots,
    temporal = TRUE,
    param_year_list = param_year_list,
    H_list = H_list,
    use_exp_loadings = use_exp_loadings,
    time_point_max = time_point_max,
    retain_eta = retain_eta,
    retain_loadings = retain_loadings,
    retain_loading_covariances = retain_loading_covariances,
    retain_component_covariances = retain_component_covariances,
    residual_covariance_form = residual_covariance_form,
    clean_ids = clean_ids
  )
}

#' Build Temporal Family Group Models with Per-Family Relatedness Matrices
#'
#' Wrapper around \code{\link{buildFamilyGroups_list}} with \code{temporal = TRUE}. Use this
#' when families vary in size or structure.
#'
#' @inheritParams buildFamilyGroups_list
#' @return A list of OpenMx models for each family group.
#' @export
buildTemporalFamilyGroups_list <- function(
  dat_list,
  obs_ids_list,
  param_year_list,
  H_list,
  Addmat_list = NULL,
  Nucmat_list = NULL,
  Extmat_list = NULL,
  Mtdmat_list = NULL,
  Amimat_list = NULL,
  Dmgmat_list = NULL,
  prefix = "fam",
  use_exp_loadings = FALSE,
  condenseMatrixSlots = TRUE,
  time_point_max = NULL,
  clean_ids = FALSE
) {
  buildFamilyGroups_list(
    dat_list = dat_list,
    obs_ids_list = obs_ids_list,
    Addmat_list = Addmat_list,
    Nucmat_list = Nucmat_list,
    Extmat_list = Extmat_list,
    Mtdmat_list = Mtdmat_list,
    Amimat_list = Amimat_list,
    Dmgmat_list = Dmgmat_list,
    prefix = prefix,
    condenseMatrixSlots = condenseMatrixSlots,
    temporal = TRUE,
    param_year_list = param_year_list,
    H_list = H_list,
    use_exp_loadings = use_exp_loadings,
    time_point_max = time_point_max,
    clean_ids = clean_ids
  )
}

#' Build a Temporal Pedigree mxModel
#'
#' Wrapper around \code{\link{buildPedigreeMx}} with \code{temporal = TRUE}.
#'
#' @inheritParams buildPedigreeMx
#' @param p_hist Integer. Number of historical moderator columns.
#' @return An OpenMx pedigree model combining the temporal covariance sub-model and family groups.
#' @export
buildTemporalPedigreeMx <- function(
  model_name,
  group_models,
  p_hist,
  components = c("a", "e"),
  ci = FALSE,
  time_point_max = NULL,
  mean_degree = 0,
  start_mean = 0
) {
  buildPedigreeMx(
    model_name = model_name,
    group_models = group_models,
    ci = ci,
    temporal = TRUE,
    p_hist = p_hist,
    components = components,
    time_point_max = time_point_max,
    mean_degree = mean_degree,
    start_mean = start_mean
  )
}

#' Fit a Temporal OpenMx Pedigree Model to Observed Data
#'
#' Wrapper around \code{\link{fitPedigreeModel}} with \code{temporal = TRUE}, exposing only the
#' temporal-relevant arguments.
#'
#' @inheritParams fitPedigreeModel
#' @return A fitted OpenMx model.
#' @export
fitTemporalPedigreeModel <- function(
  model_name = "TemporalPedigreeModel",
  group_models = NULL,
  dat_list = NULL,
  obs_ids_list = NULL,
  param_year_list = NULL,
  H_list = NULL,
  Addmat_list = NULL,
  Nucmat_list = NULL,
  Extmat_list = NULL,
  Mtdmat_list = NULL,
  Amimat_list = NULL,
  Dmgmat_list = NULL,
  p_hist = NULL,
  components = c("a", "d", "cn", "ce", "mt", "e"),
  use_exp_loadings = FALSE,
  tryhard = TRUE,
  intervals = TRUE,
  extraTries = 10,
  runmodel = TRUE,
  time_point_max = NULL,
  clean_ids = FALSE
) {
  fitPedigreeModel(
    model_name = model_name,
    group_models = group_models,
    temporal = TRUE,
    dat_list = dat_list,
    obs_ids_list = obs_ids_list,
    param_year_list = param_year_list,
    H_list = H_list,
    Addmat_list = Addmat_list,
    Nucmat_list = Nucmat_list,
    Extmat_list = Extmat_list,
    Mtdmat_list = Mtdmat_list,
    Amimat_list = Amimat_list,
    Dmgmat_list = Dmgmat_list,
    p_hist = p_hist,
    components = components,
    use_exp_loadings = use_exp_loadings,
    tryhard = tryhard,
    intervals = intervals,
    extraTries = extraTries,
    runmodel = runmodel,
    time_point_max = time_point_max,
    clean_ids = clean_ids
  )
}
