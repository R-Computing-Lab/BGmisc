# Fit an OpenMx pedigree model to observed data

This function constructs and fits an OpenMx model for a pedigree using
specified variance components and family group models.

## Usage

``` r
fitPedigreeModel(
  model_name = "PedigreeModel",
  vars = list(ad2 = 0.5, dd2 = 0.3, cn2 = 0.2, ce2 = 0.4, mt2 = 0.1, am2 = 0.25, ee2 =
    0.6),
  data = NULL,
  group_models = NULL,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  tryhard = TRUE,
  intervals = TRUE,
  extraTries = 10,
  condenseMatrixSlots = TRUE,
  runmodel = TRUE,
  temporal = FALSE,
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
  components = c("a", "d", "cn", "ce", "mt", "am", "e"),
  use_exp_loadings = FALSE,
  time_point_max = NULL,
  start_beta0 = 0.5,
  start_beta_time = 0,
  start_gamma = 0,
  mean_degree = 0,
  start_mean = 0,
  mean_hist_free = FALSE,
  start_mean_hist = 0,
  retain_eta = TRUE,
  retain_loadings = TRUE,
  retain_loading_covariances = TRUE,
  retain_component_covariances = TRUE,
  residual_covariance_form = c("outer_product", "diagonal"),
  clean_ids = TRUE,
  group_static_families = FALSE
)
```

## Arguments

- model_name:

  Character. Name for the overall OpenMx model. Default is
  "PedigreeModel".

- vars:

  A named list or vector of initial variance component values. Only used
  when `temporal = FALSE`.

- data:

  A matrix or data frame of observed data, where each row is a family
  and columns correspond to individuals. Only used when `group_models`
  is NULL and `temporal = FALSE`.

- group_models:

  Optional list of pre-built OpenMx family group models (from
  [`buildOneFamilyGroup`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md)).
  If NULL, they are generated from `data` (or, when `temporal = TRUE`,
  from `dat_list` and friends) using the provided relatedness matrices.

- Addmat:

  Additive genetic relatedness matrix (from
  [`ped2add`](https://r-computing-lab.github.io/BGmisc/reference/ped2add.md)).

- Nucmat:

  Nuclear family shared environment relatedness matrix (from
  [`ped2cn`](https://r-computing-lab.github.io/BGmisc/reference/ped2cn.md)).

- Extmat:

  Common extended family environment relatedness matrix. When non-NULL,
  a Vce term scaled by this matrix is added to the covariance. If a
  non-matrix value (e.g. `TRUE`) is supplied, a unit matrix (all members
  share equally) is created automatically.

- Mtdmat:

  Mitochondrial genetic relatedness matrix (from
  [`ped2mit`](https://r-computing-lab.github.io/BGmisc/reference/ped2mit.md)).

- Amimat:

  Additive by mitochondrial interaction relatedness matrix.

- Dmgmat:

  Dominance genetic relatedness matrix.

- tryhard:

  Logical. If TRUE (default), use `mxTryHard` for robust optimization;
  if FALSE, use `mxRun`.

- intervals:

  Logical. If TRUE (default), compute confidence intervals for the
  parameters using `mxSE` and `mxCI`.

- extraTries:

  Numeric. The number of extra optimization attempts to make when
  `tryhard` is TRUE. Default is 10.

- condenseMatrixSlots:

  Logical. If TRUE, use the mxCondenseMatrixSlots wrapper to optimize
  memory usage for large matrices. Default is TRUE.

- runmodel:

  Logical. If TRUE (default), the model is fitted; if FALSE, the model
  is returned without fitting.

- temporal:

  Logical. If TRUE, build a time-varying family-group model where each
  component's weight is a function of `param_year` (and, optionally,
  `H`) instead of a fixed scalar. Default is FALSE.

- dat_list:

  A list of numeric vectors of observed data, one per family. Only used
  when `temporal = TRUE` and `group_models` is NULL.

- obs_ids_list:

  A list of character vectors of individual IDs, one per family. Only
  used when `temporal = TRUE` and `group_models` is NULL.

- param_year_list:

  A list of numeric birth-year vectors, one per family. Only used when
  `temporal = TRUE` and `group_models` is NULL.

- H_list:

  A list of historical-moderator matrices, one per family. Only used
  when `temporal = TRUE`.

- Addmat_list:

  A list of additive genetic relatedness matrices, one per family. Only
  used when `temporal = TRUE` and `group_models` is NULL.

- Nucmat_list:

  A list of nuclear family shared environment relatedness matrices, one
  per family. Only used when `temporal = TRUE` and `group_models` is
  NULL.

- Extmat_list:

  A list of common extended family environment relatedness matrices, one
  per family. Only used when `temporal = TRUE` and `group_models` is
  NULL.

- Mtdmat_list:

  A list of mitochondrial genetic relatedness matrices, one per family.
  Only used when `temporal = TRUE` and `group_models` is NULL.

- Amimat_list:

  A list of additive by mitochondrial interaction relatedness matrices,
  one per family. Only used when `temporal = TRUE` and `group_models` is
  NULL.

- Dmgmat_list:

  A list of dominance genetic relatedness matrices, one per family. Only
  used when `temporal = TRUE` and `group_models` is NULL.

- p_hist:

  Integer. Number of historical moderator columns. Only used when
  `temporal = TRUE`. If NULL, inferred from `H_list`.

- components:

  Character vector of component keys to include (any of "a", "d", "cn",
  "ce", "mt", "am", "e"). Only used when `temporal = TRUE`. Default is
  `c("a", "e")`.

- use_exp_loadings:

  Logical. If TRUE (default), each component's loading is exponentiated
  so its implied variance stays positive. Only used when
  `temporal = TRUE`.

- time_point_max:

  Integer degree of the polynomial birth-year basis. Only used when
  `temporal = TRUE`. Default is 3.

- start_beta0:

  Numeric. Starting value for each component's intercept loading. Only
  used when `temporal = TRUE`. Default is 0.5.

- start_beta_time:

  Numeric. Starting value for each component's time-slope loadings. Only
  used when `temporal = TRUE`. Default is 0.

- start_gamma:

  Numeric. Starting value for historical-moderator loadings. Only used
  when `temporal = TRUE`. Default is 0.

- mean_degree:

  Integer. Degree of the polynomial temporal mean. The default, 0,
  estimates a constant mean. Must not exceed `time_point_max`.

- start_mean:

  Numeric. Starting value for the mean's intercept. Set this near the
  observed phenotype mean.

- mean_hist_free:

  Logical, length 1 or `p_hist`. Whether historical moderators can shift
  the phenotypic mean. Defaults to FALSE, fixing those shifts at zero.

- start_mean_hist:

  Numeric. Starting value for the free elements of `G_mean`.

- retain_eta:

  Logical. Retain named temporal linear predictors `Eta_*`.

- retain_loadings:

  Logical. Retain named loading vectors `L_*`.

- retain_loading_covariances:

  Logical. Retain named loading outer products `K*`.

- retain_component_covariances:

  Logical. Retain named relatedness-weighted component covariance
  algebras `Cov_*`. Any unretained layer is inlined, leaving `V` and the
  likelihood unchanged.

- residual_covariance_form:

  Character. Use `"outer_product"` to preserve the original `I * K_e`
  representation or `"diagonal"` for the exactly equivalent
  `vec2diag(L_e * L_e)` representation.

- clean_ids:

  Logical. If TRUE, clean the `obs_ids` to be syntactically valid R
  names using `make.names`. Default is FALSE.

- group_static_families:

  Logical. If TRUE for a static model built from `data`, build one
  raw-data submodel containing all family rows. If FALSE, preserve the
  original one-submodel-per-family implementation.

## Value

A fitted OpenMx model.
