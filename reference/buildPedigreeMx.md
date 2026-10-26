# Build Pedigree mxModel

This function constructs an OpenMx pedigree model by combining variance
component parameters and family group models. For static
(`temporal = FALSE`) models, it auto-detects which variance components
are referenced in the group algebras and creates only those parameters.

## Usage

``` r
buildPedigreeMx(
  model_name,
  vars,
  group_models,
  ci = FALSE,
  condenseMatrixSlots = TRUE,
  temporal = FALSE,
  p_hist = 0,
  param_year = NULL,
  components = c("a", "e"),
  ci_names = NULL,
  time_point_max = NULL,
  start_beta0 = 0.5,
  start_beta_time = 0,
  start_gamma = 0,
  mean_degree = 0,
  start_mean = 0,
  mean_hist_free = FALSE,
  start_mean_hist = 0
)
```

## Arguments

- model_name:

  Name of the overall pedigree model.

- vars:

  A named list or vector of initial variance component values. Only used
  when `temporal = FALSE`.

- group_models:

  A list of OpenMx models for each family group.

- ci:

  Logical. If TRUE, include confidence interval computations for the
  variance components. Default is FALSE

- condenseMatrixSlots:

  Logical. If TRUE, use the mxCondenseMatrixSlots wrapper to optimize
  memory usage for large matrices. Default is TRUE.

- temporal:

  Logical. If TRUE, build a time-varying family-group model where each
  component's weight is a function of `param_year` (and, optionally,
  `H`) instead of a fixed scalar. Default is FALSE.

- p_hist:

  Integer. Number of historical moderator columns. Only used when
  `temporal = TRUE`. Default is 0.

- param_year:

  Numeric vector of birth years (or another time index), one per member
  of the family, in the same order as `obs_ids`. Only used when
  `temporal = TRUE`.

- components:

  Character vector of component keys to include (any of "a", "d", "cn",
  "ce", "mt", "am", "e"). Only used when `temporal = TRUE`. Default is
  `c("a", "e")`.

- ci_names:

  A vector of parameters for confidence intervals. If not provided, it
  is derived from components

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

## Value

An OpenMx pedigree model combining variance components and family
groups.
