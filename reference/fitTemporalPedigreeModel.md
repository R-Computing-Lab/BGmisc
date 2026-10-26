# Fit a Temporal OpenMx Pedigree Model to Observed Data

Wrapper around
[`fitPedigreeModel`](https://r-computing-lab.github.io/BGmisc/reference/fitPedigreeModel.md)
with `temporal = TRUE`, exposing only the temporal-relevant arguments.

## Usage

``` r
fitTemporalPedigreeModel(
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
  start_beta0 = 0.5,
  start_beta_time = 0,
  start_gamma = 0,
  mean_degree = 0,
  start_mean = 0,
  mean_hist_free = FALSE,
  start_mean_hist = 0,
  clean_ids = FALSE
)
```

## Arguments

- model_name:

  Character. Name for the overall OpenMx model. Default is
  "PedigreeModel".

- group_models:

  Optional list of pre-built OpenMx family group models (from
  [`buildOneFamilyGroup`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md)).
  If NULL, they are generated from `data` (or, when `temporal = TRUE`,
  from `dat_list` and friends) using the provided relatedness matrices.

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

- tryhard:

  Logical. If TRUE (default), use `mxTryHard` for robust optimization;
  if FALSE, use `mxRun`.

- intervals:

  Logical. If TRUE (default), compute confidence intervals for the
  parameters using `mxSE` and `mxCI`.

- extraTries:

  Numeric. The number of extra optimization attempts to make when
  `tryhard` is TRUE. Default is 10.

- runmodel:

  Logical. If TRUE (default), the model is fitted; if FALSE, the model
  is returned without fitting.

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

- clean_ids:

  Logical. If TRUE, clean the `obs_ids` to be syntactically valid R
  names using `make.names`. Default is FALSE.

## Value

A fitted OpenMx model.
