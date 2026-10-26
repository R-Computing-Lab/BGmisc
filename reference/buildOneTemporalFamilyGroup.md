# Build One Temporal Family Group Model

Wrapper around
[`buildOneFamilyGroup`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md)
with `temporal = TRUE`.

## Usage

``` r
buildOneTemporalFamilyGroup(
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
)
```

## Arguments

- group_name:

  Name of the family group.

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

- full_df_row:

  A 1-row matrix of observed data with column names matching `obs_ids`.

- obs_ids:

  A character vector of individual IDs corresponding to the columns of
  `full_df_row` and the rows/columns of the relatedness matrices. Must
  be in the same order as the relatedness matrix rows.

- param_year:

  Numeric vector of birth years (or another time index), one per member
  of the family, in the same order as `obs_ids`. Only used when
  `temporal = TRUE`.

- H:

  Optional numeric matrix of historical moderators, one row per family
  member (same order as `obs_ids`), one column per moderator. Only used
  when `temporal = TRUE`.

- use_exp_loadings:

  Logical. If TRUE (default), each component's loading is exponentiated
  so its implied variance stays positive. Only used when
  `temporal = TRUE`.

- condenseMatrixSlots:

  Logical. If TRUE, use the mxCondenseMatrixSlots wrapper to optimize
  memory usage for large matrices. Default is TRUE.

- time_point_max:

  Integer degree of the polynomial birth-year basis. Only used when
  `temporal = TRUE`. Default is 3.

- clean_ids:

  Logical. If TRUE, clean the `obs_ids` to be syntactically valid R
  names using `make.names`. Default is FALSE.

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

## Value

An OpenMx model for the specified family group.
