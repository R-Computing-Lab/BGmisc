# Build family group models with per-family relatedness matrices

This function constructs OpenMx models for multiple family groups, each
with its own relatedness matrices, observed IDs, and (for temporal
models) birth years / historical moderators. Use this when families vary
in size or structure; for families that all share the same relatedness
matrices, see
[`buildFamilyGroups`](https://r-computing-lab.github.io/BGmisc/reference/buildFamilyGroups.md).

## Usage

``` r
buildFamilyGroups_list(
  dat_list,
  obs_ids_list,
  Addmat_list = NULL,
  Nucmat_list = NULL,
  Extmat_list = NULL,
  Mtdmat_list = NULL,
  Amimat_list = NULL,
  Dmgmat_list = NULL,
  prefix = "fam",
  condenseMatrixSlots = TRUE,
  temporal = FALSE,
  param_year_list = NULL,
  H_list = NULL,
  use_exp_loadings = TRUE,
  time_point_max = NULL,
  retain_eta = TRUE,
  retain_loadings = TRUE,
  retain_loading_covariances = TRUE,
  retain_component_covariances = TRUE,
  residual_covariance_form = c("outer_product", "diagonal"),
  clean_ids = TRUE
)
```

## Arguments

- dat_list:

  A list of numeric vectors of observed data, one per family.

- obs_ids_list:

  A list of character vectors of individual IDs, one per family,
  matching `dat_list` and the rows/columns of that family's relatedness
  matrices.

- Addmat_list:

  A list of additive genetic relatedness matrices, one per family.

- Nucmat_list:

  A list of nuclear family shared environment relatedness matrices, one
  per family.

- Extmat_list:

  A list of common extended family environment relatedness matrices, one
  per family.

- Mtdmat_list:

  A list of mitochondrial genetic relatedness matrices, one per family.

- Amimat_list:

  A list of additive by mitochondrial interaction relatedness matrices,
  one per family.

- Dmgmat_list:

  A list of dominance genetic relatedness matrices, one per family.

- prefix:

  A prefix for naming the family groups. Default is "fam".

- condenseMatrixSlots:

  Logical. If TRUE, use the mxCondenseMatrixSlots wrapper to optimize
  memory usage for large matrices. Default is TRUE.

- temporal:

  Logical. If TRUE, build a time-varying family-group model where each
  component's weight is a function of `param_year` (and, optionally,
  `H`) instead of a fixed scalar. Default is FALSE.

- param_year_list:

  A list of numeric birth-year vectors, one per family, each matching
  the corresponding entry of `obs_ids_list`. Only used when
  `temporal = TRUE`.

- H_list:

  A list of historical-moderator matrices, one per family. Only used
  when `temporal = TRUE`.

- use_exp_loadings:

  Logical. If TRUE (default), each component's loading is exponentiated
  so its implied variance stays positive. Only used when
  `temporal = TRUE`.

- time_point_max:

  Integer degree of the polynomial birth-year basis. Only used when
  `temporal = TRUE`. Default is 3.

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

## Value

A list of OpenMx models for each family group.
