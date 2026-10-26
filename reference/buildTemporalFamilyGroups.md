# Build Temporal Family Group Models

Wrapper around
[`buildFamilyGroups`](https://r-computing-lab.github.io/BGmisc/reference/buildFamilyGroups.md)
with `temporal = TRUE`. All families share the same relatedness
matrices; only phenotype data, birth year, and historical moderators
vary by family. For families with different structure or size, see
[`buildTemporalFamilyGroups_list`](https://r-computing-lab.github.io/BGmisc/reference/buildTemporalFamilyGroups_list.md).

## Usage

``` r
buildTemporalFamilyGroups(
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
)
```

## Arguments

- dat:

  A data frame where each row represents a family group and columns
  correspond to observed variables.

- obs_ids:

  A character vector of individual IDs corresponding to the columns of
  `dat` and the rows/columns of the relatedness matrices.

- param_year_list:

  A list of numeric birth-year vectors, one per family (row of `dat`),
  each matching `obs_ids` in length and order. Only used when
  `temporal = TRUE`.

- H_list:

  A list of historical-moderator matrices, one per family. Only used
  when `temporal = TRUE`.

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

- prefix:

  A prefix for naming the family groups. Default is "fam".

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
