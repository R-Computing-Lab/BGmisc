# Build Temporal Family Group Models with Per-Family Relatedness Matrices

Wrapper around
[`buildFamilyGroups_list`](https://r-computing-lab.github.io/BGmisc/reference/buildFamilyGroups_list.md)
with `temporal = TRUE`. Use this when families vary in size or
structure.

## Usage

``` r
buildTemporalFamilyGroups_list(
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
)
```

## Arguments

- dat_list:

  A list of numeric vectors of observed data, one per family.

- obs_ids_list:

  A list of character vectors of individual IDs, one per family,
  matching `dat_list` and the rows/columns of that family's relatedness
  matrices.

- param_year_list:

  A list of numeric birth-year vectors, one per family, each matching
  the corresponding entry of `obs_ids_list`. Only used when
  `temporal = TRUE`.

- H_list:

  A list of historical-moderator matrices, one per family. Only used
  when `temporal = TRUE`.

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

## Value

A list of OpenMx models for each family group.
