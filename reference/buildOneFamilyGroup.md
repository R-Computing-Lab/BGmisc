# Build one family group model

This function constructs an OpenMx model for a single family group based
on provided relatedness matrices and observed data. The implied
covariance is built as a weighted sum of the supplied relatedness
matrices, where the weights are variance component parameters shared
across groups via a parent `ModelOne` sub-model.

## Usage

``` r
buildOneFamilyGroup(
  group_name,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  full_df_row,
  obs_ids,
  condenseMatrixSlots = TRUE
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

- condenseMatrixSlots:

  Logical. If TRUE, use the mxCondenseMatrixSlots wrapper to optimize
  memory usage for large matrices. Default is TRUE.

## Value

An OpenMx model for the specified family group.
