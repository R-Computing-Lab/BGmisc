# Build family group models

This function constructs OpenMx models for multiple family groups based
on provided relatedness matrices and observed data.

## Usage

``` r
buildFamilyGroups(
  dat,
  obs_ids,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  prefix = "fam",
  condenseMatrixSlots = TRUE
)
```

## Arguments

- dat:

  A data frame where each row represents a family group and columns
  correspond to observed variables.

- obs_ids:

  A character vector of individual IDs corresponding to the columns of
  `dat` and the rows/columns of the relatedness matrices.

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

- condenseMatrixSlots:

  Logical. If TRUE, use the mxCondenseMatrixSlots wrapper to optimize
  memory usage for large matrices. Default is TRUE.

## Value

A list of OpenMx models for each family group.
