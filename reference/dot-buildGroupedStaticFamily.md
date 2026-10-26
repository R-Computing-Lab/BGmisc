# Build one grouped static family model

This function constructs one OpenMx model for multiple independent
family observations that share the same pedigree structure, relatedness
matrices, variable ordering, expected covariance matrix, and expected
mean vector.

## Usage

``` r
.buildGroupedStaticFamily(
  group_name,
  dat,
  obs_ids,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  condenseMatrixSlots = TRUE,
  clean_ids = FALSE
)
```

## Arguments

- group_name:

  Name of the grouped family model.

- dat:

  A matrix or data frame where each row represents an independent family
  and columns correspond to pedigree positions.

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

- condenseMatrixSlots:

  Logical. If TRUE, use the mxCondenseMatrixSlots wrapper to optimize
  memory usage for large matrices. Default is TRUE.

- clean_ids:

  Logical. If TRUE, clean the `obs_ids` to be syntactically valid R
  names using `make.names`. Default is FALSE.

## Value

An OpenMx model containing all static family observations.

## Details

The observed data contain one family per row. The shared matrices,
covariance algebra, expectation, and fit function are stored only once
rather than once per family.
