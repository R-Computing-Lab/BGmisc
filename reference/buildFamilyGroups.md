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
  prefix = "fam"
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

  Additive genetic relatedness matrix.

- Nucmat:

  Nuclear family shared environment relatedness matrix.

- Extmat:

  Extended family shared environment relatedness matrix.

- Mtdmat:

  Mitochondrial genetic relatedness matrix.

- Amimat:

  Additive by mitochondrial interaction relatedness matrix.

- Dmgmat:

  Dominance genetic relatedness matrix.

- prefix:

  A prefix for naming the family groups. Default is "fam".

## Value

A list of OpenMx models for each family group.
