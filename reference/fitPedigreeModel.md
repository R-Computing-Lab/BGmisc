# Fit an OpenMx pedigree model to observed data

This function constructs and fits an OpenMx model for a pedigree using
specified variance components and family group models.

## Usage

``` r
fitPedigreeModel(
  model_name = "PedigreeModel",
  vars = list(ad2 = 0.5, dd2 = 0.3, cn2 = 0.2, ce2 = 0.4, mt2 = 0.1, am2 = 0.25, ee2 =
    0.6),
  data = NULL,
  group_models = NULL,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL,
  tryhard = TRUE,
  intervals = TRUE,
  extraTries = 10,
  condenseMatrixSlots = TRUE,
  runmodel = TRUE
)
```

## Arguments

- model_name:

  Character. Name for the overall OpenMx model. Default is
  "PedigreeModel".

- vars:

  A named list or vector of initial variance component values.

- data:

  A matrix or data frame of observed data, where each row is a family
  and columns correspond to individuals. Only used when `group_models`
  is NULL.

- group_models:

  Optional list of pre-built OpenMx family group models (from
  [`buildOneFamilyGroup`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md)).
  If NULL, they are generated from `data` using the provided relatedness
  matrices.

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

- tryhard:

  Logical. If TRUE (default), use `mxTryHard` for robust optimization;
  if FALSE, use `mxRun`.

- intervals:

  Logical. If TRUE (default), compute confidence intervals for the
  parameters using `mxSE` and `mxCI`.

- extraTries:

  Numeric. The number of extra optimization attempts to make when
  `tryhard` is TRUE. Default is 10.

- condenseMatrixSlots:

  Logical. If TRUE, use the mxCondenseMatrixSlots wrapper to optimize
  memory usage for large matrices. Default is TRUE.

- runmodel:

  Logical. If TRUE (default), the model is fitted; if FALSE, the model
  is returned without fitting.

## Value

A fitted OpenMx model.
