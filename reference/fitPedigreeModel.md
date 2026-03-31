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
  tryhard = TRUE,
  intervals = TRUE,
  Addmat = NULL,
  Nucmat = NULL,
  Extmat = NULL,
  Mtdmat = NULL,
  Amimat = NULL,
  Dmgmat = NULL
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

- tryhard:

  Logical. If TRUE (default), use `mxTryHard` for robust optimization;
  if FALSE, use `mxRun`.

- intervals:

  Logical. If TRUE (default), compute confidence intervals for the
  parameters using `mxSE` and `mxCI`.

- Addmat:

  Additive genetic relatedness matrix. Required when `group_models` is
  NULL.

- Nucmat:

  Common nuclear environment relatedness matrix. Optional.

- Extmat:

  Common extended environment relatedness matrix. Optional.

- Mtdmat:

  Mitochondrial relatedness matrix. Optional.

- Amimat:

  Additive-by-mitochondrial interaction matrix. Optional.

- Dmgmat:

  Dominance genetic relatedness matrix. Optional.

## Value

A fitted OpenMx model.
