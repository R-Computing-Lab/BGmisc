# Build Pedigree mxModel

This function constructs an OpenMx pedigree model by combining variance
component parameters and family group models. It auto-detects which
variance components are referenced in the group algebras and creates
only those parameters.

## Usage

``` r
buildPedigreeMx(
  model_name,
  vars,
  group_models,
  ci = FALSE,
  condenseMatrixSlots = TRUE
)
```

## Arguments

- model_name:

  Name of the overall pedigree model.

- vars:

  A named list or vector of initial variance component values.

- group_models:

  A list of OpenMx models for each family group.

- ci:

  Logical. If TRUE, include confidence interval computations for the
  variance components. Default is FALSE

- condenseMatrixSlots:

  Logical. If TRUE, use the mxCondenseMatrixSlots wrapper to optimize
  memory usage for large matrices. Default is TRUE.

## Value

An OpenMx pedigree model combining variance components and family
groups.
