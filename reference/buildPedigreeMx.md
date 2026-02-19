# Build Pedigree mxModel

This function constructs an OpenMx pedigree model by combining variance
component parameters and family group models. It auto-detects which
variance components are referenced in the group algebras and creates
only those parameters.

## Usage

``` r
buildPedigreeMx(model_name, vars, group_models)
```

## Arguments

- model_name:

  Name of the overall pedigree model.

- vars:

  A named list or vector of initial variance component values.

- group_models:

  A list of OpenMx models for each family group.

## Value

An OpenMx pedigree model combining variance components and family
groups.
