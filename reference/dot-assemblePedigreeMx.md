# Assemble the top-level pedigree mxModel (internal)

Combines the covariance sub-model (named `"ModelOne"`), the family-group
models, the multigroup fit function, and (optionally) a
confidence-interval specification into one mxModel. Shared by both the
static and temporal branches of
[`buildPedigreeMx`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md),
which otherwise only differ in how `model_one` and `ci_obj` are built.

## Usage

``` r
.assemblePedigreeMx(model_name, model_one, group_models, ci_obj = NULL)
```

## Arguments

- model_name:

  Name of the overall pedigree model.

- model_one:

  The covariance sub-model (from
  [`buildPedigreeModelCovariance`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeModelCovariance.md)).

- group_models:

  A list of OpenMx models for each family group.

- ci_obj:

  An `mxCI` object to include, or NULL to omit confidence intervals.

## Value

An OpenMx pedigree model.
