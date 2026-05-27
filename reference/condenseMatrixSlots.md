# Condense Matrix Slots in an OpenMx Model

This function takes an OpenMx model and applies the
`mxCondenseMatrixSlots` wrapper to optimize memory usage for large
matrices. This can be particularly beneficial when working with large
pedigree models that include multiple relatedness matrices.

## Usage

``` r
condenseMatrixSlots(model)
```

## Arguments

- model:

  An OpenMx model object for which to condense matrix slots. If NULL,
  the function returns NULL.

## Value

An OpenMx model with condensed matrix slots, or NULL if the input model
is NULL.
