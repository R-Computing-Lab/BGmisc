# Make Clean IDs for OpenMx

This function takes a vector of IDs and returns a cleaned version
suitable for use in OpenMx models. It replaces any illegal characters
(such as '.') with underscores and ensures that the IDs are valid R
variable names. This is important because OpenMx does not allow certain
characters in matrix or variable names, which can lead to errors when
building models.

## Usage

``` r
make_clean_personids(ids)
```

## Arguments

- ids:

  A vector of IDs to be cleaned.

## Value

A vector of cleaned IDs suitable for use in OpenMx models.
