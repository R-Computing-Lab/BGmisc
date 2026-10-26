# Symmetrize a Matrix

Averages a matrix with its transpose when it is not already symmetric
within tolerance. Used to clean up relatedness matrices before they are
passed to OpenMx, which requires exact symmetry for `"Symm"`-type
`mxMatrix` objects.

## Usage

``` r
make_symmetric(x, tol = 1e-10)
```

## Arguments

- x:

  A square matrix, or an object coercible to one.

- tol:

  Numeric tolerance for asymmetry. Default is 1e-10.

## Value

A numeric matrix, symmetrized if needed.
