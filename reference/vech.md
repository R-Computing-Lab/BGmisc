# vech Create the half-vectorization of a matrix

vech Create the half-vectorization of a matrix

## Usage

``` r
vech(x)
```

## Arguments

- x:

  a matrix, the half-vectorization of which is desired

## Value

A vector containing the lower triangle of the matrix, including the
diagonal.

## Details

This function returns the vectorized form of the lower triangle of a
matrix, including the diagonal. The upper triangle is ignored with no
checking that the provided matrix is symmetric.

## Examples

``` r
vech(matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2))
#> [1] 1.0 0.5 1.0
```
