# comp2vech Turn a variance component relatedness matrix into its half-vectorization

comp2vech Turn a variance component relatedness matrix into its
half-vectorization

## Usage

``` r
comp2vech(x, include.zeros = FALSE)
```

## Arguments

- x:

  Relatedness component matrix (can be a matrix, list, or object that
  inherits from 'Matrix').

- include.zeros:

  logical. Whether to include all-zero rows. Default is FALSE.

## Value

The half-vectorization of the relatedness component matrix.

## Details

This function is a wrapper around the `vech` function, extending it to
allow for blockwise matrices and specific classes. It facilitates the
conversion of a variance component relatedness matrix into a
half-vectorized form.

## Examples

``` r
comp2vech(list(matrix(c(1, .5, .5, 1), 2, 2), matrix(1, 2, 2)))
#> [1] 1.0 0.5 1.0 1.0 1.0 1.0
```
