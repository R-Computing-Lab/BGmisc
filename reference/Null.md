# Compute the null space of a matrix

Compute the null space of a matrix

## Usage

``` r
Null(M)
```

## Arguments

- M:

  a matrix of which the null space is desired

## Details

The method uses the QR factorization to determine a basis for the null
space of a matrix. This is sometimes also called the orthogonal
complement of a matrix. As implemented, this function is identical to
the function of the same name in the MASS package.
