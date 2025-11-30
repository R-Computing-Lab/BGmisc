# validate_and_convert_matrix

This function validates and converts a matrix to a specific format.

## Usage

``` r
validate_and_convert_matrix(
  mat,
  name,
  ensure_symmetric = FALSE,
  force_binary = FALSE
)
```

## Arguments

- mat:

  The matrix to be validated and converted.

- name:

  The name of the matrix for error messages.

- ensure_symmetric:

  Logical indicating whether to ensure the matrix is symmetric.

- force_binary:

  Logical indicating whether to force the matrix to be binary.

## Value

The validated and converted matrix.
