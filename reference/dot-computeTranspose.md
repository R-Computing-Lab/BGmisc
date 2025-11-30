# Compute the transpose multiplication for the relatedness matrix

Compute the transpose multiplication for the relatedness matrix

## Usage

``` r
.computeTranspose(r2, transpose_method = "tcrossprod", verbose = FALSE)
```

## Arguments

- r2:

  a relatedness matrix

- transpose_method:

  character. The method to use for computing the transpose. Options are
  "tcrossprod", "crossprod", or "star"

- verbose:

  logical. If TRUE, print progress through stages of algorithm

## Details

The algorithms and methodologies used in this function are further
discussed and exemplified in the vignette titled
"examplePedigreeFunctions". For more advanced scenarios and detailed
explanations, consult this vignette.
