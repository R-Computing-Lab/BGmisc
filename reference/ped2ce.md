# Take a pedigree and turn it into an extended environmental relatedness matrix

Take a pedigree and turn it into an extended environmental relatedness
matrix

## Usage

``` r
ped2ce(
  ped,
  personID = "ID",
  keep_ids = NULL,
  sparse = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- ped:

  a pedigree dataset. Needs ID, momID, and dadID columns

- personID:

  Character. Column name for individual IDs.

- keep_ids:

  character vector of IDs to retain in the final relatedness matrix.
  When supplied, only the rows of `r2` corresponding to these IDs are
  used in the tcrossprod, so the result is a
  `length(keep_ids) x length(keep_ids)` matrix. All columns of `r2` are
  retained during the multiplication so relatedness values remain
  correct. IDs not found in the pedigree are silently dropped with a
  warning.

- sparse:

  logical. If TRUE, use and return sparse matrices from Matrix package

- verbose:

  logical. If TRUE, print progress through stages of algorithm

- ...:

  additional arguments to be passed to
  [`ped2com`](https://R-Computing-Lab.github.io/BGmisc/reference/ped2com.md)

## Details

The algorithms and methodologies used in this function are further
discussed and exemplified in the vignette titled
"examplePedigreeFunctions". For more advanced scenarios and detailed
explanations, consult this vignette.
