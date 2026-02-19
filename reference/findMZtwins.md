# Find MZ twin pair_rows in a pedigree

Identifies MZ twin pair_rows from the `twinID` column and returns their
row indices. These indices are used later to merge the twins' columns in
the `r2` matrix before `tcrossprod`, which correctly produces
relatedness 1 between MZ co-twins with no diagonal or downstream
artifacts.

## Usage

``` r
findMZtwins(
  ped,
  verbose = FALSE,
  returnRows = TRUE,
  returnIDs = FALSE,
  returnAsList = TRUE,
  beta = FALSE
)
```

## Arguments

- ped:

  A pedigree data.frame with columns `ID` and `twinID`. Optionally a
  `zygosity` column; when present only pair_rows where both members have
  `zygosity == "MZ"` are used.

- verbose:

  logical. If TRUE, print progress messages.

- returnRows:

  logical. If TRUE, return the row indices of the twin pair_rows instead
  of IDs.

- returnIDs:

  logical. If TRUE, return the IDs of the twin pair_rows instead of row
  indices.

- returnAsList:

  logical. If TRUE, return results as a list of vectors (default). If
  FALSE, return results as a data.frame with separate columns for each
  twin's ID and row index. @param beta logical. If TRUE, use an
  optimized approach with O(1) lookups for large pedigrees. If FALSE
  (default), use a simpler approach that may be less efficient for large
  pedigrees.

## Value

A list of length-2 integer vectors `c(idx1, idx2)` giving the row
indices of each MZ pair in the pedigree, or `NULL` if none found.
