# Reduce an ancestor distance matrix to a pairwise generational distance matrix

Given the `n x n` ancestor distance matrix returned by
`ped2com(component = "distance")`, computes a pairwise distance matrix
`D` where `D[i, j]` summarizes the relationship between individuals `i`
and `j` through their common ancestors.

## Usage

``` r
.pairDistFromAnc(ancDist, method)
```

## Arguments

- ancDist:

  Ancestor distance matrix from `ped2com(component = "distance")`:
  `ancDist[i, j]` is the minimum number of parent-child steps from
  individual `i` up to ancestor `j`; `NA` if `j` is not an ancestor of
  `i`; diagonal = 0.

- method:

  One of `"path"`, `"mrca_min"`, `"mrca_max"`, or `"mrca_all"`.

## Value

A symmetric numeric `n x n` matrix; `NA` for unrelated pairs.

## Details

For each potential common ancestor column `c`, the combined step count
`ancDist[i, c] + ancDist[j, c]` is computed for all pairs simultaneously
via `outer`, then collapsed across ancestors with `pmin`/`pmax`.
