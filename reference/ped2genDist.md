# Compute a full pairwise generational distance matrix

Returns an n x n matrix of generational distances between all pairs of
individuals in `ped`. Pairs with no genealogical path receive `NA`.

Uses
[`ped2com`](https://r-computing-lab.github.io/BGmisc/reference/ped2com.md)
with `component = "distance"` to build the ancestor distance matrix via
the same adjacency construction and power iteration used by all other
pedigree components, then reduces it to pairwise distances with
vectorised `outer` + `pmin`/`pmax`.

## Usage

``` r
ped2genDist(
  ped,
  method = c("rank", "path", "mrca_min", "mrca_max", "mrca_all"),
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  max_gen = 25L,
  ...
)
```

## Arguments

- ped:

  A pedigree data frame.

- method:

  Distance method; see
  [`getGenDist`](https://r-computing-lab.github.io/BGmisc/reference/getGenDist.md).

- personID:

  Character. ID column name. Default `"ID"`.

- momID:

  Character. Mother ID column name. Default `"momID"`.

- dadID:

  Character. Father ID column name. Default `"dadID"`.

- max_gen:

  Integer. Maximum generations to traverse. Default `25`.

- ...:

  Additional arguments passed to
  [`ped2com`](https://r-computing-lab.github.io/BGmisc/reference/ped2com.md)
  (e.g. `adjacency_method`, `verbose`, `sparse`).

## Value

A numeric matrix with row and column names set to individual IDs.

## See also

[`getGenDist`](https://r-computing-lab.github.io/BGmisc/reference/getGenDist.md),
[`ped2genDistFocal`](https://r-computing-lab.github.io/BGmisc/reference/ped2genDistFocal.md)
