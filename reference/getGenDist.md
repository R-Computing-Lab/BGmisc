# Compute the generational distance between two individuals

Returns a single numeric value representing how many generations apart
two individuals are, according to the chosen method.

## Usage

``` r
getGenDist(
  ped,
  id1,
  id2,
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

- id1, id2:

  IDs of the two individuals to compare.

- method:

  Distance method. One of:

  `"rank"`

  :   Absolute difference in generation numbers from
      [`ped2gen`](https://r-computing-lab.github.io/BGmisc/reference/ped2gen.md).
      Founders = 1, their children = 2, etc.

  `"path"`

  :   Minimum number of parent-child steps between the two individuals
      through any shared ancestor (undirected path length).

  `"mrca_min"`

  :   Total steps via the most recent common ancestor (fewest combined
      steps to reach a shared ancestor).

  `"mrca_max"`

  :   Total steps via the most distant common ancestor (most combined
      steps to reach a shared ancestor).

  `"mrca_all"`

  :   Aggregated distance across all common ancestors

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

A single numeric value, or `NA` if no genealogical path exists.

## See also

[`ped2genDist`](https://r-computing-lab.github.io/BGmisc/reference/ped2genDist.md),
[`ped2genDistFocal`](https://r-computing-lab.github.io/BGmisc/reference/ped2genDistFocal.md)
