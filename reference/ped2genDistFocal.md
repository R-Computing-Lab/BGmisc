# Compute generational distances from a focal individual to all others

Appends a new column to `ped` containing the generational distance
between each individual and the focal person. Unrelated individuals
receive `NA`.

## Usage

``` r
ped2genDistFocal(
  ped,
  focal_id,
  method = c("rank", "path", "mrca_min", "mrca_max", "mrca_all"),
  col_name = NULL,
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

- focal_id:

  ID of the target individual.

- method:

  Distance method; see
  [`getGenDist`](https://r-computing-lab.github.io/BGmisc/reference/getGenDist.md).

- col_name:

  Name of the new column. Defaults to
  `paste0("genDist_", method, "_", focal_id)`.

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

The input `ped` with one additional column.

## See also

[`getGenDist`](https://r-computing-lab.github.io/BGmisc/reference/getGenDist.md),
[`ped2genDist`](https://r-computing-lab.github.io/BGmisc/reference/ped2genDist.md)
