# Post-process GEDCOM Data Frame

This function optionally adds parent information, combines duplicate
columns, and removes empty columns from the GEDCOM data frame. It is
called by
[`readGedcom()`](https://r-computing-lab.github.io/BGmisc/reference/readGedcom.md)
if `post_process = TRUE`.

## Usage

``` r
postProcessGedcom(
  df_temp,
  remove_empty_cols = TRUE,
  combine_cols = TRUE,
  add_parents = TRUE,
  skinny = TRUE,
  verbose = FALSE
)
```

## Arguments

- df_temp:

  A data frame produced by
  [`readGedcom()`](https://r-computing-lab.github.io/BGmisc/reference/readGedcom.md).

- remove_empty_cols:

  Logical indicating whether to remove columns that are entirely
  missing.

- combine_cols:

  Logical indicating whether to combine columns with duplicate values.

- add_parents:

  Logical indicating whether to add parent information.

- skinny:

  Logical indicating whether to slim down the data frame.

- verbose:

  Logical indicating whether to print progress messages.

## Value

The post-processed data frame.
