# Post-process GEDCOM Data Frame

Post-process GEDCOM Data Frame

## Usage

``` r
.postProcessGedcom.legacy(
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

  A data frame containing information about individuals.

- remove_empty_cols:

  A logical value indicating whether to remove columns with all missing
  values.

- combine_cols:

  A logical value indicating whether to combine columns with duplicate
  values.

- add_parents:

  A logical value indicating whether to add parents to the data frame.

- skinny:

  A logical value indicating whether to return a skinny data frame.

- verbose:

  A logical value indicating whether to print messages.

## Value

A data frame with processed information.
