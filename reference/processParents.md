# Process Parents Information from GEDCOM Data

This function adds mother and father IDs to individuals in the data
frame

## Usage

``` r
processParents(df_temp, datasource)
```

## Arguments

- df_temp:

  A data frame produced by
  [`readGedcom()`](https://r-computing-lab.github.io/BGmisc/reference/readGedcom.md).

- datasource:

  Character string indicating the data source ("gedcom" or "wiki").

## Value

The updated data frame with parent IDs added.
