# Create a Mapping from Family IDs to Parent IDs

This function scans the data frame and creates a mapping of family IDs
to the corresponding parent IDs.

## Usage

``` r
mapFAMS2parents(df_temp)
```

## Arguments

- df_temp:

  A data frame produced by
  [`readGedcom()`](https://r-computing-lab.github.io/BGmisc/reference/readGedcom.md).

## Value

A list mapping family IDs to parent information.
