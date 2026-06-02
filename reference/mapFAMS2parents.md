# Create a Mapping from Family IDs to Parent IDs

This function scans the data frame and creates a mapping of family IDs
to the corresponding parent IDs.

## Usage

``` r
mapFAMS2parents(df_temp, mom_sex = "F", dad_sex = "M")
```

## Arguments

- df_temp:

  A data frame produced by
  [`readGedcom()`](https://r-computing-lab.github.io/BGmisc/reference/readGedcom.md).

- mom_sex:

  Character string indicating the value of sex that corresponds to
  mothers (default "F").

- dad_sex:

  Character string indicating the value of sex that corresponds to
  fathers (default "M").

## Value

A list mapping family IDs to parent information.
