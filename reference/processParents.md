# Process Parents Information from GEDCOM Data

This function adds mother and father IDs to individuals in the data
frame

## Usage

``` r
processParents(df_temp, datasource, person_id_col = "personID")
```

## Arguments

- df_temp:

  A data frame produced by
  [`readGedcom()`](https://r-computing-lab.github.io/BGmisc/reference/readGedcom.md).

- datasource:

  Character string indicating the data source ("gedcom" or "wiki").

- person_id_col:

  Character string indicating the column name for individual IDs
  (default "personID").

## Value

The updated data frame with parent IDs added.
