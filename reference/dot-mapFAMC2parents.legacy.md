# Assign momID and dadID based on family mapping

This function assigns mother and father IDs to individuals in the data
frame based on the mapping of family IDs to parent IDs.

## Usage

``` r
.mapFAMC2parents.legacy(df_temp, family_to_parents)
```

## Arguments

- df_temp:

  A data frame containing individual information.

- family_to_parents:

  A list mapping family IDs to parent IDs.

## Value

A data frame with added momID and dad_ID columns.
