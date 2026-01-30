# Restore Original Column Names in a Pedigree Dataframe

This function restores the original column names of a pedigree dataframe
based on user-specified names. It is useful for reverting standardized
column names back to their original names after processing.

## Usage

``` r
restorePedColnames(
  ped,
  famID = "famID",
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  gen = "gen",
  patID = "patID",
  matID = "matID",
  spID = "spID",
  twinID = "twinID",
  zygosity = "zygosity",
  sex = "sex",
  verbose = FALSE
)
```

## Arguments

- ped:

  A pedigree dataframe with standardized column names.

- famID:

  The original name for the family ID column. Default is "fam ID".

- personID:

  The original name for the person ID column. Default is "ID".

- momID:

  The original name for the mother ID column. Default is "momID".

- dadID:

  The original name for the father ID column. Default is "dadID".

- gen:

  The original name for the generation column. Default is "gen".

- patID:

  The original name for the paternal ID column. Default is "patID".

- matID:

  The original name for the maternal ID column. Default is "matID".

- spID:

  The original name for the spouse ID column. Default is "spID".

- twinID:

  The original name for the twin ID column. Default is "twinID".

- zygosity:

  The original name for the zygosity column. Default is "zygosity".

- sex:

  The original name for the sex column. Default is "sex".

- verbose:

  A logical indicating whether to print progress messages.

## Value

A pedigree dataframe with restored original column names.
