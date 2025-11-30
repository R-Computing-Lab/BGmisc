# Repair Parent IDs

This function repairs parent IDs in a pedigree.

## Usage

``` r
repairParentIDs(
  ped,
  verbose = FALSE,
  famID = "famID",
  personID = "ID",
  momID = "momID",
  dadID = "dadID"
)
```

## Arguments

- ped:

  A dataframe representing the pedigree data with columns 'ID', 'dadID',
  and 'momID'.

- verbose:

  A logical flag indicating whether to print progress and validation
  messages to the console.

- famID:

  Character. Column name for family IDs.

- personID:

  Character. Column name for individual IDs.

- momID:

  Character. Column name for maternal IDs.

- dadID:

  Character. Column name for paternal IDs.

## Value

A corrected pedigree
