# Drop Identical Duplicate IDs from Pedigree Data Frame

\#' This function identifies and removes duplicate entries in a pedigree
data frame based on a list of specified IDs. If multiple rows share the
same ID and are identical, only one instance is retained. The function
returns the modified pedigree data frame along with a log of changes
made.

## Usage

``` r
dropIdenticalDuplicateIDs(ped, ids, changes = NULL)
```

## Arguments

- ped:

  A data frame representing the pedigree.

- ids:

  A vector of IDs to check for duplicates in the pedigree.

- changes:

  An optional list to log changes made during the process.
