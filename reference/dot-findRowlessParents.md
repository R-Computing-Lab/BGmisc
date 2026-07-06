# Find Rowless Parents

Identifies IDs referenced in momID or dadID that have no row of their
own in `ped`. Used to detect parents (e.g., unrecorded founder stock)
whose genetic contribution cannot be traced because their own ancestry
is absent from the pedigree.

## Usage

``` r
.findRowlessParents(ped)
```

## Arguments

- ped:

  A dataframe representing the pedigree data with columns 'ID', 'dadID',
  and 'momID'.

## Value

A character/numeric vector of rowless parent IDs (empty if none).
