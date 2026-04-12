# Align Phenotype Vector to Matrix Format for OpenMx

This function takes a pedigree data frame, a specified phenotype column,
and a vector of IDs to keep, and returns a matrix formatted for use in
OpenMx models. The resulting matrix has one row and columns
corresponding to the specified IDs, with values taken from the phenotype
column of the pedigree.

## Usage

``` r
alignPhenToMatrix(ped, phenotype, keep_ids, personID = "ID")
```

## Arguments

- ped:

  A data frame representing the pedigree, containing at least the
  columns specified by `phenotype` and `personID`.

- phenotype:

  A character string specifying the column name in `ped` that contains
  the phenotype values to be aligned.

- keep_ids:

  A vector of IDs for which the phenotype values should be extracted and
  aligned. These IDs should correspond to the values in the `personID`
  of `ped`.

- personID:

  A character string specifying the column name in `ped` that contains
  the individual IDs. Default is "ID".
