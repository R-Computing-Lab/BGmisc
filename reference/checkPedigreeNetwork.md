# Validate Pedigree Network Structure

Checks for structural issues in pedigree networks, including: -
Individuals with more than two parents. - Presence of cyclic
parent-child relationships.

## Usage

``` r
checkPedigreeNetwork(
  ped,
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  verbose = FALSE
)
```

## Arguments

- ped:

  Dataframe representing the pedigree.

- personID:

  Character. Column name for individual IDs.

- momID:

  Character. Column name for maternal IDs.

- dadID:

  Character. Column name for paternal IDs.

- verbose:

  Logical. If TRUE, print informative messages.

## Value

List containing detailed validation results.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- checkPedigreeNetwork(ped,
  personID = "ID",
  momID = "momID", dadID = "dadID", verbose = TRUE
)
} # }
```
