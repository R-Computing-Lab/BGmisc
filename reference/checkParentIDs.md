# Validates and Optionally Repairs Parent IDs in a Pedigree Dataframe

This function takes a pedigree object and performs two main tasks: 1.
Checks for the validity of parent IDs, specifically looking for
instances where only one parent ID is missing. 2. Optionally repairs the
missing parent IDs based on a specified logic.

## Usage

``` r
checkParentIDs(
  ped,
  verbose = FALSE,
  repair = FALSE,
  repairsex = repair,
  addphantoms = repair,
  parentswithoutrow = repair,
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

- repair:

  A logical flag indicating whether to attempt repairs on missing parent
  IDs.

- repairsex:

  A logical flag indicating whether to attempt repairs on sex of the
  parents

- addphantoms:

  A logical flag indicating whether to add phantom parents for missing
  parent IDs.

- parentswithoutrow:

  A logical flag indicating whether to add parents without a row in the
  pedigree.

- famID:

  Character. Column name for family IDs.

- personID:

  Character. Column name for individual IDs.

- momID:

  Character. Column name for maternal IDs.

- dadID:

  Character. Column name for paternal IDs.

## Value

Depending on the value of \`repair\`, either a list containing
validation results or a repaired dataframe is returned.

## Examples

``` r
if (FALSE) { # \dontrun{
ped <- data.frame(ID = 1:4, dadID = c(NA, 1, 1, 2), momID = c(NA, NA, 2, 2))
checkParentIDs(ped, verbose = TRUE, repair = FALSE)
} # }
```
