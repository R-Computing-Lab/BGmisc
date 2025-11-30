# Validates and Optionally Repairs Unique IDs in a Pedigree Dataframe

This function takes a pedigree object and performs two main tasks: 1.
Checks for the uniqueness of individual IDs. 2. Optionally repairs
non-unique IDs based on a specified logic.

## Usage

``` r
checkIDs(ped, verbose = FALSE, repair = FALSE)
```

## Arguments

- ped:

  A dataframe representing the pedigree data with columns \`ID\`,
  \`dadID\`, and \`momID\`.

- verbose:

  A logical flag indicating whether to print progress and validation
  messages to the console.

- repair:

  A logical flag indicating whether to attempt repairs on non-unique
  IDs.

## Value

Depending on \`repair\` value, either returns a list containing
validation results or a repaired dataframe

## Examples

``` r
if (FALSE) { # \dontrun{
ped <- data.frame(ID = c(1, 2, 2, 3), dadID = c(NA, 1, 1, 2), momID = c(NA, NA, 2, 2))
checkIDs(ped, verbose = TRUE, repair = FALSE)
} # }
```
