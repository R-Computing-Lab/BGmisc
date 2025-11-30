# Parse a GEDCOM Individual Block

Processes a block of GEDCOM lines corresponding to a single individual.

## Usage

``` r
parseIndividualBlock(block, pattern_rows, all_var_names, verbose = FALSE)
```

## Arguments

- block:

  A character vector containing the GEDCOM lines for one individual.

- pattern_rows:

  A list with counts of lines matching specific GEDCOM tags.

- all_var_names:

  A character vector of variable names.

- verbose:

  Logical indicating whether to print progress messages.

## Value

A named list representing the parsed record for the individual, or NULL
if no ID is found.
