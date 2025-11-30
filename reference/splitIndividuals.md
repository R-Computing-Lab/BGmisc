# Split GEDCOM Lines into Individual Blocks

This function partitions the GEDCOM file (as a vector of lines) into a
list of blocks, where each block corresponds to a single individual
starting with an "@ INDI" line.

## Usage

``` r
splitIndividuals(lines, verbose = FALSE)
```

## Arguments

- lines:

  A character vector of lines from the GEDCOM file.

- verbose:

  Logical indicating whether to output progress messages.

## Value

A list of character vectors, each representing one individual.

## Details

Each block runs until the next "@ INDI" line or end-of-file. Blocks are
raw subsets of the file; no parsing occurs here.
