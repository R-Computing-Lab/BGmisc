# Count GEDCOM Pattern Rows

Counts the number of lines in a file (passed as a data frame with column
"X1") that match various GEDCOM patterns. Returns a list with counts for
each pattern.

## Usage

``` r
countPatternRows(file)
```

## Arguments

- file:

  A data frame with a column `X1` containing GEDCOM lines.

## Value

A list with counts of specific GEDCOM tag occurrences.
