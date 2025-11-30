# Function to find the originating member for each line

This function finds the originating member for each line in a pedigree.
It is supposed to be used internally by the `summarize_pedigree`
function.

## Usage

``` r
findFounder(data, group_var, sort_var)
```

## Arguments

- data:

  A data.table containing the pedigree data.

- sort_var:

  A character string specifying the column name to sort by.

## Value

A data.table containing the originating member for each line.
