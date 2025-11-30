# Function to summarize the originating members for each line

This function summarizes the originating members for each line in a
pedigree. It is supposed to be used internally by the
`summarize_pedigree` function.

## Usage

``` r
summarizeFounder(ped_dt, group_var, sort_var, foo_summary_dt, verbose)
```

## Arguments

- sort_var:

  A character string specifying the column name to sort by.

- verbose:

  Logical, if TRUE, print progress messages.
