# Function to find the biggest families in a pedigree This function finds the biggest families in a pedigree. It is supposed to be used internally by the `summarize_pedigree` function.

Function to find the biggest families in a pedigree This function finds
the biggest families in a pedigree. It is supposed to be used internally
by the `summarize_pedigree` function.

## Usage

``` r
findBiggest(foo_summary_dt, n_fooest = 5, n_foo_total = nrow(foo_summary_dt))
```

## Arguments

- foo_summary_dt:

  A data.table containing the summary statistics.

- n_fooest:

  An integer specifying the number of individuals in the summary.

- n_foo_total:

  An integer specifying the total number of individuals in the summary.

## Value

a data.table containing the biggest families in the pedigree.
