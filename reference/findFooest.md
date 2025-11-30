# Function to find the most extreme individuals in a pedigree This function finds the most extreme individuals (oldest or youngest) in a pedigree. It is supposed to be used internally by the `summarize_pedigree` function.

Function to find the most extreme individuals in a pedigree This
function finds the most extreme individuals (oldest or youngest) in a
pedigree. It is supposed to be used internally by the
`summarize_pedigree` function.

## Usage

``` r
findFooest(
  foo_summary_dt,
  sort_var,
  n_fooest,
  n_foo_total = nrow(foo_summary_dt),
  decreasing = FALSE
)
```

## Arguments

- sort_var:

  A character string specifying the column to sort by.

- n_fooest:

  An integer specifying the number of individuals to return in the
  summary.

- n_foo_total:

  An integer specifying the total number of individuals in the summary.

- decreasing:

  A logical indicating whether to sort in decreasing order.

## Value

A data.table with the top rows selected from `foo_summary_dt`.
