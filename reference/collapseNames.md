# collapse Names

This function combines the \`name_given\` and \`name_given_pieces\`
columns in a data frame. If both columns have non-missing values that
differ, a warning is issued and the original \`name_given\` is retained.
If one column is missing, the other is used. The same logic applies to
the \`name_surn\` and \`name_surn_pieces\` columns.

## Usage

``` r
collapseNames(verbose, df_temp)
```

## Arguments

- verbose:

  Logical. If TRUE, print progress messages.

- df_temp:

  A data frame containing the columns to be combined.

## Value

A data frame with the combined columns.
