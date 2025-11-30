# Function to calculate summary statistics for all numeric variables This function calculates summary statistics for all numeric variables in a data.table. It is supposed to be used internally by the `summarize_pedigree` function.

Function to calculate summary statistics for all numeric variables This
function calculates summary statistics for all numeric variables in a
data.table. It is supposed to be used internally by the
`summarize_pedigree` function.

## Usage

``` r
calculateSummaryDT(
  data,
  group_var,
  skip_var,
  five_num_summary = FALSE,
  na_rm = TRUE
)
```

## Arguments

- data:

  A data.table containing the pedigree data.

- group_var:

  A character string specifying the column name of the grouping
  variable.

- skip_var:

  Character vector. Variables to exclude from summary calculations.

- five_num_summary:

  Logical. If \`TRUE\`, includes the first quartile (Q1) and third
  quartile (Q3) in addition to the minimum, median, and maximum values.

- na_rm:

  Logical. If \`TRUE\`, removes \`NA\` values when calculating
  statistics.

## Value

A data.table containing the summary statistics for all numeric
variables.
