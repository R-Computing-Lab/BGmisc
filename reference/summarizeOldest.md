# Function to summarize the oldest individuals in a pedigree

Function to summarize the oldest individuals in a pedigree

## Usage

``` r
summarizeOldest(
  byr = NULL,
  n_oldest = 5,
  n_families = NULL,
  type = NULL,
  verbose = FALSE,
  output,
  family_summary_dt = NULL,
  n_mothers = NULL,
  maternal_summary_dt = NULL,
  n_fathers = NULL,
  paternal_summary_dt = NULL
)
```

## Arguments

- byr:

  Character. Optional column name for birth year. Used to determine the
  oldest lineages.

- n_oldest:

  Integer. Number of oldest lineages to return (sorted by birth year).

- type:

  Character vector. Specifies which summaries to compute. Options:
  \`"fathers"\`, \`"mothers"\`, \`"families"\`. Default includes all
  three.

- verbose:

  Logical, if TRUE, print progress messages.

## Value

A data.table containing the summary statistics for all numeric
variables.
