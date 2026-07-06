# Post-process GEDCOM Data Frame

This function optionally adds parent information, combines duplicate
columns, and removes empty columns from the GEDCOM data frame. It is
called by
[`readGedcom()`](https://r-computing-lab.github.io/BGmisc/reference/readGedcom.md)
if `post_process = TRUE`.

## Usage

``` r
postProcessGedcom(
  df_temp,
  remove_empty_cols = TRUE,
  combine_cols = TRUE,
  add_parents = TRUE,
  parse_dates = FALSE,
  clean_names = TRUE,
  skinny = TRUE,
  verbose = FALSE
)
```

## Arguments

- df_temp:

  A data frame produced by
  [`readGedcom()`](https://r-computing-lab.github.io/BGmisc/reference/readGedcom.md).

- remove_empty_cols:

  Logical indicating whether to remove columns that are entirely
  missing.

- combine_cols:

  Logical. If \`TRUE\`, combine redundant name columns, such as
  \`name_given\` with \`name_given_pieces\` and \`name_surn\` with
  \`name_surn_pieces\`, when their values do not conflict.

- add_parents:

  Logical. If \`TRUE\`, infer \`momID\` and \`dadID\` from \`FAMC\` and
  \`FAMS\` mappings during post-processing.

- parse_dates:

  Logical. If \`TRUE\`, attempt to parse date columns (e.g.,
  \`birth_date\`, \`death_date\`) into Date objects, after removing
  common GEDCOM date qualifiers like "ABT", "BEF", and "AFT".

- clean_names:

  Logical indicating whether to clean name columns by removing trailing
  slashes and squishing whitespace.

- skinny:

  Logical. If \`TRUE\`, return a slimmer data frame by dropping
  \`FAMC\`, \`FAMS\`, and columns that are entirely \`NA\` during
  post-processing.

- verbose:

  Logical indicating whether to print progress messages.

## Value

The post-processed data frame.
