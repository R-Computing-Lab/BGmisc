# Summarize the maternal lines in a pedigree

Summarize the maternal lines in a pedigree

## Usage

``` r
summarizeMatrilines(
  ped,
  famID = "famID",
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  matID = "matID",
  patID = "patID",
  byr = NULL,
  include_founder = FALSE,
  founder_sort_var = NULL,
  n_biggest = 5,
  n_oldest = 5,
  skip_var = NULL,
  five_num_summary = FALSE,
  verbose = FALSE,
  network_checks = FALSE
)

summariseMatrilines(
  ped,
  famID = "famID",
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  matID = "matID",
  patID = "patID",
  byr = NULL,
  include_founder = FALSE,
  founder_sort_var = NULL,
  n_biggest = 5,
  n_oldest = 5,
  skip_var = NULL,
  five_num_summary = FALSE,
  verbose = FALSE,
  network_checks = FALSE
)
```

## Arguments

- ped:

  a pedigree dataset. Needs ID, momID, and dadID columns

- famID:

  character. Name of the column to be created in ped for the family ID
  variable

- personID:

  character. Name of the column in ped for the person ID variable

- momID:

  character. Name of the column in ped for the mother ID variable

- dadID:

  character. Name of the column in ped for the father ID variable

- matID:

  Character. Maternal line ID variable to be created and added to the
  pedigree

- patID:

  Character. Paternal line ID variable to be created and added to the
  pedigree

- byr:

  Character. Optional column name for birth year. Used to determine the
  oldest lineages.

- include_founder:

  Logical. If \`TRUE\`, includes the founder (originating member) of
  each lineage in the output.

- founder_sort_var:

  Character. Column used to determine the founder of each lineage.
  Defaults to \`byr\` (if available) or \`personID\` otherwise.

- n_biggest:

  Integer. Number of largest lineages to return (sorted by count).

- n_oldest:

  Integer. Number of oldest lineages to return (sorted by birth year).

- skip_var:

  Character vector. Variables to exclude from summary calculations.

- five_num_summary:

  Logical. If \`TRUE\`, includes the first quartile (Q1) and third
  quartile (Q3) in addition to the minimum, median, and maximum values.

- verbose:

  Logical, if TRUE, print progress messages.

- network_checks:

  Logical. If \`TRUE\`, performs network checks on the pedigree data.

## See also

\[summarizePedigrees ()\]
