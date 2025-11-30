# Summarize Pedigree Data

This function summarizes pedigree data, by computing key summary
statistics for all numeric variables and identifying the originating
member (founder) for each family, maternal, and paternal lineage.

## Usage

``` r
summarizePedigrees(
  ped,
  famID = "famID",
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  matID = "matID",
  patID = "patID",
  type = c("fathers", "mothers", "families"),
  byr = NULL,
  include_founder = FALSE,
  founder_sort_var = NULL,
  n_keep = 5,
  n_biggest = n_keep,
  n_oldest = n_keep,
  skip_var = NULL,
  five_num_summary = FALSE,
  network_checks = FALSE,
  verbose = FALSE
)

summarisePedigrees(
  ped,
  famID = "famID",
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  matID = "matID",
  patID = "patID",
  type = c("fathers", "mothers", "families"),
  byr = NULL,
  include_founder = FALSE,
  founder_sort_var = NULL,
  n_keep = 5,
  n_biggest = n_keep,
  n_oldest = n_keep,
  skip_var = NULL,
  five_num_summary = FALSE,
  network_checks = FALSE,
  verbose = FALSE
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

- type:

  Character vector. Specifies which summaries to compute. Options:
  \`"fathers"\`, \`"mothers"\`, \`"families"\`. Default includes all
  three.

- byr:

  Character. Optional column name for birth year. Used to determine the
  oldest lineages.

- include_founder:

  Logical. If \`TRUE\`, includes the founder (originating member) of
  each lineage in the output.

- founder_sort_var:

  Character. Column used to determine the founder of each lineage.
  Defaults to \`byr\` (if available) or \`personID\` otherwise.

- n_keep:

  Integer. Number of lineages to keep in the output for each type of
  summary.

- n_biggest:

  Integer. Number of largest lineages to return (sorted by count).

- n_oldest:

  Integer. Number of oldest lineages to return (sorted by birth year).

- skip_var:

  Character vector. Variables to exclude from summary calculations.

- five_num_summary:

  Logical. If \`TRUE\`, includes the first quartile (Q1) and third
  quartile (Q3) in addition to the minimum, median, and maximum values.

- network_checks:

  Logical. If \`TRUE\`, performs network checks on the pedigree data.

- verbose:

  Logical, if TRUE, print progress messages.

## Value

A data.frame (or list) containing summary statistics for family,
maternal, and paternal lines, as well as the 5 oldest and biggest lines.

## Details

The function calculates standard descriptive statistics, including the
count of individuals in each lineage, means, medians, minimum and
maximum values, and standard deviations. Additionally, if
\`five_num_summary = TRUE\`, the function includes the first and third
quartiles (Q1, Q3) to provide a more detailed distributional summary.
Users can also specify variables to exclude from the analysis via
\`skip_var\`.

Beyond summary statistics, the function identifies the founding member
of each lineage based on the specified sorting variable
(\`founder_sort_var\`), defaulting to birth year (\`byr\`) when
available or \`personID\` otherwise. Users can retrieve the largest and
oldest lineages by setting \`n_fooest\` and \`n_oldest\`, respectively.
