# Convert Pedigree Matrices to Related Pairs File (Legacy)

This legacy function converts pedigree matrices into a related pairs
file.

## Usage

``` r
.com2links.og(
  rel_pairs_file = "dataRelatedPairs.csv",
  ad_ped_matrix = NULL,
  mit_ped_matrix = mt_ped_matrix,
  mt_ped_matrix = NULL,
  cn_ped_matrix = NULL,
  update_rate = 500,
  verbose = FALSE,
  outcome_name = "data",
  ...
)
```

## Arguments

- rel_pairs_file:

  File path to write related pairs to (CSV format).

- ad_ped_matrix:

  Matrix of additive genetic relatedness coefficients.

- mit_ped_matrix:

  Matrix of mitochondrial relatedness coefficients. Alias:
  `mt_ped_matrix`.

- mt_ped_matrix:

  Matrix of mitochondrial relatedness coefficients.

- cn_ped_matrix:

  Matrix of common nuclear relatedness coefficients.

- update_rate:

  Numeric. Frequency (in iterations) at which progress messages are
  printed.

- verbose:

  Logical. If TRUE, prints progress messages.

- outcome_name:

  Character string representing the outcome name (used in file naming).

- ...:

  Additional arguments to be passed to
  [`com2links`](https://r-computing-lab.github.io/BGmisc/reference/com2links.md)
