# Convert Sparse Relationship Matrices to Kinship Links

Convert Sparse Relationship Matrices to Kinship Links

## Usage

``` r
.com2links.legacy(
  rel_pairs_file = "dataRelatedPairs.csv",
  ad_ped_matrix = NULL,
  mit_ped_matrix = mt_ped_matrix,
  mt_ped_matrix = NULL,
  cn_ped_matrix = NULL,
  write_buffer_size = 1000,
  update_rate = 1000,
  gc = TRUE,
  writetodisk = TRUE,
  verbose = FALSE,
  legacy = FALSE,
  outcome_name = "data",
  drop_upper_triangular = TRUE,
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

- write_buffer_size:

  Number of related pairs to write to disk at a time.

- update_rate:

  Numeric. Frequency (in iterations) at which progress messages are
  printed.

- gc:

  Logical. If TRUE, performs garbage collection via
  [`gc`](https://rdrr.io/r/base/gc.html) to free memory.

- writetodisk:

  Logical. If TRUE, writes the related pairs to disk; if FALSE, returns
  a data frame.

- verbose:

  Logical. If TRUE, prints progress messages.

- legacy:

  Logical. If TRUE, uses the legacy branch of the function.

- outcome_name:

  Character string representing the outcome name (used in file naming).

- drop_upper_triangular:

  Logical. If TRUE, drops the upper triangular portion of the matrix.

- ...:

  Additional arguments to be passed to
  [`com2links`](https://r-computing-lab.github.io/BGmisc/reference/com2links.md)
