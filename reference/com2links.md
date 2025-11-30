# Convert Sparse Relationship Matrices to Kinship Links

This function processes one or more sparse relationship components
(additive, mitochondrial, and common nuclear) and converts them into
kinship link pairs. The resulting related pairs are either returned as a
data frame or written to disk in CSV format.

## Usage

``` r
com2links(
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
  include_all_links_1ped = FALSE,
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

- include_all_links_1ped:

  Logical. If TRUE, includes all links in the output. (Default is true
  when only one ped is provided)

- ...:

  Additional arguments to be passed to `com2links`

## Value

A data frame of related pairs if `writetodisk` is FALSE; otherwise,
writes the results to disk.
