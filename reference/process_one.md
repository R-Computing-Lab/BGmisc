# Convert Sparse Relationship Matrices to Kinship Links for one Matrix

Convert Sparse Relationship Matrices to Kinship Links for one Matrix

## Usage

``` r
process_one(
  matrix,
  rel_name,
  ids,
  nc,
  rel_pairs_file,
  writetodisk,
  write_buffer_size,
  drop_upper_triangular,
  update_rate,
  verbose,
  gc,
  include_all_links = TRUE,
  ...
)
```

## Arguments

- rel_pairs_file:

  File path to write related pairs to (CSV format).

- writetodisk:

  Logical. If TRUE, writes the related pairs to disk; if FALSE, returns
  a data frame.

- write_buffer_size:

  Number of related pairs to write to disk at a time.

- drop_upper_triangular:

  Logical. If TRUE, drops the upper triangular portion of the matrix.

- update_rate:

  Numeric. Frequency (in iterations) at which progress messages are
  printed.

- verbose:

  Logical. If TRUE, prints progress messages.

- gc:

  Logical. If TRUE, performs garbage collection via
  [`gc`](https://rdrr.io/r/base/gc.html) to free memory.

- include_all_links:

  Logical. If TRUE, all links are included in the output.

- ...:

  Additional arguments to be passed to
  [`com2links`](https://r-computing-lab.github.io/BGmisc/reference/com2links.md)
