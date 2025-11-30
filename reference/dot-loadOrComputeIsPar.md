# Load or compute the isPar matrix

Load or compute the isPar matrix

## Usage

``` r
.loadOrComputeIsPar(
  iss,
  jss,
  parVal,
  ped,
  checkpoint_files,
  config,
  compress = TRUE
)
```

## Arguments

- iss:

  The row indices of the sparse matrix.

- jss:

  The column indices of the sparse matrix.

- parVal:

  The value to assign to the non-zero elements of the sparse matrix.

- ped:

  The pedigree dataset.

- checkpoint_files:

  A list of checkpoint file paths.

- config:

  A list containing configuration parameters such as \`resume\`,
  \`verbose\`, and \`saveable\`.

- compress:

  a logical specifying whether saving to a named file is to use "gzip"
  compression, or one of "gzip", "bzip2", "xz" or "zstd" to indicate the
  type of compression to be used. Ignored if file is a connection.
