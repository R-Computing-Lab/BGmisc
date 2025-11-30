# Load or compute the inverse diagonal matrix

Load or compute the inverse diagonal matrix

## Usage

``` r
.loadOrComputeInverseDiagonal(
  r,
  isChild,
  checkpoint_files,
  config,
  compress = TRUE
)
```

## Arguments

- r:

  The relatedness matrix.

- config:

  A list containing configuration parameters such as \`resume\`,
  \`verbose\`, and \`saveable\`.

- compress:

  a logical specifying whether saving to a named file is to use "gzip"
  compression, or one of "gzip", "bzip2", "xz" or "zstd" to indicate the
  type of compression to be used. Ignored if file is a connection.

## Value

The computed inverse diagonal matrix.
