# Load or compute the isChild matrix

Load or compute the isChild matrix

## Usage

``` r
.loadOrComputeIsChild(ped, checkpoint_files, config, compress = TRUE)
```

## Arguments

- ped:

  a pedigree dataset. Needs ID, momID, and dadID columns

- checkpoint_files:

  A list of checkpoint file paths.

  @keywords internal

- config:

  A list containing configuration parameters such as \`resume\`,
  \`verbose\`, and \`saveable\`.

- compress:

  a logical specifying whether saving to a named file is to use "gzip"
  compression, or one of "gzip", "bzip2", "xz" or "zstd" to indicate the
  type of compression to be used. Ignored if file is a connection.
