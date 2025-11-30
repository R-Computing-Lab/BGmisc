# parent-child adjacency data

parent-child adjacency data

## Usage

``` r
.loadOrComputeParList(
  checkpoint_files,
  config,
  ped = NULL,
  parList = NULL,
  lens = NULL,
  compress = TRUE
)
```

## Arguments

- checkpoint_files:

  A list of checkpoint file paths.

- config:

  A list containing configuration parameters such as \`resume\`,
  \`verbose\`, and \`saveable\`.

- ped:

  a pedigree dataset. Needs ID, momID, and dadID columns

- parList:

  A list of parent-child adjacency data.

- lens:

  A vector of lengths for each parent-child relationship.

- compress:

  a logical specifying whether saving to a named file is to use "gzip"
  compression, or one of "gzip", "bzip2", "xz" or "zstd" to indicate the
  type of compression to be used. Ignored if file is a connection.

## Value

A list containing the parent-child adjacency data either loaded from a
checkpoint or initialized.
