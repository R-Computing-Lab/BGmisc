# Load or compute a checkpoint

Load or compute a checkpoint

## Usage

``` r
loadOrComputeCheckpoint(
  file,
  compute_fn,
  config,
  message_resume = NULL,
  message_compute = NULL,
  compress = TRUE
)
```

## Arguments

- file:

  The file path to load the checkpoint from.

- compute_fn:

  The function to compute the checkpoint if it doesn't exist.

- config:

  A list containing configuration parameters such as \`resume\`,
  \`verbose\`, and \`saveable\`.

- message_resume:

  Optional message to display when resuming from a checkpoint.

- message_compute:

  Optional message to display when computing the checkpoint.

- compress:

  a logical specifying whether saving to a named file is to use "gzip"
  compression, or one of "gzip", "bzip2", "xz" or "zstd" to indicate the
  type of compression to be used. Ignored if file is a connection.

## Value

The loaded or computed checkpoint.
