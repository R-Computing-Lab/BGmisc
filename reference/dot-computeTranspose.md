# Compute the transpose multiplication for the relatedness matrix

Compute the transpose multiplication for the relatedness matrix

## Usage

``` r
.computeTranspose(
  r2,
  transpose_method = "tcrossprod",
  chunk_size = 1000L,
  verbose = FALSE,
  force_symmetric = FALSE,
  config = NULL,
  checkpoint_files = NULL
)
```

## Arguments

- r2:

  a relatedness matrix

- transpose_method:

  character. The method to use for computing the transpose. Options are
  "tcrossprod", "crossprod", "star", or "chunked"

- chunk_size:

  numeric. If greater than 1 is Number of rows per chunk when
  `transpose_method = "chunked"`. Defaults to 1000. If less than or
  equal to 1, the entire matrix is processed in a single chunk.

- verbose:

  logical. If TRUE, print progress through stages of algorithm

- force_symmetric:

  logical. If TRUE, forces the output matrix to be symmetric using
  Matrix::forceSymmetric. This can be helpful when using chunked
  multiplication to ensure the final result is exactly symmetric despite
  potential numerical issues. Defaults to TRUE.

- config:

  list of configuration parameters, used for checkpointing and verbose
  output

- checkpoint_files:

  list of checkpoint file paths for saving intermediate results when
  using chunked multiplication. Should contain at least
  'tcrossprod_checkpoint' for saving the chunks and 'tcrossprod_ids' for
  saving the keep_ids used in the tcrossprod checkpoint.

## Details

The algorithms and methodologies used in this function are further
discussed and exemplified in the vignette titled
"examplePedigreeFunctions". For more advanced scenarios and detailed
explanations, consult this vignette.
