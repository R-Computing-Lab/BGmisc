# Add a focal-person common nuclear relatedness column to a pedigree

Add a focal-person common nuclear relatedness column to a pedigree

## Usage

``` r
ped2cnFocal(
  ped,
  focal_id,
  personID = "ID",
  col_name = NULL,
  max_gen = 25,
  sparse = TRUE,
  verbose = FALSE,
  gc = FALSE,
  flatten_diag = FALSE,
  standardize_colnames = TRUE,
  transpose_method = "tcrossprod",
  chunk_size = 1000L,
  keep_ids = NULL,
  adjacency_method = "direct",
  saveable = FALSE,
  resume = FALSE,
  save_rate = 5,
  save_rate_gen = save_rate,
  save_rate_parlist = 1e+05 * save_rate,
  save_path = "checkpoint/",
  compress = TRUE,
  force_symmetric = FALSE,
  ...
)
```

## Arguments

- ped:

  A pedigree data frame with at minimum columns for individual ID,
  mother ID, and father ID.

- focal_id:

  The ID of the target individual. Must match a value in the `personID`
  column of `ped`.

- personID:

  Character. Name of the individual ID column. Default `"ID"`.

- col_name:

  Character. Name of the new column added to `ped`. Defaults to
  `paste0(component, "Rel_", focal_id)`.

- max_gen:

  the maximum number of iterations that the adjacency matrix is
  multiplied to get the relatedness matrix. \`Inf\` uses as many
  iterations as there are in the data. Defaults to 25.

- sparse:

  logical. If TRUE, use and return sparse matrices from Matrix package

- verbose:

  logical. If TRUE, print progress through stages of algorithm

- gc:

  logical. If TRUE, do frequent garbage collection via
  [`gc`](https://rdrr.io/r/base/gc.html) to save memory

- flatten_diag:

  logical. If TRUE, overwrite the diagonal of the final relatedness
  matrix with ones

- standardize_colnames:

  logical. If TRUE, standardize the column names of the pedigree dataset

- transpose_method:

  character. The method to use for computing the transpose. Options are
  "tcrossprod", "crossprod", "star", or "chunked"

- chunk_size:

  numeric. If greater than 1 is Number of rows per chunk when
  `transpose_method = "chunked"`. Defaults to 1000. If less than or
  equal to 1, the entire matrix is processed in a single chunk.

- keep_ids:

  character vector of IDs to retain in the final relatedness matrix.
  When supplied, only the rows of `r2` corresponding to these IDs are
  used in the tcrossprod, so the result is a
  `length(keep_ids) x length(keep_ids)` matrix. All columns of `r2` are
  retained during the multiplication so relatedness values remain
  correct. IDs not found in the pedigree are silently dropped with a
  warning.

- adjacency_method:

  character. The method to use for computing the adjacency matrix.
  Options are "loop", "indexed", direct or beta

- saveable:

  logical. If TRUE, save the intermediate results to disk

- resume:

  logical. If TRUE, resume from a checkpoint

- save_rate:

  numeric. The rate at which to save the intermediate results

- save_rate_gen:

  numeric. The rate at which to save the intermediate results by
  generation. If NULL, defaults to save_rate

- save_rate_parlist:

  numeric. The rate at which to save the intermediate results by parent
  list. If NULL, defaults to save_rate\*1000

- save_path:

  character. The path to save the checkpoint files

- compress:

  logical. If TRUE, use compression when saving the checkpoint files.
  Defaults to TRUE.

- force_symmetric:

  logical. If TRUE, force the final relatedness matrix to be symmetric.
  This can help mitigate any numerical asymmetry introduced by the
  transpose method, especially when using sparse matrices. Defaults to
  TRUE.

- ...:

  additional arguments to be passed to
  [`ped2com`](https://r-computing-lab.github.io/BGmisc/reference/ped2com.md)

## See also

[`ped2focal`](https://r-computing-lab.github.io/BGmisc/reference/ped2focal.md)
