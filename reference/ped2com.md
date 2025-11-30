# Take a pedigree and turn it into a relatedness matrix

Take a pedigree and turn it into a relatedness matrix

## Usage

``` r
ped2com(
  ped,
  component,
  max_gen = 25,
  sparse = TRUE,
  verbose = FALSE,
  gc = FALSE,
  flatten_diag = FALSE,
  standardize_colnames = TRUE,
  transpose_method = "tcrossprod",
  adjacency_method = "direct",
  isChild_method = "classic",
  saveable = FALSE,
  resume = FALSE,
  save_rate = 5,
  save_rate_gen = save_rate,
  save_rate_parlist = 1e+05 * save_rate,
  update_rate = 100,
  save_path = "checkpoint/",
  adjBeta_method = NULL,
  compress = TRUE,
  ...
)
```

## Arguments

- ped:

  a pedigree dataset. Needs ID, momID, and dadID columns

- component:

  character. Which component of the pedigree to return. See Details.

- max_gen:

  the maximum number of generations to compute (e.g., only up to 4th
  degree relatives). The default is 25. However it can be set to
  infinity. \`Inf\` uses as many generations as there are in the data.

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
  "tcrossprod", "crossprod", or "star"

- adjacency_method:

  character. The method to use for computing the adjacency matrix.
  Options are "loop", "indexed", direct or beta

- isChild_method:

  character. The method to use for computing the isChild matrix. Options
  are "classic" or "partialparent"

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

- update_rate:

  numeric. The rate at which to print progress

- save_path:

  character. The path to save the checkpoint files

- adjBeta_method:

  numeric The method to use for computing the building the
  adjacency_method matrix when using the "beta" build

- compress:

  logical. If TRUE, use compression when saving the checkpoint files.
  Defaults to TRUE.

- ...:

  additional arguments to be passed to `ped2com`

## Details

The algorithms and methodologies used in this function are further
discussed and exemplified in the vignette titled
"examplePedigreeFunctions". For more advanced scenarios and detailed
explanations, consult this vignette.
