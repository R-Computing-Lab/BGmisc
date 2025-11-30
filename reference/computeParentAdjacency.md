# Compute Parent Adjacency Matrix with Multiple Approaches

Compute Parent Adjacency Matrix with Multiple Approaches

## Usage

``` r
computeParentAdjacency(
  ped,
  component,
  adjacency_method = "direct",
  saveable,
  resume,
  save_path,
  verbose = FALSE,
  lastComputed = 0,
  checkpoint_files,
  update_rate,
  parList,
  lens,
  save_rate_parlist,
  adjBeta_method = NULL,
  config,
  compress = config$compress,
  ...
)
```

## Arguments

- ped:

  a pedigree dataset. Needs ID, momID, and dadID columns

- component:

  character. Which component of the pedigree to return. See Details.

- adjacency_method:

  character. The method to use for computing the adjacency matrix.
  Options are "loop", "indexed", direct or beta

- saveable:

  logical. If TRUE, save the intermediate results to disk

- resume:

  logical. If TRUE, resume from a checkpoint

- save_path:

  character. The path to save the checkpoint files

- verbose:

  logical. If TRUE, print progress through stages of algorithm

- lastComputed:

  the last computed index

- checkpoint_files:

  a list of checkpoint files

- update_rate:

  the rate at which to update the progress

- parList:

  a list of parent-child relationships

- lens:

  a vector of the lengths of the parent-child relationships

- save_rate_parlist:

  numeric. The rate at which to save the intermediate results by parent
  list. If NULL, defaults to save_rate\*1000

- adjBeta_method:

  numeric The method to use for computing the building the
  adjacency_method matrix when using the "beta" build

- config:

  a configuration list that passes parameters to the function

- compress:

  logical. If TRUE, use compression when saving the checkpoint files.
  Defaults to TRUE.

- ...:

  additional arguments to be passed to
  [`ped2com`](https://r-computing-lab.github.io/BGmisc/reference/ped2com.md)

## Details

The algorithms and methodologies used in this function are further
discussed and exemplified in the vignette titled
"examplePedigreeFunctions". For more advanced scenarios and detailed
explanations, consult this vignette.
