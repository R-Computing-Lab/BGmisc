# Construct Adjacency Matrix for Parent-Child Relationships Using Beta Method This function constructs an adjacency matrix for parent-child relationships using a method in beta testing. It identifies parent-child pairs based on the specified component of relatedness.

Construct Adjacency Matrix for Parent-Child Relationships Using Beta
Method This function constructs an adjacency matrix for parent-child
relationships using a method in beta testing. It identifies parent-child
pairs based on the specified component of relatedness.

## Usage

``` r
.adjBeta(
  ped,
  component,
  adjBeta_method = 5,
  parList = NULL,
  lastComputed = 0,
  lens = NULL,
  saveable = FALSE,
  resume = FALSE,
  save_path = NULL,
  verbose = FALSE,
  save_rate_parlist = NULL,
  update_rate = NULL,
  checkpoint_files = NULL,
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

- adjBeta_method:

  numeric The method to use for computing the building the
  adjacency_method matrix when using the "beta" build

- parList:

  a list of parent-child relationships

- lastComputed:

  the last computed index

- lens:

  a vector of the lengths of the parent-child relationships

- saveable:

  logical. If TRUE, save the intermediate results to disk

- resume:

  logical. If TRUE, resume from a checkpoint

- save_path:

  character. The path to save the checkpoint files

- verbose:

  logical. If TRUE, print progress through stages of algorithm

- save_rate_parlist:

  numeric. The rate at which to save the intermediate results by parent
  list. If NULL, defaults to save_rate\*1000

- update_rate:

  numeric. The rate at which to print progress

- checkpoint_files:

  a list of checkpoint files

- config:

  a configuration list that passes parameters to the function

- compress:

  logical. If TRUE, use compression when saving the checkpoint files.
  Defaults to TRUE.

- ...:

  additional arguments to be passed to
  [`ped2com`](https://r-computing-lab.github.io/BGmisc/reference/ped2com.md)
