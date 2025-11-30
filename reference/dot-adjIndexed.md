# Construct Adjacency Matrix for Parent-Child Relationships Using Indexed Method

Construct Adjacency Matrix for Parent-Child Relationships Using Indexed
Method

## Usage

``` r
.adjIndexed(
  ped,
  component,
  saveable,
  resume,
  save_path,
  verbose,
  lastComputed,
  checkpoint_files,
  update_rate,
  parList,
  lens,
  save_rate_parlist,
  config,
  compress = config$compress
)
```

## Arguments

- ped:

  a pedigree dataset. Needs ID, momID, and dadID columns

- component:

  character. Which component of the pedigree to return. See Details.

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

  numeric. The rate at which to print progress

- parList:

  a list of parent-child relationships

- lens:

  a vector of the lengths of the parent-child relationships

- save_rate_parlist:

  numeric. The rate at which to save the intermediate results by parent
  list. If NULL, defaults to save_rate\*1000

- config:

  a configuration list that passes parameters to the function

- compress:

  logical. If TRUE, use compression when saving the checkpoint files.
  Defaults to TRUE.
