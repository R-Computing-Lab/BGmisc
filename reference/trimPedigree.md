# Iteratively Trim Leaf Nodes from a Pedigree

Repeatedly removes structural leaf nodes from a pedigree until no
further trimming is possible or a stopping condition is reached. After
each removal pass, parent ID columns are updated so that references to
removed individuals are set to `NA`.

## Usage

``` r
trimPedigree(
  ped,
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  include_terminal = TRUE,
  include_founder_singletons = TRUE,
  max_iter = Inf,
  min_size = 0L,
  remove_ids = NULL,
  keep_var = NULL,
  keep_vals = NULL,
  verbose = FALSE
)
```

## Arguments

- ped:

  a pedigree dataset. Needs ID, momID, and dadID columns

- personID:

  character. Name of the column in ped for the person ID variable

- momID:

  character. Name of the column in ped for the mother ID variable

- dadID:

  character. Name of the column in ped for the father ID variable

- include_terminal:

  Logical. If `TRUE` (default), flag individuals with no children
  (outdegree 0) as leaves.

- include_founder_singletons:

  Logical. If `TRUE` (default), also flag founders with exactly one
  child (indegree 0, outdegree 1) as leaves.

- max_iter:

  Integer or `Inf`. Maximum number of trimming iterations. Defaults to
  `Inf`, which trims until no other stopping condition applies.

- min_size:

  Integer. Minimum number of individuals to retain. Trimming stops
  before any removal that would reduce the pedigree below this size.
  Defaults to `0L`.

- remove_ids:

  Character vector of additional individual IDs to remove before any
  leaf-based trimming. Defaults to `NULL`.

- keep_var:

  Character. Optional column name of a phenotypic variable. Passed to
  [`findLeaves`](https://r-computing-lab.github.io/BGmisc/reference/findLeaves.md)
  at every iteration so that individuals with protected phenotype values
  are never removed.

- keep_vals:

  Optional vector of phenotype values that protect an individual from
  removal. See
  [`findLeaves`](https://r-computing-lab.github.io/BGmisc/reference/findLeaves.md)
  for full details.

- verbose:

  Logical. If `TRUE`, print counts of each leaf type.

## Value

A trimmed pedigree `data.frame` with the same columns as the input.
Parent ID columns (`momID`, `dadID`) are updated to `NA` for any
references to removed individuals.

## Details

The trimming process peels the pedigree from the outside in: first
removing the outermost leaves, then re-evaluating the remaining
structure so that newly exposed leaves can be removed in subsequent
iterations.

Iteration stops when any of the following conditions is met:

- No leaf nodes remain.

- The number of iterations reaches `max_iter`.

- Removing the next batch of leaves would reduce the pedigree below
  `min_size` rows.

## See also

[`findLeaves`](https://r-computing-lab.github.io/BGmisc/reference/findLeaves.md)
to preview which individuals would be removed.

## Examples

``` r
if (FALSE) { # \dontrun{
ped <- data.frame(
  ID    = 1:6,
  dadID = c(NA, NA, 1, 1, 3, NA),
  momID = c(NA, NA, 2, 2, 4, NA)
)
trimPedigree(ped, verbose = TRUE)
trimPedigree(ped, min_size = 2, verbose = TRUE)
} # }
```
