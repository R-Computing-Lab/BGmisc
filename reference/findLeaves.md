# Find Leaf Nodes in a Pedigree

Identifies individuals who are structural "leaves" in the pedigree
network — those who can potentially be removed without substantially
altering the connectivity of the larger tree.

## Usage

``` r
findLeaves(
  ped,
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  include_terminal = TRUE,
  include_founder_singletons = TRUE,
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

- keep_var:

  Character. Optional column name of a phenotypic variable. When
  supplied, individuals are protected from removal based on their value
  in this column (see `keep_vals`).

- keep_vals:

  Optional vector of values in `keep_var` that protect an individual
  from being flagged as a leaf. If `NULL` (default) and `keep_var` is
  supplied, any individual with a *non-missing* value is protected. To
  protect individuals with missing data instead, pass `keep_vals = NA`.

- verbose:

  Logical. If `TRUE`, print counts of each leaf type.

## Value

A character vector of person IDs that are leaf nodes.

## Details

Two types of leaves are identified:

- **Terminal nodes**: individuals with outdegree 0, meaning they have no
  children recorded in the pedigree. Controlled by `include_terminal`.

- **Founder singletons**: individuals with indegree 0 *and* outdegree 1,
  meaning they are founders (no recorded parents) who appear as a parent
  of exactly one child. Controlled by `include_founder_singletons`.

In the directed pedigree graph used by
[`ped2graph`](https://r-computing-lab.github.io/BGmisc/reference/ped2graph.md),
edges run from **parent to child**. Consequently, indegree reflects the
number of recorded parents and outdegree reflects the number of recorded
children.

## See also

[`trimPedigree`](https://r-computing-lab.github.io/BGmisc/reference/trimPedigree.md)
to iteratively remove the identified leaves.

## Examples

``` r
if (FALSE) { # \dontrun{
ped <- data.frame(
  ID    = 1:6,
  dadID = c(NA, NA, 1, 1, 3, NA),
  momID = c(NA, NA, 2, 2, 4, NA)
)
findLeaves(ped)
} # }
```
