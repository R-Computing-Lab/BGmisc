# Trace paths between individuals in a family tree grid

Trace paths between individuals in a family tree grid

## Usage

``` r
traceTreePaths(tree_long, deduplicate = TRUE)
```

## Arguments

- tree_long:

  A data.frame with columns: Row, Column, Value, id

- deduplicate:

  Logical, if TRUE, will remove duplicate paths

## Value

A data.frame with columns: from_id, to_id, direction, path_length,
intermediates
