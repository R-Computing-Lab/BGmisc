# Subset output to requested IDs

Subset output to requested IDs

## Usage

``` r
.subsetKeepIds(
  component,
  keep_ids = NULL,
  available_ids,
  config,
  verbose_message = "Subsetting to %d target individuals\n",
  drop = FALSE
)
```

## Arguments

- component:

  A component to subset.

- keep_ids:

  Character vector of IDs to retain.

- available_ids:

  Character vector of IDs available in `x`.

- verbose_message:

  Character. Message prefix to print when `config$verbose == TRUE`.

- drop:

  logical. Passed to `[` when subsetting matrices.
