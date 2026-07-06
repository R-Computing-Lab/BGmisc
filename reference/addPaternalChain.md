# This is a convenience wrapper around \[addParentalChain()\] with \`component = "dadID"\`.

This is a convenience wrapper around \[addParentalChain()\] with
\`component = "dadID"\`.

## Usage

``` r
addPaternalChain(
  ped,
  personID = "personID",
  dadID = "dadID",
  momID = "momID",
  chain_col = "dadID_chain",
  chain_string_col = "dadID_chain_string",
  collapse = "|",
  traversal_direction = "in"
)
```

## Arguments

- ped:

  A pedigree data frame.

- personID:

  Character string giving the name of the column containing individual
  IDs.

- dadID:

  Character string giving the name of the column containing paternal
  IDs.

- momID:

  Character string giving the name of the column containing maternal
  IDs.

- chain_col:

  Character string giving the name of the output list-column that will
  contain the ordered parental ancestor chain for each individual.

- chain_string_col:

  Character string giving the name of the output character column that
  will contain the collapsed parental ancestor chain.

- collapse:

  Character string used to collapse ancestor IDs into
  \`chain_string_col\`.

- traversal_direction:

  Character giving the mode of transversion: defaults to "in". If set to
  out, will procedure a list of descendants.

## Value

A data frame with two added columns:

- \`chain_col\`:

  A list-column containing the ordered paternal ancestor chain for each
  individual.

- \`chain_string_col\`:

  A character column containing the collapsed paternal ancestor chain,
  or \`NA_character\_\` when no paternal ancestors are found.
