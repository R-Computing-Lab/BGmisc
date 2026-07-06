# Add maternal ancestor chains to a pedigree

Adds an ordered maternal ancestor chain for each individual in a
pedigree. The chain follows only mother-to-mother links, so the
resulting chain is: mother, maternal grandmother, maternal
great-grandmother, and so on.

## Usage

``` r
addMaternalChain(
  ped,
  personID = "personID",
  dadID = "dadID",
  momID = "momID",
  chain_col = "momID_chain",
  chain_string_col = "momID_chain_string",
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

  A list-column containing the ordered maternal ancestor chain for each
  individual.

- \`chain_string_col\`:

  A character column containing the collapsed maternal ancestor chain,
  or \`NA_character\_\` when no maternal ancestors are found.

## Details

This is a convenience wrapper around \[addParentalChain()\] with
\`component = "momID"\`.
