# Add a paternal-line descendant flag to a pedigree

Adds a logical flag indicating whether a specified anchor individual
appears anywhere in each person's ordered paternal ancestor chain.

## Usage

``` r
addPaternalLineFlag(ped, anchor_id, flag_col, chain_col = "dadID_chain")
```

## Arguments

- ped:

  A pedigree data frame containing a parental chain list-column.

- anchor_id:

  ID of the anchor individual to search for within each person's
  parental chain.

- flag_col:

  Character string giving the name of the logical output column to add
  to \`ped\`.

- chain_col:

  Character string giving the name of the list-column containing ordered
  parental ancestor chains.

## Value

A data frame with \`flag_col\` added. The flag is \`TRUE\` when
\`anchor_id\` appears in the individual's paternal chain and \`FALSE\`
otherwise.

## Details

This is a convenience wrapper around \[addParentalLineFlag()\] with
\`component = "dadID"\`.
