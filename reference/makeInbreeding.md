# makeInbreeding

A function to create inbred mates in the simulated pedigree
`data.frame`. Inbred mates can be created by specifying their IDs or the
generation the inbred mate should be created. When specifying the
generation, inbreeding between siblings or 1st cousin needs to be
specified. This is a supplementary function for `simulatePedigree`.

## Usage

``` r
makeInbreeding(
  ped,
  ID_mate1 = NA_integer_,
  ID_mate2 = NA_integer_,
  verbose = FALSE,
  gen_inbred = 2,
  type_inbred = "sib"
)
```

## Arguments

- ped:

  A `data.frame` in the same format as the output of `simulatePedigree`.

- ID_mate1:

  A vector of `ID` of the first mate. If not provided, the function will
  randomly select two individuals from the second generation.

- ID_mate2:

  A vector of `ID` of the second mate.

- verbose:

  logical. If TRUE, print progress through stages of algorithm

- gen_inbred:

  A vector of `generation` of the twin to be imputed.

- type_inbred:

  A character vector indicating the type of inbreeding. "sib" for
  sibling inbreeding and "cousin" for cousin inbreeding.

## Value

Returns a `data.frame` with some inbred mates.

## Details

This function creates inbred mates in the simulated pedigree
`data.frame`. This function's purpose is to evaluate the effect of
inbreeding on model fitting and parameter estimation. In case it needs
to be said, we do not condone inbreeding in real life. But we recognize
that it is a common practice in some fields to create inbred strains for
research purposes.
