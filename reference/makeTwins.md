# makeTwins

A function to impute twins in the simulated pedigree `data.frame`. Twins
can be imputed by specifying their IDs or by specifying the generation
the twin should be imputed. This is a supplementary function for
`simulatePedigree`.

## Usage

``` r
makeTwins(
  ped,
  ID_twin1 = NA_integer_,
  ID_twin2 = NA_integer_,
  gen_twin = 2,
  verbose = FALSE,
  zygosity = "MZ"
)
```

## Arguments

- ped:

  A `data.frame` in the same format as the output of `simulatePedigree`.

- ID_twin1:

  A vector of `ID` of the first twin.

- ID_twin2:

  A vector of `ID` of the second twin.

- gen_twin:

  A vector of `generation` of the twin to be imputed.

- verbose:

  logical. If TRUE, print progress through stages of algorithm

- zygosity:

  A character string indicating the zygosity of the twins. Default is
  "MZ" for monozygotic twins.

## Value

Returns a `data.frame` with MZ twins information added as a new column.
