# calcFamilySizeByGen An internal supporting function for `simulatePedigree`.

calcFamilySizeByGen An internal supporting function for
`simulatePedigree`.

## Usage

``` r
calcFamilySizeByGen(kpc, Ngen, marR)

sizeAllGens(kpc, Ngen, marR)
```

## Arguments

- kpc:

  Number of kids per couple (integer \>= 2).

- Ngen:

  Number of generations (integer \>= 1).

- marR:

  Mating rate (numeric value ranging from 0 to 1).

## Value

Returns a vector including the number of individuals in every
generation.
