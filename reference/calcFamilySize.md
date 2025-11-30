# calcFamilySize A function to calculate the total number of individuals in a pedigree given parameters. This is a supporting function for function `simulatePedigree`

calcFamilySize A function to calculate the total number of individuals
in a pedigree given parameters. This is a supporting function for
function `simulatePedigree`

## Usage

``` r
calcFamilySize(kpc, Ngen, marR)

famSizeCal(kpc, Ngen, marR)
```

## Arguments

- kpc:

  Number of kids per couple (integer \>= 2).

- Ngen:

  Number of generations (integer \>= 1).

- marR:

  Mating rate (numeric value ranging from 0 to 1).

## Value

Returns a numeric value indicating the total pedigree size.
