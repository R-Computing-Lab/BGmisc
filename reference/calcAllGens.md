# calcAllGens A function to calculate the number of individuals in each generation. This is a supporting function for `simulatePedigree`.

calcAllGens A function to calculate the number of individuals in each
generation. This is a supporting function for `simulatePedigree`.

## Usage

``` r
calcAllGens(kpc, Ngen, marR)

allGens(kpc, Ngen, marR)
```

## Arguments

- kpc:

  Number of kids per couple (integer \>= 2).

- Ngen:

  Number of generations (integer \>= 1).

- marR:

  Mating rate (numeric value ranging from 0 to 1).

## Value

Returns a vector containing the number of individuals in every
generation.
