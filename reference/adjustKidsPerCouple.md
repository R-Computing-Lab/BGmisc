# Generate or Adjust Number of Kids per Couple Based on Mating Rate

This function generates or adjusts the number of kids per couple in a
generation based on the specified average and whether the count should
be randomly determined.

## Usage

``` r
adjustKidsPerCouple(nMates, kpc, rd_kpc = TRUE)
```

## Arguments

- nMates:

  Integer, the number of mated pairs in the generation.

- kpc:

  Number of kids per couple. An integer \>= 2 that determines how many
  kids each fertilized mated couple will have in the pedigree. Default
  value is 3. Returns an error when kpc equals 1.

- rd_kpc:

  logical. If TRUE, the number of kids per mate will be randomly
  generated from a poisson distribution with mean kpc. If FALSE, the
  number of kids per mate will be fixed at kpc.

## Value

A numeric vector with the generated or adjusted number of kids per
couple.
