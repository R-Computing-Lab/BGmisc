# Determine Sex of Offspring

This internal function assigns sexes to the offspring in a generation
based on the specified sex ratio.

## Usage

``` r
determineSex(idGen, sexR, code_male = "M", code_female = "F", beta = FALSE)

determineSex_beta(idGen, sexR, code_male = "M", code_female = "F")
```

## Arguments

- idGen:

  Vector of IDs for the generation.

- sexR:

  Numeric value indicating the sex ratio (proportion of males).

- code_male:

  The value to use for males. Default is "M"

- code_female:

  The value to use for females. Default is "F"

- beta:

  logical. If TRUE, use the optimized version of the algorithm.

## Value

Vector of sexes ("M" for male, "F" for female) for the offspring.
