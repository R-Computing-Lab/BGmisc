# Generate or Adjust Number of Kids per Couple Based on Mating Rate

This function generates or adjusts the number of kids per couple in a
generation based on the specified average and whether the count should
be randomly determined.

## Usage

``` r
adjustKidsPerCouple(nMates, kpc, rd_kpc = TRUE, beta = FALSE)

adjustKidsPerCouple_beta(nMates, kpc, rd_kpc = TRUE)
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

- beta:

  logical or character. Controls which algorithm version to use:

  - `FALSE`, `"base"`, or `"original"` (default): Use the original
    algorithm. Slower but ensures exact reproducibility with set.seed().

  - `TRUE` or `"optimized"`: Use the optimized algorithm with 4-5x
    speedup. Produces statistically equivalent results but not identical
    to base version due to different random number consumption.
    Recommended for large simulations where speed matters more than
    exact reproducibility.

  Note: Both versions are mathematically correct and produce valid
  pedigrees with the same statistical properties (sex ratios, mating
  rates, etc.). The optimized version uses vectorized operations instead
  of loops, making it much faster for large pedigrees.

## Value

A numeric vector with the generated or adjusted number of kids per
couple.
