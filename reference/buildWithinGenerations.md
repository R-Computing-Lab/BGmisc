# Process Generations for Pedigree Simulation

This function iterates through generations in a pedigree simulation,
assigning IDs, creating data frames, determining sexes, and managing
pairing within each generation.

## Usage

``` r
buildWithinGenerations(
  beta = FALSE,
  sizeGens,
  marR,
  sexR,
  Ngen,
  verbose = FALSE,
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  code_male = "M",
  code_female = "F",
  fam_shift = 1L
)
```

## Arguments

- beta:

  logical. If TRUE, use the optimized version of the algorithm.

- sizeGens:

  A numeric vector containing the sizes of each generation within the
  pedigree.

- marR:

  Mating rate. A numeric value ranging from 0 to 1 which determines the
  proportion of mated (fertilized) couples in the pedigree within each
  generation. For instance, marR = 0.5 suggests 50 percent of the
  offspring in a specific generation will be mated and have their
  offspring.

- sexR:

  Sex ratio of offspring. A numeric value ranging from 0 to 1 that
  determines the proportion of males in all offspring in this pedigree.
  For instance, 0.4 means 40 percent of the offspring will be male.

- Ngen:

  Number of generations. An integer \>= 2 that determines how many
  generations the simulated pedigree will have. The first generation is
  always a fertilized couple. The last generation has no mated
  individuals.

- verbose:

  logical If TRUE, message progress through stages of algorithm

- personID:

  character. Name of the column in ped for the person ID variable

- momID:

  character. Name of the column in ped for the mother ID variable

- dadID:

  character. Name of the column in ped for the father ID variable

- code_male:

  The value to use for males. Default is "M"

- code_female:

  The value to use for females. Default is "F"

- fam_shift:

  An integer to shift the person ID. Default is 1L. This is useful when
  simulating multiple pedigrees to avoid ID conflicts.

## Value

A data frame representing the simulated pedigree, including columns for
family ID (\`fam\`),
