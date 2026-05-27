# Simulate Pedigrees This function simulates "balanced" pedigrees based on a group of parameters: 1) k - Kids per couple; 2) G - Number of generations; 3) p - Proportion of males in offspring; 4) r - Mating rate.

Simulate Pedigrees This function simulates "balanced" pedigrees based on
a group of parameters: 1) k - Kids per couple; 2) G - Number of
generations; 3) p - Proportion of males in offspring; 4) r - Mating
rate.

## Usage

``` r
simulatePedigree(
  kpc = 3,
  Ngen = 4,
  sexR = 0.5,
  marR = 2/3,
  rd_kpc = FALSE,
  balancedSex = TRUE,
  balancedMar = TRUE,
  verbose = FALSE,
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  spouseID = "spouseID",
  code_male = "M",
  code_female = "F",
  fam_shift = 1L,
  remap_ids = FALSE,
  beta = FALSE
)

SimPed(...)
```

## Arguments

- kpc:

  Number of kids per couple. An integer \>= 2 that determines how many
  kids each fertilized mated couple will have in the pedigree. Default
  value is 3. Returns an error when kpc equals 1.

- Ngen:

  Number of generations. An integer \>= 2 that determines how many
  generations the simulated pedigree will have. The first generation is
  always a fertilized couple. The last generation has no mated
  individuals.

- sexR:

  Sex ratio of offspring. A numeric value ranging from 0 to 1 that
  determines the proportion of males in all offspring in this pedigree.
  For instance, 0.4 means 40 percent of the offspring will be male.

- marR:

  Mating rate. A numeric value ranging from 0 to 1 which determines the
  proportion of mated (fertilized) couples in the pedigree within each
  generation. For instance, marR = 0.5 suggests 50 percent of the
  offspring in a specific generation will be mated and have their
  offspring.

- rd_kpc:

  logical. If TRUE, the number of kids per mate will be randomly
  generated from a poisson distribution with mean kpc. If FALSE, the
  number of kids per mate will be fixed at kpc.

- balancedSex:

  Not fully developed yet. Always `TRUE` in the current version.

- balancedMar:

  Not fully developed yet. Always `TRUE` in the current version.

- verbose:

  logical If TRUE, message progress through stages of algorithm

- personID:

  character. Name of the column in ped for the person ID variable

- momID:

  character. Name of the column in ped for the mother ID variable

- dadID:

  character. Name of the column in ped for the father ID variable

- spouseID:

  The name of the column that will contain the spouse ID in the output
  data frame. Default is "spID".

- code_male:

  The value to use for males. Default is "M"

- code_female:

  The value to use for females. Default is "F"

- fam_shift:

  An integer to shift the person ID. Default is 1L. This is useful when
  simulating multiple pedigrees to avoid ID conflicts.

- remap_ids:

  logical. If TRUE, remap all ID columns to sequential integers (1, 2,
  3, ...) in row order.

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

- ...:

  Additional arguments to be passed to other functions.

## Value

A `data.frame` with each row representing a simulated individual. The
columns are as follows:

- fam: The family id of each simulated individual. It is 'fam1' in a
  single simulated pedigree.

- ID: The unique personal ID of each simulated individual. The first
  digit is the fam id; the fourth digit is the generation the individual
  is in; the following digits represent the order of the individual
  within their pedigree. For example, 100411 suggests this individual
  has a family id of 1, is in the 4th generation, and is the 11th
  individual in the 4th generation.

- gen: The generation the simulated individual is in.

- dadID: Personal ID of the individual's father.

- momID: Personal ID of the individual's mother.

- spID: Personal ID of the individual's mate.

- sex: Biological sex of the individual. F - female; M - male.

## Examples

``` r
set.seed(5)
df_ped <- simulatePedigree(
  kpc = 4,
  Ngen = 4,
  sexR = .5,
  marR = .7
)
summary(df_ped)
#>         fam           ID             gen            dadID           momID      
#>  Length   :57   Min.   :10101   Min.   :1.000   Min.   :10102   Min.   :10101  
#>  N.unique : 1   1st Qu.:10306   1st Qu.:3.000   1st Qu.:10204   1st Qu.:10202  
#>  N.blank  : 0   Median :10320   Median :3.000   Median :10307   Median :10306  
#>  Min.nchar: 5   Mean   :10342   Mean   :3.298   Mean   :10263   Mean   :10263  
#>  Max.nchar: 5   3rd Qu.:10416   3rd Qu.:4.000   3rd Qu.:10311   3rd Qu.:10316  
#>                 Max.   :10432   Max.   :4.000   Max.   :10320   Max.   :10318  
#>                                                 NAs    :13      NAs    :13     
#>     spouseID            sex    
#>  Min.   :10101   Length   :57  
#>  1st Qu.:10205   N.unique : 2  
#>  Median :10306   N.blank  : 0  
#>  Mean   :10266   Min.nchar: 1  
#>  3rd Qu.:10311   Max.nchar: 1  
#>  Max.   :10320                 
#>  NAs    :33                    
```
