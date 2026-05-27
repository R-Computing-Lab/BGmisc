# Simulate Multiple Pedigrees

This function simulates multiple "balanced" pedigrees and returns them
combined into a single data frame. It is a convenience wrapper around
[`simulatePedigree`](https://r-computing-lab.github.io/BGmisc/reference/simulatePedigree.md)
that makes it easy to simulate many families at once, with unique IDs
across all families.

## Usage

``` r
simulatePedigrees(
  n_fam = 2,
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
  remap_ids = TRUE,
  beta = FALSE
)
```

## Arguments

- n_fam:

  Integer. Number of families to simulate. Default is 2.

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

- remap_ids:

  Logical. If TRUE (default), all ID columns (personID, momID, dadID,
  spouseID) will be remapped to sequential integers starting at 1 across
  the combined data frame. This ensures tidy consecutive IDs regardless
  of fam_shift offsets. If FALSE, IDs will retain their original values
  from each pedigree simulation, which may include gaps or
  non-sequential values due to fam_shift.

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

A `data.frame` containing all simulated individuals from all families
combined, with the same columns as
[`simulatePedigree`](https://r-computing-lab.github.io/BGmisc/reference/simulatePedigree.md).
The `fam` column uniquely identifies each family (e.g., "fam1", "fam2",
...). Individual IDs are sequential integers starting at 1 (i.e.,
`1:nrow(result)`), and all parent/spouse ID references are remapped to
match.

## Examples

``` r
set.seed(5)
df_peds <- simulatePedigrees(
  n_fam = 3,
  kpc = 4,
  Ngen = 4,
  sexR = .5,
  marR = .7
)
summary(df_peds)
#>         fam            ID             gen            dadID       
#>  Length   :171   Min.   :  1.0   Min.   :1.000   Min.   :  2.00  
#>  N.unique :  3   1st Qu.: 43.5   1st Qu.:3.000   1st Qu.: 21.00  
#>  N.blank  :  0   Median : 86.0   Median :3.000   Median : 71.00  
#>  Min.nchar:  4   Mean   : 86.0   Mean   :3.298   Mean   : 71.21  
#>  Max.nchar:  4   3rd Qu.:128.5   3rd Qu.:4.000   3rd Qu.:120.00  
#>                  Max.   :171.0   Max.   :4.000   Max.   :142.00  
#>                                                  NAs    :39      
#>      momID           spouseID             sex     
#>  Min.   :  1.00   Min.   :  1.00   Length   :171  
#>  1st Qu.: 22.00   1st Qu.: 20.75   N.unique :  2  
#>  Median : 70.00   Median : 72.00   N.blank  :  0  
#>  Mean   : 70.82   Mean   : 71.31   Min.nchar:  1  
#>  3rd Qu.:119.00   3rd Qu.:120.50   Max.nchar:  1  
#>  Max.   :141.00   Max.   :142.00                  
#>  NAs    :39       NAs    :99                      
```
