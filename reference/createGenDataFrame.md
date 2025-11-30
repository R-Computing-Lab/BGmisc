# Create Data Frame for Generation

This function creates a data frame for a specific generation within the
simulated pedigree. It initializes the data frame with default values
for family ID, individual ID, generation number, paternal ID, maternal
ID, spouse ID, and sex. All individuals are initially set with NA for
paternal, maternal, spouse IDs, and sex, awaiting further assignment.

## Usage

``` r
createGenDataFrame(sizeGens, genIndex, idGen)
```

## Arguments

- sizeGens:

  A numeric vector containing the sizes of each generation within the
  pedigree.

- genIndex:

  An integer representing the current generation index for which the
  data frame is being created.

- idGen:

  A numeric vector containing the ID numbers to be assigned to
  individuals in the current generation.

## Value

A data frame representing the initial structure for the individuals in
the specified generation before any relationships (parental, spousal)
are defined. The columns include family ID (\`fam\`), individual ID
(\`id\`), generation number (\`gen\`), father's ID (\`pat\`), mother's
ID (\`mat\`), spouse's ID (\`spID\`), and sex (\`sex\`), with NA values
for paternal, maternal, and spouse IDs, and sex.

## Examples

``` r
sizeGens <- c(3, 5, 4) # Example sizes for 3 generations
genIndex <- 2 # Creating data frame for the 2nd generation
idGen <- 101:105 # Example IDs for the 2nd generation
df_Ngen <- createGenDataFrame(sizeGens, genIndex, idGen)
print(df_Ngen)
#>     fam  id gen pat mat spID sex
#> 1 fam 1 101   2  NA  NA   NA  NA
#> 2 fam 1 102   2  NA  NA   NA  NA
#> 3 fam 1 103   2  NA  NA   NA  NA
#> 4 fam 1 104   2  NA  NA   NA  NA
#> 5 fam 1 105   2  NA  NA   NA  NA
```
