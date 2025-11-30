# Check for within-row duplicates (self-parents, same mom/dad)

This function checks for within-row duplicates in a pedigree.

## Usage

``` r
checkWithinRowDuplicates(ped, verbose = FALSE)
```

## Arguments

- ped:

  A dataframe representing the pedigree data with columns \`ID\`,
  \`dadID\`, and \`momID\`.

- verbose:

  A logical flag indicating whether to print progress and validation
  messages to the console.

## Value

A list containing the results of the check
