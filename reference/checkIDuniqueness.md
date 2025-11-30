# Check for duplicated individual IDs

This function checks for duplicated individual IDs in a pedigree.

## Usage

``` r
checkIDuniqueness(ped, verbose = FALSE)
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
