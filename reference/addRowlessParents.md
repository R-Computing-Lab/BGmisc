# Add addRowlessParents

This function adds parents who appear in momID or dadID but are missing
from ID

## Usage

``` r
addRowlessParents(ped, verbose, validation_results)
```

## Arguments

- ped:

  A dataframe representing the pedigree data with columns 'ID', 'dadID',
  and 'momID'.

- verbose:

  A logical flag indicating whether to print progress and validation
  messages to the console.

- validation_results:

  validation results
