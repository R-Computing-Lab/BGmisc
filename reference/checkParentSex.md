# Check Parental Role Sex Consistency

Validates sex coding consistency for a given parental role (momID or
dadID).

## Usage

``` r
checkParentSex(ped, parent_col, sex_col = "sex", verbose = FALSE)
```

## Arguments

- ped:

  Pedigree dataframe.

- parent_col:

  The column name for parent IDs ("momID" or "dadID").

- sex_col:

  The column name for sex coding. Default is "sex".

- verbose:

  Logical, whether to print messages.

## Value

A list containing role, unique sex codes, modal sex, inconsistent
parents, and linked children.
