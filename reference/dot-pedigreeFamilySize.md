# Determine family size from a family's relatedness matrices (internal)

Determine family size from a family's relatedness matrices (internal)

## Usage

``` r
.pedigreeFamilySize(mats_in)
```

## Arguments

- mats_in:

  A list of relatedness matrices (possibly containing NULLs); the first
  non-NULL matrix's row count is used as the family size.

## Value

Integer family size.
