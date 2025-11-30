# identifyComponentModel Determine if a variance components model is identified

identifyComponentModel Determine if a variance components model is
identified

## Usage

``` r
identifyComponentModel(..., verbose = TRUE)
```

## Arguments

- ...:

  Comma-separated relatedness component matrices representing the
  variance components of the model.

- verbose:

  logical. If FALSE, suppresses messages about identification; TRUE by
  default.

## Value

A list of length 2 containing:

- `identified`: TRUE if the model is identified, FALSE otherwise.

- `nidp`: A vector of non-identified parameters, specifying the names of
  components that are not simultaneously identified.

## Details

This function checks the identification status of a given variance
components model by examining the rank of the concatenated matrices of
the components. If any components are not identified, their names are
returned in the output.

## Examples

``` r
identifyComponentModel(A = list(matrix(1, 2, 2)), C = list(matrix(1, 2, 2)), E = diag(1, 2))
#> Component model is not identified.
#> Non-identified parameters are  A, C 
#> $identified
#> [1] FALSE
#> 
#> $nidp
#> [1] "A" "C"
#> 
```
