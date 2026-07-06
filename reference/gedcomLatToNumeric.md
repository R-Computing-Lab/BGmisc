# Convert GEDCOM Latitude String to Numeric

Converts GEDCOM-style latitude strings like \`"N51.5074"\` or
\`"S33.8688"\` to signed decimal degrees. Returns \`NA\` for \`NA\` or
unrecognised-prefix input.

## Usage

``` r
gedcomLatToNumeric(x)
```

## Arguments

- x:

  Character vector of GEDCOM latitude values.

## Value

Numeric vector of decimal degrees (positive = N, negative = S).

## Examples

``` r
BGmisc:::gedcomLatToNumeric(c("N51.5074", "S33.8688", NA))
#> [1]  51.5074 -33.8688       NA
```
