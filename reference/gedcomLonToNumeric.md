# Convert GEDCOM Longitude String to Numeric

Converts GEDCOM-style longitude strings like \`"E151.2093"\` or
\`"W0.1278"\` to signed decimal degrees. Returns \`NA\` for \`NA\` or
unrecognised-prefix input.

## Usage

``` r
gedcomLonToNumeric(x)
```

## Arguments

- x:

  Character vector of GEDCOM longitude values.

## Value

Numeric vector of decimal degrees (positive = E, negative = W).

## Examples

``` r
BGmisc:::gedcomLonToNumeric(c("E151.2093", "W0.1278", NA))
#> [1] 151.2093  -0.1278       NA
```
