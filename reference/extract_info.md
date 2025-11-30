# Extract Information from Line

Extracts the relevant information from a GEDCOM line based on the
specified type. The function uses regular expressions to locate and
return the desired data.

## Usage

``` r
extract_info(line, type)
```

## Arguments

- line:

  A character string representing a line from a GEDCOM file.

- type:

  A character string representing the type of information to extract.

## Value

A character string with the extracted information.
