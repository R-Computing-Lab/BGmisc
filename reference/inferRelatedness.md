# Infer Relatedness Coefficient

This function infers the relatedness coefficient between two groups
based on the observed correlation between their additive genetic
variance and shared environmental variance.

## Usage

``` r
inferRelatedness(obsR, aceA = 0.9, aceC = 0, sharedC = 0)

relatedness(...)
```

## Arguments

- obsR:

  Numeric. Observed correlation between the two groups. Must be between
  -1 and 1.

- aceA:

  Numeric. Proportion of variance attributable to additive genetic
  variance. Must be between 0 and 1. Default is 0.9.

- aceC:

  Numeric. Proportion of variance attributable to shared environmental
  variance. Must be between 0 and 1. Default is 0.

- sharedC:

  Numeric. Proportion of shared environment shared between the two
  individuals. Must be between 0 (no shared environment) and 1
  (completely shared environment). Default is 0.

- ...:

  Further named arguments that may be passed to another function.

## Value

Numeric. The calculated relatedness coefficient (\`est_r\`).

## Details

The function uses the ACE (Additive genetic, Common environmental, and
Unique environmental) model to infer the relatedness between two
individuals or groups. By considering the observed correlation
(\`obsR\`), the proportion of variance attributable to additive genetic
variance (\`aceA\`), and the proportion of shared environmental variance
(\`aceC\`), it calculates the relatedness coefficient.

## Examples

``` r
if (FALSE) { # \dontrun{
# Infer the relatedness coefficient:
inferRelatedness(obsR = 0.5, aceA = 0.9, aceC = 0, sharedC = 0)
} # }
```
