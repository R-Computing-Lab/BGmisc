# Calculate Relatedness Coefficient

This function calculates the relatedness coefficient between two
individuals based on their shared ancestry, as described by Wright
(1922).

## Usage

``` r
calculateRelatedness(
  generations = 2,
  path = NULL,
  full = TRUE,
  maternal = FALSE,
  empirical = FALSE,
  segregating = TRUE,
  total_a = 6800 * 1e+06,
  total_m = 16500,
  weight_a = 1,
  weight_m = 1,
  denom_m = FALSE,
  ...
)

related_coef(...)
```

## Arguments

- generations:

  Number of generations back of common ancestors the pair share.

- path:

  Traditional method to count common ancestry, which is twice the number
  of generations removed from common ancestors. If not provided, it is
  calculated as 2\*generations.

- full:

  Logical. Indicates if the kin share both parents at the common
  ancestor's generation. Default is TRUE.

- maternal:

  Logical. Indicates if the maternal lineage should be considered in the
  calculation.

- empirical:

  Logical. Adjusts the coefficient based on empirical data, using the
  total number of nucleotides and other parameters.

- segregating:

  Logical. Adjusts for segregating genes.

- total_a:

  Numeric. Represents the total size of the autosomal genome in terms of
  nucleotides, used in empirical adjustment. Default is 6800\*1000000.

- total_m:

  Numeric. Represents the total size of the mitochondrial genome in
  terms of nucleotides, used in empirical adjustment. Default is 16500.

- weight_a:

  Numeric. Represents the weight of phenotypic influence from additive
  genetic variance, used in empirical adjustment.

- weight_m:

  Numeric. Represents the weight of phenotypic influence from
  mitochondrial effects, used in empirical adjustment.

- denom_m:

  Logical. Indicates if \`total_m\` and \`weight_m\` should be included
  in the denominator of the empirical adjustment calculation.

- ...:

  Further named arguments that may be passed to another function.

## Value

Relatedness Coefficient (\`coef\`): A measure of the genetic
relationship between two individuals.

## Details

The relatedness coefficient between two people (b & c) is defined in
relation to their common ancestors: \\r\_{bc} = \sum
\left(\frac{1}{2}\right)^{n+n'+1} (1+f_a)\\

## Examples

``` r
if (FALSE) { # \dontrun{
# For full siblings, the relatedness coefficient is expected to be 0.5:
calculateRelatedness(generations = 1, full = TRUE)
# For half siblings, the relatedness coefficient is expected to be 0.25:
calculateRelatedness(generations = 1, full = FALSE)
} # }
```
