# Falconer's Formula

Use Falconer's formula to solve for H using the observed correlations
for two groups of any two levels of relatednesses.

## Usage

``` r
calculateH(r1, r2, obsR1, obsR2)
```

## Arguments

- r1:

  Relatedness coefficient of the first group.

- r2:

  Relatedness coefficient of the second group.

- obsR1:

  Observed correlation between members of the first group.

- obsR2:

  Observed correlation between members of the second group.

## Value

Heritability estimates (\`heritability_estimates\`).

## Details

This generalization of Falconer's formula provides a method to calculate
heritability by using the observed correlations for two groups of any
two relatednesses. This function solves for H using the formula: \$\$H^2
= \frac{obsR1 - obsR2}{r1 - r2}\$\$ where r1 and r2 are the relatedness
coefficients for the first and second group, respectively, and obsR1 and
obsR2 are the observed correlations.
