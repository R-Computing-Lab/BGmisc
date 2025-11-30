# fitComponentModel Fit the estimated variance components of a model to covariance data

fitComponentModel Fit the estimated variance components of a model to
covariance data

## Usage

``` r
fitComponentModel(covmat, ...)
```

## Arguments

- covmat:

  The covariance matrix of the raw data, which may be blockwise.

- ...:

  Comma-separated relatedness component matrices representing the
  variance components of the model.

## Value

A regression (linear model fitted with `lm`). The coefficients of the
regression represent the estimated variance components.

## Details

This function fits the estimated variance components of a model to given
covariance data. The rank of the component matrices is checked to ensure
that the variance components are all identified. Warnings are issued if
there are inconsistencies.

## Examples

``` r
if (FALSE) { # \dontrun{
# install.packages("OpenMX")
data(twinData, package = "OpenMx")
sellVars <- c("ht1", "ht2")
mzData <- subset(twinData, zyg %in% c(1), c(selVars, "zyg"))
dzData <- subset(twinData, zyg %in% c(3), c(selVars, "zyg"))

fitComponentModel(
  covmat = list(cov(mzData[, selVars], use = "pair"), cov(dzData[, selVars], use = "pair")),
  A = list(matrix(1, nrow = 2, ncol = 2), matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)),
  C = list(matrix(1, nrow = 2, ncol = 2), matrix(1, nrow = 2, ncol = 2)),
  E = list(diag(1, nrow = 2), diag(1, nrow = 2))
)
} # }
```
