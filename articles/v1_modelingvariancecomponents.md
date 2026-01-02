# Modeling variance components

## Introduction

This vignette provides a detailed guide to specific functions within the
`BGmisc` package that aid in the identification and fitting of variance
component models common in behavior genetics. We will explore key
functions such as `identifyComponentModel`, providing practical examples
and theoretical background. Identification ensures a unique set of
parameters that define the model-implied covariance matrix, preventing
free parameters from trading off one another.

### Loading Required Libraries

Ensure that the `BGmisc` package is installed and loaded.

Ensure that the following dependencies are installed before proceeding
as they provide us with behavior genetic data and models:

- `EasyMx`

- `OpenMx`

``` r
library(BGmisc)
library(EasyMx)
library(OpenMx)
```

Note: If any of the libraries are not installed, you can install them
using install.packages(“`package_name`”).

## Working with Variance Component Models

In this section, we will demonstrate core functions related to the
identification and fitting of variance component models.

### Using `comp2vech` Function

The `comp2vech` function is used to vectorize a components model. The
function is often used in conjunction with the identification process.
In this example, we apply it to a list of matrices:

``` r
comp2vech(list(
  matrix(c(1, .5, .5, 1), 2, 2),
  matrix(1, 2, 2)
))
#> [1] 1.0 0.5 1.0 1.0 1.0 1.0
```

The result showcases how the matrices have been transformed, reflecting
their role in subsequent variance component analysis.

### Using `identifyComponentModel` Function

The `identifyComponentModel` function helps determine if a variance
components model is identified. It accepts relatedness component
matrices and returns information about identified and non-identified
parameters.

Here’s an example using the classical twin model *with only MZ twins*:

``` r
identifyComponentModel(
  A = list(matrix(1, 2, 2)),
  C = list(matrix(1, 2, 2)),
  E = diag(1, 2)
)
#> Component model is not identified.
#> Non-identified parameters are  A, C
#> $identified
#> [1] FALSE
#> 
#> $nidp
#> [1] "A" "C"
```

As you can see, the model is not identified. We need to add an
additional group so that we have sufficient information. Let us add the
rest of the classical twin model, in this case DZ twins.

``` r
identifyComponentModel(
  A = list(matrix(c(1, .5, .5, 1), 2, 2), matrix(1, 2, 2)),
  C = list(matrix(1, 2, 2), matrix(1, 2, 2)),
  E = diag(1, 4)
)
#> Component model is identified.
#> $identified
#> [1] TRUE
#> 
#> $nidp
#> character(0)
```

As you can see the model is identified, now that we’ve added another
group. Let us confirm by fitting a model. First we prepare the data.

``` r
require(dplyr)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
# require(purrr)

data(twinData, package = "OpenMx")
selVars <- c("ht1", "ht2")

mzdzData <- subset(
  twinData, zyg %in% c(1, 3),
  c(selVars, "zyg")
)

mzdzData$RCoef <- c(1, NA, .5)[mzdzData$zyg]


mzData <- mzdzData %>% filter(zyg == 1)
```

Let us fit the data with MZ twins by themselves.

``` r
run1 <- emxTwinModel(
  model = "Cholesky",
  relatedness = "RCoef",
  data = mzData,
  use = selVars,
  run = TRUE, name = "TwCh"
)
#> Running TwCh with 4 parameters

summary(run1)
#> Summary of TwCh 
#>  
#> free parameters:
#>      name matrix row col   Estimate    Std.Error A lbound ubound
#> 1 sqrtA11  sqrtA   1   1 0.05122646           NA    1e-06       
#> 2 sqrtC11  sqrtC   1   1 0.03518629           NA       0!       
#> 3 sqrtE11  sqrtE   1   1 0.02325722 0.0007017955 !     0!       
#> 4    Mht1  Means ht1   1 1.62974908 0.0027023907                
#> 
#> Model Statistics: 
#>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
#>        Model:              4                   1112             -3693.148
#>    Saturated:              5                   1111                    NA
#> Independence:              4                   1112                    NA
#> Number of observations/statistics: 569/1116
#> 
#> Information Criteria: 
#>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
#> AIC:      -5917.148              -3685.148                -3685.078
#> BIC:     -10747.543              -3667.773                -3680.471
#> To get additional fit indices, see help(mxRefModels)
#> timestamp: 2026-01-02 21:16:09 
#> Wall clock time: 0.0700562 secs 
#> optimizer:  SLSQP 
#> OpenMx version number: 2.22.10 
#> Need help?  See help(mxSummary)
```

As you can see the model was unsuccessful because it was not identified.
But when we add another group, so that the model is identified, the
model now fits.

``` r
run2 <- emxTwinModel(
  model = "Cholesky",
  relatedness = "RCoef",
  data = mzdzData,
  use = selVars,
  run = TRUE, name = "TwCh"
)
#> Running TwCh with 4 parameters

summary(run2)
#> Summary of TwCh 
#>  
#> free parameters:
#>      name matrix row col   Estimate    Std.Error A lbound ubound
#> 1 sqrtA11  sqrtA   1   1 0.06339271 0.0014377690    1e-06       
#> 2 sqrtC11  sqrtC   1   1 0.00000100 0.0250260004 !     0!       
#> 3 sqrtE11  sqrtE   1   1 0.02330040 0.0007015267       0!       
#> 4    Mht1  Means ht1   1 1.63295540 0.0020511844                
#> 
#> Model Statistics: 
#>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
#>        Model:              4                   1803             -5507.092
#>    Saturated:              5                   1802                    NA
#> Independence:              4                   1803                    NA
#> Number of observations/statistics: 920/1807
#> 
#> Information Criteria: 
#>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
#> AIC:      -9113.092              -5499.092                -5499.048
#> BIC:     -17811.437              -5479.794                -5492.498
#> To get additional fit indices, see help(mxRefModels)
#> timestamp: 2026-01-02 21:16:10 
#> Wall clock time: 0.05088806 secs 
#> optimizer:  SLSQP 
#> OpenMx version number: 2.22.10 
#> Need help?  See help(mxSummary)
```
