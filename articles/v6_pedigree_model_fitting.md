# Fitting Pedigree-Based Variance Component Models

## Introduction

A core goal of behavior genetics is to decompose observed phenotypic
variance into genetic and environmental components. Traditionally, this
has been done using twin studies, which compare monozygotic (MZ) and
dizygotic (DZ) twin pairs. However, extended pedigrees –
multi-generational families with known parentage – provide richer
information about relatedness and allow researchers to estimate a wider
array of variance components.

The `BGmisc` package provides a complete pipeline for pedigree-based
variance component modeling:

1.  **Simulate** multi-generational pedigrees with
    [`simulatePedigree()`](https://r-computing-lab.github.io/BGmisc/reference/simulatePedigree.md)
2.  **Compute** relatedness matrices with
    [`ped2add()`](https://r-computing-lab.github.io/BGmisc/reference/ped2add.md),
    [`ped2cn()`](https://r-computing-lab.github.io/BGmisc/reference/ped2cn.md),
    [`ped2mit()`](https://r-computing-lab.github.io/BGmisc/reference/ped2mit.md),
    and
    [`ped2ce()`](https://r-computing-lab.github.io/BGmisc/reference/ped2ce.md)
3.  **Check identification** with
    [`identifyComponentModel()`](https://r-computing-lab.github.io/BGmisc/reference/identifyComponentModel.md)
    and
    [`comp2vech()`](https://r-computing-lab.github.io/BGmisc/reference/comp2vech.md)
4.  **Build and fit** structural equation models with
    [`buildOneFamilyGroup()`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md),
    [`buildPedigreeMx()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md),
    and
    [`fitPedigreeModel()`](https://r-computing-lab.github.io/BGmisc/reference/fitPedigreeModel.md)

This vignette walks through each step, from generating a pedigree to
fitting a variance component model and interpreting the results.

### Prerequisites

This vignette requires the `OpenMx` package for structural equation
modeling. If you don’t have it installed, you can install it from CRAN
or from the OpenMx website.

``` r
library(BGmisc)

has_openmx <- requireNamespace("OpenMx", quietly = TRUE)
has_mvtnorm <- requireNamespace("mvtnorm", quietly = TRUE)

if (has_openmx) {
  library(OpenMx)
} else {
  message(
    "OpenMx is not installed. Code will be shown but not executed.\n",
    "Install OpenMx with: install.packages('OpenMx')"
  )
}
#> To take full advantage of multiple cores, use:
#>   mxOption(key='Number of Threads', value=parallel::detectCores()) #now
#>   Sys.setenv(OMP_NUM_THREADS=parallel::detectCores()) #before library(OpenMx)
#> 
#> Attaching package: 'OpenMx'
#> The following object is masked from 'package:BGmisc':
#> 
#>     vech

if (!has_mvtnorm) {
  message(
    "mvtnorm is not installed. Data simulation examples will not run.\n",
    "Install mvtnorm with: install.packages('mvtnorm')"
  )
} else {
  library(mvtnorm)
}

run_models <- has_openmx && has_mvtnorm
```

## Step 1: Simulate a Pedigree

We begin by simulating a multi-generational pedigree using
[`simulatePedigree()`](https://r-computing-lab.github.io/BGmisc/reference/simulatePedigree.md).
This creates a balanced family structure with a specified number of
generations and children per couple.

``` r
set.seed(421)

ped <- simulatePedigree(
  kpc = 3,    # 3 children per couple
  Ngen = 4,   # 4 generations
  sexR = 0.5, # equal sex ratio
  marR = 0.6  # 60% mating rate
)

head(ped)
#>     fam    ID gen dadID momID  spID sex
#> 1 fam 1 10101   1    NA    NA 10102   F
#> 2 fam 1 10102   1    NA    NA 10101   M
#> 3 fam 1 10201   2    NA    NA 10203   M
#> 4 fam 1 10202   2 10102 10101    NA   M
#> 5 fam 1 10203   2 10102 10101 10201   F
#> 6 fam 1 10204   2    NA    NA 10205   M
```

The resulting data frame contains one row per individual with columns
for family ID (`fam`), personal ID (`ID`), generation (`gen`), parent
IDs (`dadID`, `momID`), spouse ID (`spID`), and biological sex (`sex`).

- Number of individuals: `25`
- Number of generations: `4`

``` r
summarizeFamilies(ped, famID = "fam")$family_summary
#>       fam count gen_mean gen_median gen_min gen_max    gen_sd spID_mean
#>    <char> <int>    <num>      <num>   <num>   <num>     <num>     <num>
#> 1:  fam 1    25        3          3       1       4 0.9574271  10237.42
#>    spID_median spID_min spID_max spID_sd
#>          <num>    <num>    <num>   <num>
#> 1:       10253    10101    10309 79.4818
```

## Step 2: Compute Relatedness Matrices

With a pedigree in hand, we compute the various relatedness component
matrices. Each matrix is square and symmetric, with rows and columns
corresponding to individuals in the pedigree. The entry in row *i* and
column *j* quantifies the relatedness between person *i* and person *j*
along a specific pathway.

### Additive Genetic Relatedness

The additive genetic relatedness matrix captures the expected proportion
of nuclear DNA shared identical by descent (IBD) between two
individuals. For example, parent-offspring pairs share 0.5, full
siblings share 0.5 on average, half-siblings share 0.25, and so on.

``` r
add_matrix <- ped2add(ped, sparse = FALSE)
add_matrix[1:5, 1:5]
#>       10101 10102 10201 10202 10203
#> 10101   1.0   0.0     0   0.5   0.5
#> 10102   0.0   1.0     0   0.5   0.5
#> 10201   0.0   0.0     1   0.0   0.0
#> 10202   0.5   0.5     0   1.0   0.5
#> 10203   0.5   0.5     0   0.5   1.0
```

### Common Nuclear Environment

The common nuclear environment matrix captures whether two individuals
shared both biological parents (i.e., were raised in the same nuclear
family). Full siblings who share the same mother and father have a value
of 1; all others have 0.

``` r
cn_matrix <- ped2cn(ped, sparse = FALSE)
cn_matrix[1:5, 1:5]
#>       10101 10102 10201 10202 10203
#> 10101     1     0     0     0     0
#> 10102     0     1     0     0     0
#> 10201     0     0     1     0     0
#> 10202     0     0     0     1     1
#> 10203     0     0     0     1     1
```

### Mitochondrial Relatedness

The mitochondrial relatedness matrix captures shared maternal lineage.
Individuals who share the same maternal line (mother, maternal
grandmother, etc.) share mitochondrial DNA.

``` r
mt_matrix <- ped2mit(ped, sparse = FALSE)
mt_matrix[1:5, 1:5]
#>       10101 10102 10201 10202 10203
#> 10101     1     0     0     1     1
#> 10102     0     1     0     0     0
#> 10201     0     0     1     0     0
#> 10202     1     0     0     1     1
#> 10203     1     0     0     1     1
```

### Common Extended Family Environment

The common extended family environment matrix indicates whether
individuals belong to the same extended family. For a single pedigree,
this is simply a matrix of ones.

``` r
ce_matrix <- ped2ce(ped)
ce_matrix[1:5, 1:5]
#>       10101 10102 10201 10202 10203
#> 10101     1     1     1     1     1
#> 10102     1     1     1     1     1
#> 10201     1     1     1     1     1
#> 10202     1     1     1     1     1
#> 10203     1     1     1     1     1
```

## Step 3: Check Model Identification

Before fitting a model, we need to verify that the variance components
are *identified* – that is, the data provide enough information to
uniquely estimate each parameter. If components are not identified,
their estimates can trade off against each other, leading to unstable or
meaningless results.

The
[`identifyComponentModel()`](https://r-computing-lab.github.io/BGmisc/reference/identifyComponentModel.md)
function checks identification by vectorizing each relatedness component
matrix (via
[`comp2vech()`](https://r-computing-lab.github.io/BGmisc/reference/comp2vech.md))
and testing whether the resulting design matrix has full column rank.
Each component matrix becomes a column in this design matrix. If the
rank equals the number of components, the model is identified.

For more background on identification in variance component models, see
[`vignette("v1_modelingvariancecomponents", package = "BGmisc")`](https://r-computing-lab.github.io/BGmisc/articles/v1_modelingvariancecomponents.md).

### Checking Our Full Model

We plan to fit a 5-component model with additive genetic (A), common
nuclear environment (Cn), common extended environment (Ce),
mitochondrial (Mt), and unique environment (E) components. Let’s check
whether these five components are simultaneously identified using the
relatedness matrices we just computed:

``` r
id_full <- identifyComponentModel(
  A  = add_matrix,
  Cn = cn_matrix,
  Ce = ce_matrix,
  Mt = mt_matrix,
  E  = diag(1, nrow(add_matrix))
)
#> Component model is identified.
id_full
#> $identified
#> [1] TRUE
#> 
#> $nidp
#> character(0)
```

The full model is identified. We can proceed to fit it. However, to
illustrate the process of checking identification and refining the
model, we will also show how to interpret the details of the
identification check. Below, I have provided an unidentified model to
demonstrate how to use the `nidp` element of the result to understand
which components are confounded.

``` r
# show if model is identified

identifyComponentModel(
  A  = add_matrix,
  A2  = add_matrix,
  Cn = cn_matrix,
  Ce = ce_matrix,
  Mt = mt_matrix,
  E  = diag(1, nrow(add_matrix))
)
#> Component model is not identified.
#> Non-identified parameters are  A, A2
#> $identified
#> [1] FALSE
#> 
#> $nidp
#> [1] "A"  "A2"
```

With a single pedigree, the 5-component model *may* not be fully
identified. The `nidp` element in the result tells us which components
are confounded. This is because some relatedness matrices can be
linearly dependent – for example, `ce_matrix` is a matrix of all ones
for a single family, and the identity matrix (E) plus other components
may span a similar space. In this case, our model is identified, but if
it were not, we would see a message like “Variance components are not
all identified. (And even if they were, we might not have enough data to
estimate them all.)

### Narrowing Down to an Identified Model

Based on the identification check above, we can drop or fix the
non-identified components. A natural choice is to remove the components
flagged by
[`identifyComponentModel()`](https://r-computing-lab.github.io/BGmisc/reference/identifyComponentModel.md)
and re-check:

``` r
# A simpler model: A + Cn + E
id_ace <- identifyComponentModel(
  A  = list(add_matrix),
  Cn = list(cn_matrix),
  E  = diag(1, nrow(add_matrix))
)
#> Component model is identified.
id_ace
#> $identified
#> [1] TRUE
#> 
#> $nidp
#> character(0)
```

A single extended pedigree typically provides enough variation in
relatedness coefficients (parent-offspring = 0.5, siblings = 0.5,
grandparent-grandchild = 0.25, cousins = 0.125, etc.) to identify the
A + Cn + E model.

### Recovering Identification with Multiple Families

When a component is not identified with one family, adding families with
different structures can help. Each family contributes additional rows
to the design matrix. Let’s check whether the full 5-component model
becomes identified when we combine two pedigrees:

``` r
set.seed(99)
ped2 <- simulatePedigree(kpc = 4, Ngen = 3, marR = 0.5) |> makeTwins()
add2 <- ped2add(ped2, sparse = FALSE)
cn2  <- ped2cn(ped2, sparse = FALSE)
ce2  <- ped2ce(ped2)
mt2  <- ped2mit(ped2, sparse = FALSE)

# Check the full model across two families
n_combined <- nrow(add_matrix) + nrow(add2)
id_two_fam <- identifyComponentModel(
  A  = list(add_matrix, add2),
  Cn = list(cn_matrix, cn2),
  Ce = list(ce_matrix, ce2),
  Mt = list(mt_matrix, mt2),
  E  = diag(1, n_combined)
)
#> Component model is identified.
id_two_fam
#> $identified
#> [1] TRUE
#> 
#> $nidp
#> character(0)
```

This result guides our modeling decisions in the steps that follow. When
fitting the model below, we set the non-identified components’ true
values to zero so that we have a known ground truth to recover.

## Step 4: Simulate Phenotypic Data

Before fitting a model, we need observed data. In practice, this would
be measured phenotypes (e.g., height, cognitive ability, disease
status). Here, we simulate phenotypic data from a known variance
component structure so we can verify that our model recovers the true
parameters.

We define “true” variance components and use the relatedness matrices to
construct the population covariance matrix, then sample from it.

``` r
# True variance components (proportions of total variance)
true_var <- list(
  ad2 = 0.50,  # additive genetic
  cn2 = 0.10,  # common nuclear environment
  ce2 = 0.00,  # common extended environment (set to 0 for identifiability)
  mt2 = 0.00,  # mitochondrial (set to 0 for simplicity)
  ee2 = 0.40   # unique environment (residual)
)

# Build the implied covariance matrix
# V = ad2*A + cn2*Cn + ce2*U + mt2*Mt + ee2*I
n <- nrow(add_matrix)
I_mat <- diag(1, n)
U_mat <- matrix(1, n, n)

V_true <- true_var$ad2 * add_matrix +
  true_var$cn2 * cn_matrix +
  true_var$ce2 * U_mat +
  true_var$mt2 * mt_matrix +
  true_var$ee2 * I_mat

# Simulate one realization of the phenotype vector
set.seed(123)
y <- mvtnorm::rmvnorm(1, sigma = V_true)


# Create named variable labels (required by OpenMx)
ytemp <- paste("S", rownames(add_matrix))
```

``` r

if (!exists("y")) {
  y <- rep(NA, nrow(add_matrix))
}
```

We simulated phenotypic data for25 individuals, with a mean of -0.064
and a standard deviation of 0.884. The variance in this simulated
phenotype arises from the specified genetic and environmental components
according to the covariance structure we defined.

In practice, you would have data from multiple independently ascertained
families. Here we simulate data from a single pedigree for simplicity,
but the model-fitting functions support multiple pedigrees (shown in a
later section).

## Step 5: Build the OpenMx Model

The `BGmisc` package provides helper functions that construct the OpenMx
model in three layers:

1.  **[`buildPedigreeModelCovariance()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeModelCovariance.md)**
    – Creates the variance component parameters (free parameters to be
    estimated)
2.  **[`buildOneFamilyGroup()`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md)**
    – Creates the model for a single family, embedding the relatedness
    matrices and observed data
3.  **[`buildPedigreeMx()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md)**
    – Combines the variance components with one or more family groups
    into a multi-group model

### Building a Single-Family Model

Let’s walk through building the model step by step.

#### Variance Component Parameters

The
[`buildPedigreeModelCovariance()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeModelCovariance.md)
function creates the OpenMx sub-model that holds the free variance
component parameters. You can control which components to include:

``` r
# Starting values for the optimizer
start_vars <- list(
  ad2 = 0.3, dd2 = 0, cn2 = 0.1,
  ce2 = 0.1, mt2 = 0.1, am2 = 0,
  ee2 = 0.4
)

# Build variance component sub-model
vc_model <- buildPedigreeModelCovariance(
  vars = start_vars,
  Vad = TRUE,   # estimate additive genetic variance
  Vdd = FALSE,  # do not estimate dominance
  Vcn = TRUE,   # estimate common nuclear environment
  Vce = TRUE,   # estimate common extended environment
  Vmt = TRUE,   # estimate mitochondrial
  Vam = FALSE,  # do not estimate A x Mt interaction
  Ver = TRUE    # estimate unique environment
)
vc_model
#> MxModel 'ModelOne' 
#> type : default 
#> $matrices : 'Vad', 'Vcn', 'Vce', 'Vmt', and 'Ver' 
#> $algebras : NULL 
#> $penalties : NULL 
#> $constraints : NULL 
#> $intervals : NULL 
#> $latentVars : none
#> $manifestVars : none
#> $data : NULL
#> $submodels : NULL 
#> $expectation : NULL 
#> $fitfunction : NULL 
#> $compute : NULL 
#> $independent : FALSE 
#> $options :  
#> $output : FALSE

summary(vc_model)
#> Summary of ModelOne 
#>  
#> Model Statistics: 
#>                |  Parameters  |  Degrees of Freedom  |  Fit ( units)
#>        Model:              0                      0               NA
#>    Saturated:             NA                     NA               NA
#> Independence:             NA                     NA               NA
#> Number of observations/statistics: 0/0
#> 
#> timestamp: NULL 
#> Wall clock time: NULL 
#> OpenMx version number: NULL 
#> Need help?  See help(mxSummary)
#> WARNING: This model has not been run yet. Tip: Use
#>   model = mxRun(model)
#> to estimate a model.
```

#### Family Group Model

The
[`buildOneFamilyGroup()`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md)
function constructs the model for one family. It takes the relatedness
matrices and the observed data for that family:

``` r
# Format the observed data as a 1-row matrix with named columns
obs_data <- matrix(y, nrow = 1, dimnames = list(NULL, ytemp))

# Build the family group model
family_group <- buildOneFamilyGroup(
  group_name = "family1",
  Addmat = add_matrix,
  Nucmat = cn_matrix,
  Extmat = ce_matrix,
  Mtdmat = mt_matrix,
  full_df_row = obs_data,
  ytemp = ytemp
)
```

The family group model contains:

- **Identity matrix (I)** and **unit matrix (U)** for the unique and
  extended environment components
- **Relatedness matrices** (A, Cn, Mt, etc.) as fixed data matrices
- **An mxAlgebra** that computes the implied covariance:
  $V = \sigma_{a}^{2}A + \sigma_{cn}^{2}C_{n} + \sigma_{ce}^{2}U + \sigma_{mt}^{2}Mt + \sigma_{e}^{2}I$
- **Normal expectation** with the covariance and a free mean

#### Assembling the Full Model

The
[`buildPedigreeMx()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md)
function combines the variance component parameters (shared across all
families) with the family group model(s):

``` r
full_model <- buildPedigreeMx(
  model_name = "PedigreeVCModel",
  vars = start_vars,
  group_models = list(family_group)
)
full_model$submodels 
#> $ModelOne
#> MxModel 'ModelOne' 
#> type : default 
#> $matrices : 'Vad', 'Vcn', 'Vce', 'Vmt', and 'Ver' 
#> $algebras : NULL 
#> $penalties : NULL 
#> $constraints : NULL 
#> $intervals : NULL 
#> $latentVars : none
#> $manifestVars : none
#> $data : NULL
#> $submodels : NULL 
#> $expectation : NULL 
#> $fitfunction : NULL 
#> $compute : NULL 
#> $independent : FALSE 
#> $options :  
#> $output : FALSE 
#> 
#> $family1
#> MxModel 'family1' 
#> type : default 
#> $matrices : 'I', 'U', 'A', 'Cn', 'Mt', and 'M' 
#> $algebras : 'V' 
#> $penalties : NULL 
#> $constraints : NULL 
#> $intervals : NULL 
#> $latentVars : none
#> $manifestVars : none
#> $data : 1 x 25 
#> $data means : NA
#> $data type: 'raw' 
#> $submodels : NULL 
#> $expectation : MxExpectationNormal 
#> $fitfunction : MxFitFunctionML 
#> $compute : NULL 
#> $independent : FALSE 
#> $options :  
#> $output : FALSE
```

## Step 6: Fit the Model

With the model assembled, we fit it using OpenMx’s optimizer. The
[`mxRun()`](https://rdrr.io/pkg/OpenMx/man/mxRun.html) function performs
maximum likelihood estimation:

``` r
fitted_model <- mxRun(full_model)
#> Running PedigreeVCModel with 6 parameters
smr <- summary(fitted_model)
```

``` r
# Extract variance component estimates
estimates <- c(
  Vad = fitted_model$ModelOne$Vad$values[1, 1],
  Vcn = fitted_model$ModelOne$Vcn$values[1, 1],
  Vce = fitted_model$ModelOne$Vce$values[1, 1],
  Vmt = fitted_model$ModelOne$Vmt$values[1, 1],
  Ver = fitted_model$ModelOne$Ver$values[1, 1]
)

# Compare estimates to true values
truth <- c(
  Vad = true_var$ad2,
  Vcn = true_var$cn2,
  Vce = true_var$ce2,
  Vmt = true_var$mt2,
  Ver = true_var$ee2
)

comparison <- data.frame(
  Component = names(truth),
  True = truth,
  Estimated = round(estimates, 4)
)
comparison
#>     Component True Estimated
#> Vad       Vad  0.5    0.5510
#> Vcn       Vcn  0.1    0.0085
#> Vce       Vce  0.0    0.0000
#> Vmt       Vmt  0.0    0.0909
#> Ver       Ver  0.4    0.2890
```

``` r
cat("-2 Log Likelihood:", smr$Minus2LogLikelihood, "\n")
#> -2 Log Likelihood: 63.47269
cat("Converged:", fitted_model$output$status$code == 0, "\n")
#> Converged: TRUE
```

With a single pedigree realization, estimates will vary from the true
values due to sampling variability. Estimation improves substantially
with multiple families, as shown next.

## Step 7: Multi-Pedigree Model

In practice, researchers have data from multiple families. The BGmisc
helpers are designed for this multi-group scenario, where variance
component parameters are shared across families but each family has its
own relatedness structure and data.

### Simulating Multiple Families

``` r
set.seed(2024)
n_families <- 5

# Storage
ped_list <- vector("list", n_families)
add_list <- vector("list", n_families)
cn_list <- vector("list", n_families)
mt_list <- vector("list", n_families)
ce_list <- vector("list", n_families)
y_list <- vector("list", n_families)
ytemp_list <- vector("list", n_families)

for (i in seq_len(n_families)) {
  # Simulate each family with slightly different structure
  ped_i <- simulatePedigree(kpc = 3, Ngen = 4, marR = 0.6)
  ped_list[[i]] <- ped_i

  # Compute relatedness matrices
  A_i <- as.matrix(ped2add(ped_i))
  Cn_i <- as.matrix(ped2cn(ped_i))
  Mt_i <- as.matrix(ped2mit(ped_i))
  Ce_i <- ped2ce(ped_i)
  n_i <- nrow(A_i)

  add_list[[i]] <- A_i
  cn_list[[i]] <- Cn_i
  mt_list[[i]] <- Mt_i
  ce_list[[i]] <- Ce_i

  # Build implied covariance and simulate data
  I_i <- diag(1, n_i)
  U_i <- matrix(1, n_i, n_i)
  V_i <- true_var$ad2 * A_i +
    true_var$cn2 * Cn_i +
    true_var$ce2 * U_i +
    true_var$mt2 * Mt_i +
    true_var$ee2 * I_i

  y_list[[i]] <- mvtnorm::rmvnorm(1, sigma = V_i)
  ytemp_list[[i]] <- paste("S", rownames(A_i))
}

cat("Simulated", n_families, "families\n")
#> Simulated 5 families
cat("Family sizes:", vapply(ped_list, nrow, integer(1)), "\n")
#> Family sizes: 25 25 25 25 25
```

### Building and Fitting the Multi-Group Model

We build a group model for each family and then combine them:

``` r
# Build group models for each family
group_models <- lapply(seq_len(n_families), function(i) {
  obs_data_i <- matrix(y_list[[i]], nrow = 1, dimnames = list(NULL, ytemp_list[[i]]))

  buildOneFamilyGroup(
    group_name = paste0("ped", i),
    Addmat = add_list[[i]],
    Nucmat = cn_list[[i]],
    Extmat = ce_list[[i]],
    Mtdmat = mt_list[[i]],
    full_df_row = obs_data_i,
    ytemp = ytemp_list[[i]]
  )
})

# Build the multi-group model
multi_model <- buildPedigreeMx(
  model_name = "MultiPedigreeModel",
  vars = start_vars,
  group_models = group_models
)

# Fit the model
fitted_multi <- mxRun(multi_model)
#> Running MultiPedigreeModel with 6 parameters
smr_multi <- summary(fitted_multi)
```

``` r
# Extract and compare estimates
estimates_multi <- c(
  Vad = fitted_multi$ModelOne$Vad$values[1, 1],
  Vcn = fitted_multi$ModelOne$Vcn$values[1, 1],
  Vce = fitted_multi$ModelOne$Vce$values[1, 1],
  Vmt = fitted_multi$ModelOne$Vmt$values[1, 1],
  Ver = fitted_multi$ModelOne$Ver$values[1, 1]
)

comparison_multi <- data.frame(
  Component = c("Additive genetic (Vad)", "Common nuclear (Vcn)",
                "Common extended (Vce)", "Mitochondrial (Vmt)",
                "Unique environment (Ver)"),
  True = truth,
  Estimated = round(estimates_multi, 4)
)
comparison_multi
#>                    Component True Estimated
#> Vad   Additive genetic (Vad)  0.5    0.4780
#> Vcn     Common nuclear (Vcn)  0.1    0.0645
#> Vce    Common extended (Vce)  0.0    0.0000
#> Vmt      Mitochondrial (Vmt)  0.0    0.0000
#> Ver Unique environment (Ver)  0.4    0.5457

cat("\n-2 Log Likelihood:", smr_multi$Minus2LogLikelihood, "\n")
#> 
#> -2 Log Likelihood: 352.1827
cat("Converged:", fitted_multi$output$status$code == 0, "\n")
#> Converged: TRUE
```

With multiple families providing more information, the estimates should
more closely approximate the true generating values.

## Using the High-Level `fitPedigreeModel()` Wrapper

For convenience,
[`fitPedigreeModel()`](https://r-computing-lab.github.io/BGmisc/reference/fitPedigreeModel.md)
wraps the build and fit steps together. It accepts pre-built group
models and uses
[`mxTryHard()`](https://rdrr.io/pkg/OpenMx/man/mxTryHard.html) for
robust optimization with multiple starts:

``` r
fitted_easy <- fitPedigreeModel(
  model_name = "EasyFit",
  vars = start_vars,
  data = NULL,
  group_models = group_models,
  tryhard = TRUE
)
#> Beginning initial fit attemptFit attempt 0, fit=352.182707904758, new current best! (was 361.708560299926)                                                                             
#> 
#>  Solution found!  Final fit=352.18271 (started at 361.70856)  (1 attempt(s): 1 valid, 0 errors)

summary(fitted_easy)
#> Summary of EasyFit 
#>  
#> free parameters:
#>     name       matrix row     col      Estimate  Std.Error A lbound ubound
#> 1    vad ModelOne.Vad   1       1  4.779743e-01 0.31046714    1e-10       
#> 2    vcn ModelOne.Vcn   1       1  6.448255e-02 0.09488079    1e-10       
#> 3    vce ModelOne.Vce   1       1  1.042504e-10 0.22037833 !     0!       
#> 4    vmt ModelOne.Vmt   1       1  1.008520e-10 0.10411390 !     0!       
#> 5    ver ModelOne.Ver   1       1  5.457066e-01 0.19822666    1e-10       
#> 6 meanLI       ped1.M   1 S 10101 -1.761523e-01 0.15313133                
#> 
#> Model Statistics: 
#>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
#>        Model:              6                    119              352.1827
#>    Saturated:             NA                     NA                    NA
#> Independence:             NA                     NA                    NA
#> Number of observations/statistics: 5/125
#> 
#> Information Criteria: 
#>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
#> AIC:       114.1827               364.1827                 322.1827
#> BIC:       160.6596               361.8393                 344.7898
#> To get additional fit indices, see help(mxRefModels)
#> timestamp: 2026-03-13 15:56:39 
#> Wall clock time: 0.1937933 secs 
#> optimizer:  SLSQP 
#> OpenMx version number: 2.22.11 
#> Need help?  See help(mxSummary)
```

## Understanding the Covariance Structure

The key equation underlying the model is:

$$V = \sigma_{a}^{2}\mathbf{A} + \sigma_{cn}^{2}\mathbf{C}_{n} + \sigma_{ce}^{2}\mathbf{U} + \sigma_{mt}^{2}\mathbf{M} + \sigma_{e}^{2}\mathbf{I}$$

where:

- $\mathbf{A}$ is the additive genetic relatedness matrix (from
  [`ped2add()`](https://r-computing-lab.github.io/BGmisc/reference/ped2add.md))
- $\mathbf{C}_{n}$ is the common nuclear environment matrix (from
  [`ped2cn()`](https://r-computing-lab.github.io/BGmisc/reference/ped2cn.md))
- $\mathbf{U}$ is a matrix of ones representing shared extended family
  environment (from
  [`ped2ce()`](https://r-computing-lab.github.io/BGmisc/reference/ped2ce.md))
- $\mathbf{M}$ is the mitochondrial relatedness matrix (from
  [`ped2mit()`](https://r-computing-lab.github.io/BGmisc/reference/ped2mit.md))
- $\mathbf{I}$ is the identity matrix (unique environment, one per
  person)
- $\sigma_{a}^{2},\sigma_{cn}^{2},\sigma_{ce}^{2},\sigma_{mt}^{2},\sigma_{e}^{2}$
  are the variance components to be estimated

This is an extension of the classical twin model to arbitrary pedigree
structures. The additive genetic relatedness matrix generalizes the
concept of MZ twins sharing 100% of genes and DZ twins sharing 50% – in
a pedigree, every pair of relatives has a specific coefficient of
relatedness determined by their genealogical connection.

## Summary

This vignette demonstrated the full workflow for pedigree-based variance
component modeling:

| Step | Function                                                                                                                                                                                                                                                                                                           | Purpose                                |
|------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------|
| 1    | [`simulatePedigree()`](https://r-computing-lab.github.io/BGmisc/reference/simulatePedigree.md)                                                                                                                                                                                                                     | Generate a multi-generational pedigree |
| 2    | [`ped2add()`](https://r-computing-lab.github.io/BGmisc/reference/ped2add.md), [`ped2cn()`](https://r-computing-lab.github.io/BGmisc/reference/ped2cn.md), [`ped2mit()`](https://r-computing-lab.github.io/BGmisc/reference/ped2mit.md), [`ped2ce()`](https://r-computing-lab.github.io/BGmisc/reference/ped2ce.md) | Compute relatedness matrices           |
| 3    | [`identifyComponentModel()`](https://r-computing-lab.github.io/BGmisc/reference/identifyComponentModel.md)                                                                                                                                                                                                         | Check model identification             |
| 4    | Simulate or prepare phenotypic data                                                                                                                                                                                                                                                                                | Observed data for model fitting        |
| 5    | [`buildOneFamilyGroup()`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md), [`buildPedigreeModelCovariance()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeModelCovariance.md)                                                                                       | Build OpenMx model components          |
| 6    | [`buildPedigreeMx()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md), [`mxRun()`](https://rdrr.io/pkg/OpenMx/man/mxRun.html) or [`fitPedigreeModel()`](https://r-computing-lab.github.io/BGmisc/reference/fitPedigreeModel.md)                                                             | Assemble and fit the model             |
| 7    | Multiple families                                                                                                                                                                                                                                                                                                  | Scale to multi-group pedigree models   |

The helper functions
([`buildPedigreeModelCovariance()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeModelCovariance.md),
[`buildOneFamilyGroup()`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md),
[`buildFamilyGroups()`](https://r-computing-lab.github.io/BGmisc/reference/buildFamilyGroups.md),
[`buildPedigreeMx()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md),
[`fitPedigreeModel()`](https://r-computing-lab.github.io/BGmisc/reference/fitPedigreeModel.md))
handle the mechanics of translating pedigree relatedness matrices into
proper OpenMx model specifications, allowing researchers to focus on the
substantive questions rather than the modeling boilerplate.
