# Fitting Pedigree-Based Variance Component Models

## Introduction

A core goal of behavior genetics is to decompose observed phenotypic
variance into genetic and environmental components. Extended pedigrees —
multi-generational families with known parentage — provide richer
information about relatedness than twin studies alone and allow
researchers to estimate a wider array of variance components.

The `BGmisc` package provides a complete pipeline for pedigree-based
variance component modeling:

1.  **Compute** relatedness matrices with
    [`ped2add()`](https://r-computing-lab.github.io/BGmisc/reference/ped2add.md),
    [`ped2cn()`](https://r-computing-lab.github.io/BGmisc/reference/ped2cn.md),
    [`ped2mit()`](https://r-computing-lab.github.io/BGmisc/reference/ped2mit.md),
    and
    [`ped2ce()`](https://r-computing-lab.github.io/BGmisc/reference/ped2ce.md)
2.  **Check identification** with
    [`identifyComponentModel()`](https://r-computing-lab.github.io/BGmisc/reference/identifyComponentModel.md)
3.  **Build** OpenMx group models with
    [`buildOneFamilyGroup()`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md)
4.  **Fit** the combined model with
    [`buildPedigreeMx()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md)
    and [`mxRun()`](https://rdrr.io/pkg/OpenMx/man/mxRun.html) (or the
    wrapper
    [`fitPedigreeModel()`](https://r-computing-lab.github.io/BGmisc/reference/fitPedigreeModel.md))

This vignette builds up in three parts: a single-family model, a
two-family multi-group model, and a scaled-up simulation study. An
extension vignette
`vignette("v61_pedigree_model_fitting", package = "BGmisc")` covers more
complex models using squirrel data from the Kluane Red Squirrel Project.

### Prerequisites

``` r

library(BGmisc)

has_openmx <- requireNamespace("OpenMx", quietly = TRUE)
has_mvtnorm <- requireNamespace("mvtnorm", quietly = TRUE)

if (has_openmx) {
  library(OpenMx)
} else {
  message(
    "OpenMx is not installed. Code will be shown but not executed.\n",
    "Install with: install.packages('OpenMx')"
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

run_models <- has_openmx
```

## Data Requirements at a Glance

Before diving in, here is a concise reference for what your data must
look like. Return to this section whenever something is unclear.

### Pedigree data frame

One row per individual, at minimum:

| Column  | Type              | Description                  |
|---------|-------------------|------------------------------|
| `ID`    | integer/character | Unique individual identifier |
| `dadID` | same as `ID`      | Father’s ID; `NA` if unknown |
| `momID` | same as `ID`      | Mother’s ID; `NA` if unknown |
| `sex`   | integer           | `0` = male, `1` = female     |
| `famID` | integer/character | Extended family identifier   |

Column names are flexible — pass `personID = "myID"`, `famID = "famID"`,
etc. to the `ped2*` functions.

> **Tip:** Use
> [`checkIDs()`](https://r-computing-lab.github.io/BGmisc/reference/checkIDs.md),
> [`checkSex()`](https://r-computing-lab.github.io/BGmisc/reference/checkSex.md),
> and `checkParents()` to validate your pedigree before computing
> relatedness matrices.

### Relatedness matrices

Each matrix returned by
[`ped2add()`](https://r-computing-lab.github.io/BGmisc/reference/ped2add.md),
[`ped2cn()`](https://r-computing-lab.github.io/BGmisc/reference/ped2cn.md),
etc. is square ($`n \times n`$), symmetric, and **labeled**: `rownames`
and `colnames` are the individual IDs. These labels are the link between
phenotype data and the model.

### Phenotype data format

The model-fitting functions expect phenotype data as a **one-row
matrix**, where:

- Each **column** is one individual.
- **Column names** must exactly match `rownames(add_matrix)` for that
  family — same IDs, same order.
- The vector of those column names is called `obs_ids` throughout this
  vignette.

If your raw data is in long format (one row per person), you need to
pivot it wide — this is shown step by step in Part 1 below.

### Missing phenotypes

Individuals present in the pedigree only to trace relatedness (e.g.,
deceased ancestors) must be **removed from the matrices** before
fitting. The `ped2*` functions include every individual; you subset them
explicitly to those with observed data.

------------------------------------------------------------------------

## Part 1: Single-Family Model

We start with one family from the built-in `hazard` dataset. Every step
is written out explicitly — so you can see exactly what happens.

``` r

data(hazard)

# Two families: famID 1 and famID 2
table(hazard$famID)
#> 
#>  1  2 
#> 18 25
```

### Step 1: Subset to one family and inspect

``` r

fam1 <- subset(hazard, famID == 1)

# What does the pedigree look like?
head(fam1[, c("famID", "ID", "sex", "dadID", "momID", "DA2")])
#>   famID ID sex dadID momID DA2
#> 1     1  1   1    NA    NA   1
#> 2     1  2   0    NA    NA   0
#> 3     1  3   0     2     1   1
#> 4     1  4   1     2     1   1
#> 5     1  7   0    NA    NA   0
#> 6     1  5   1     2     1   1
```

`DA2` is the phenotype we will model. It is binary in the actual
dataset; for a real continuous-trait analysis you would substitute your
own measure.

### Step 2: Compute relatedness matrices

``` r

# Additive genetic relatedness
add_mat1 <- ped2add(fam1,
  sparse = FALSE,
  famID = "famID", personID = "ID",
  momID = "momID", dadID = "dadID", sex = "sex",
  keep_ids = fam1$ID
) # keep all individuals for now


# Common nuclear environment (1 if same biological parents, 0 otherwise)
cn_mat1 <- ped2cn(fam1,
  sparse = FALSE,
  famID = "famID", personID = "ID",
  momID = "momID", dadID = "dadID", sex = "sex",
  keep_ids = fam1$ID
)

mt_mat1 <- ped2mit(fam1,
  sparse = FALSE,
  famID = "famID", personID = "ID",
  momID = "momID", dadID = "dadID", sex = "sex",
  keep_ids = fam1$ID
)

# The matrices are square and labeled with individual IDs
dim(add_mat1)
#> [1] 18 18
rownames(add_mat1)
#>  [1] "1"  "2"  "3"  "4"  "7"  "5"  "6"  "8"  "10" "9"  "11" "13" "12" "15" "14"
#> [16] "16" "17" "18"
```

### Step 3: Prepare the phenotype data

The model needs a **one-row matrix** with one column per individual, in
the same order as the relatedness matrix rows.

``` r

# Individual IDs in the order the matrices use
id_order1 <- rownames(add_mat1)

# Pull phenotype values, reordered to match the matrix
pheno_vals1 <- fam1$DA2[match(id_order1, as.character(fam1$ID))]
names(pheno_vals1) <- id_order1

# Who actually has an observed phenotype?
observed1 <- !is.na(pheno_vals1)
cat("Individuals in pedigree:", length(id_order1), "\n")
#> Individuals in pedigree: 18
cat("Individuals with observed phenotype:", sum(observed1), "\n")
#> Individuals with observed phenotype: 18

# obs_ids: the IDs of observed individuals — these will be column names
# They must be syntactically valid R names (OpenMx requirement)
obs_ids1 <- make.names(id_order1[observed1])

# One-row matrix: one column per observed individual.
# as.double() is required — OpenMx will error if the matrix storage mode
# is integer rather than double, even if the values look numeric.
pheno_row1 <- matrix(
  as.double(pheno_vals1[observed1]),
  nrow = 1,
  dimnames = list(NULL, obs_ids1)
)
pheno_row1[1, 1:6] # first few values
#> X1 X2 X3 X4 X7 X5 
#>  1  0  1  1  0  1
```

### Step 4: Subset matrices to observed individuals

Individuals without a phenotype must be removed from the matrices —
their rows and columns are dropped.

``` r

# Use the original (non-make.names) IDs to index the matrix
raw_obs_ids1 <- id_order1[observed1]

add_mat1_obs <- add_mat1[raw_obs_ids1, raw_obs_ids1]
cn_mat1_obs <- cn_mat1[raw_obs_ids1, raw_obs_ids1]
mt_mat1_obs <- mt_mat1[raw_obs_ids1, raw_obs_ids1]

# Rename rows/cols to match obs_ids1 (the make.names versions)
rownames(add_mat1_obs) <- colnames(add_mat1_obs) <- obs_ids1
rownames(cn_mat1_obs) <- colnames(cn_mat1_obs) <- obs_ids1
rownames(mt_mat1_obs) <- colnames(mt_mat1_obs) <- obs_ids1

dim(add_mat1_obs)
#> [1] 18 18
```

### Step 5: Check model identification

Before fitting, verify that the variance components you want to estimate
are statistically identifiable given this pedigree’s structure.

``` r

id_check1 <- identifyComponentModel(
  A  = add_mat1_obs,
  Cn = cn_mat1_obs,
  Mt = mt_mat1_obs,
  E  = diag(1, nrow(add_mat1_obs))
)
#> Component model is identified.
id_check1
#> $identified
#> [1] TRUE
#> 
#> $nidp
#> character(0)
```

### Step 6: Build and fit the model

``` r

# Starting values for variance components
start_vars <- list(
  ad2 = 0.3, # additive genetic
  cn2 = 0.1, # common nuclear environment
  ce2 = 0, # common extended (not estimated here)
  mt2 = 0.1, # mitochondrial
  dd2 = 0, # dominance (not estimated here)
  am2 = 0, # A x Mt interaction (not estimated here)
  ee2 = 0.5 # unique environment
)

# Build the group model for family 1
group1 <- buildOneFamilyGroup(
  group_name  = "family1",
  Addmat      = add_mat1_obs,
  Nucmat      = cn_mat1_obs,
  Mtdmat      = mt_mat1_obs,
  full_df_row = pheno_row1,
  obs_ids     = obs_ids1
)

# Wrap in a full pedigree model and fit
model1 <- buildPedigreeMx(
  model_name   = "SingleFamilyModel",
  vars         = start_vars,
  group_models = list(group1)
)

fitted1 <- mxRun(model1)
#> Running SingleFamilyModel with 5 parameters
summary(fitted1)
#> Summary of SingleFamilyModel 
#>  
#> free parameters:
#>     name       matrix row col     Estimate  Std.Error A lbound ubound
#> 1    vad ModelOne.Vad   1   1 1.000000e-10 0.15553823 !     0!       
#> 2    vcn ModelOne.Vcn   1   1 1.004647e-10 0.03925027 !     0!       
#> 3    vmt ModelOne.Vmt   1   1 7.134327e-02 0.09249026    1e-10       
#> 4    ver ModelOne.Ver   1   1 1.056225e-01 0.07034232    1e-10       
#> 5 meanLI    family1.M   1  X1 1.728194e-01 0.14739428                
#> 
#> Model Statistics: 
#>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
#>        Model:              5                     13              16.38826
#>    Saturated:            189                   -171                    NA
#> Independence:             36                    -18                    NA
#> Number of observations/statistics: 1/18
#> 
#> Information Criteria: 
#>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
#> AIC:      -9.611741               26.38826                14.388259
#> BIC:      16.388259               16.38826                 5.991051
#> To get additional fit indices, see help(mxRefModels)
#> timestamp: 2026-07-06 18:04:53 
#> Wall clock time: 0.08201265 secs 
#> optimizer:  SLSQP 
#> OpenMx version number: 2.22.11 
#> Need help?  See help(mxSummary)
```

The estimated variance components are in `fitted1$ModelOne`:

``` r

cat("Additive genetic (Vad):", fitted1$ModelOne$Vad$values, "\n")
#> Additive genetic (Vad): 1e-10
cat("Common nuclear  (Vcn):", fitted1$ModelOne$Vcn$values, "\n")
#> Common nuclear  (Vcn): 1.004647e-10
cat("Mitochondrial (Vmt):", fitted1$ModelOne$Vmt$values, "\n")
#> Mitochondrial (Vmt): 0.07134327
cat("Unique environ. (Ver):", fitted1$ModelOne$Ver$values, "\n")
#> Unique environ. (Ver): 0.1056225
```

With a single family, estimates have wide uncertainty. Part 2 adds a
second family to improve precision.

## Part 2: Two-Family Multi-Group Model

Multi-group modeling means that **variance component parameters are
shared** across families, but each family has its own relatedness matrix
and phenotype data. We write out the steps for family 2 explicitly — the
same steps as Part 1, just on different data — so you can see exactly
what repeats and what changes.

### Family 2: matrices

``` r

fam2 <- subset(hazard, famID == 2)

add_mat2 <- ped2add(fam2,
  sparse = FALSE,
  famID = "famID", personID = "ID",
  momID = "momID", dadID = "dadID", sex = "sex",
  keep_ids = fam2$ID
)

cn_mat2 <- ped2cn(fam2,
  sparse = FALSE,
  famID = "famID", personID = "ID",
  momID = "momID", dadID = "dadID", sex = "sex",
  keep_ids = fam2$ID
)

mt_mat2 <- ped2mit(fam2,
  sparse = FALSE,
  famID = "famID", personID = "ID",
  momID = "momID", dadID = "dadID", sex = "sex",
  keep_ids = fam2$ID
)

dim(add_mat2)
#> [1] 25 25
```

### Family 2: phenotype data

``` r

id_order2 <- rownames(add_mat2)

pheno_vals2 <- fam2$DA2[match(id_order2, as.character(fam2$ID))]
names(pheno_vals2) <- id_order2

observed2 <- !is.na(pheno_vals2)
obs_ids2 <- make.names(id_order2[observed2])
raw_obs_ids2 <- id_order2[observed2]

pheno_row2 <- matrix(
  as.double(pheno_vals2[observed2]),
  nrow = 1,
  dimnames = list(NULL, obs_ids2)
)

cat("Family 2 observed:", sum(observed2), "of", length(id_order2), "\n")
#> Family 2 observed: 25 of 25
```

### Family 2: subset matrices

``` r

add_mat2_obs <- add_mat2[raw_obs_ids2, raw_obs_ids2]
cn_mat2_obs <- cn_mat2[raw_obs_ids2, raw_obs_ids2]
mt_mat2_obs <- mt_mat2[raw_obs_ids2, raw_obs_ids2]

rownames(add_mat2_obs) <- colnames(add_mat2_obs) <- obs_ids2
rownames(cn_mat2_obs) <- colnames(cn_mat2_obs) <- obs_ids2
rownames(mt_mat2_obs) <- colnames(mt_mat2_obs) <- obs_ids2
```

### Check identification across both families

``` r

n_total <- nrow(add_mat1_obs) + nrow(add_mat2_obs)

id_check2 <- identifyComponentModel(
  A  = list(add_mat1_obs, add_mat2_obs),
  Cn = list(cn_mat1_obs, cn_mat2_obs),
  Mt = list(mt_mat1_obs, mt_mat2_obs),
  E  = diag(1, n_total)
)
#> Component model is identified.
id_check2
#> $identified
#> [1] TRUE
#> 
#> $nidp
#> character(0)
```

### Build and fit the two-family model

Each family gets its own group model. The key difference from Part 1:
[`buildPedigreeMx()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md)
now receives both groups, and
[`mxFitFunctionMultigroup()`](https://rdrr.io/pkg/OpenMx/man/mxFitFunctionMultigroup.html)
combines their likelihoods. The variance component parameters (`Vad`,
`Vcn`, `Ver`) are estimated jointly.

``` r

# Family 2 group model (identical call structure to family 1)
group2 <- buildOneFamilyGroup(
  group_name  = "family2",
  Addmat      = add_mat2_obs,
  Nucmat      = cn_mat2_obs,
  Mtdmat      = mt_mat2_obs,
  full_df_row = pheno_row2,
  obs_ids     = obs_ids2
)

# Both groups share the same variance component parameters
model2 <- buildPedigreeMx(
  model_name   = "TwoFamilyModel",
  vars         = start_vars,
  group_models = list(group1, group2)
)

fitted2 <- mxRun(model2)
#> Running TwoFamilyModel with 5 parameters
summary(fitted2)
#> Summary of TwoFamilyModel 
#>  
#> free parameters:
#>     name       matrix row col     Estimate  Std.Error A lbound ubound
#> 1    vad ModelOne.Vad   1   1 0.0000000001         NA !     0!       
#> 2    vcn ModelOne.Vcn   1   1 0.0000000001         NA !     0!       
#> 3    vmt ModelOne.Vmt   1   1 0.0575927403 0.02303160    1e-10       
#> 4    ver ModelOne.Ver   1   1 0.0649114655         NA    1e-10       
#> 5 meanLI    family1.M   1  X1 0.1318671852 0.08308344                
#> 
#> Model Statistics: 
#>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
#>        Model:              5                     38               19.7511
#>    Saturated:             NA                     NA                    NA
#> Independence:             NA                     NA                    NA
#> Number of observations/statistics: 2/43
#> 
#> Information Criteria: 
#>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
#> AIC:     -56.248897               29.75110                 14.75110
#> BIC:      -6.588489               23.21684                 10.79231
#> To get additional fit indices, see help(mxRefModels)
#> timestamp: 2026-07-06 18:04:54 
#> Wall clock time: 0.08731341 secs 
#> optimizer:  SLSQP 
#> OpenMx version number: 2.22.11 
#> Need help?  See help(mxSummary)
```

``` r

cat("Additive genetic (Vad):", fitted2$ModelOne$Vad$values, "\n")
#> Additive genetic (Vad): 1e-10
cat("Common nuclear  (Vcn):", fitted2$ModelOne$Vcn$values, "\n")
#> Common nuclear  (Vcn): 1e-10
cat("Mitochondrial (Vmt):", fitted2$ModelOne$Vmt$values, "\n")
#> Mitochondrial (Vmt): 0.05759274
cat("Unique environ. (Ver):", fitted2$ModelOne$Ver$values, "\n")
#> Unique environ. (Ver): 0.06491147
```

The estimates from both families together are generally more stable than
those from either family alone.

------------------------------------------------------------------------

## Part 3: Scaling Up — Many Families and Parameter Recovery

Once you understand the pattern from Parts 1 and 2, you can scale to
many families using a loop. We switch to **simulated data** here so we
have known true parameter values to check our estimates against.

### Simulate pedigrees and data

``` r

library(mvtnorm)
set.seed(2024)

# True variance components
true_var <- list(
  ad2 = 0.40, cn2 = 0.10, ee2 = 0.40,
  ce2 = 0, mt2 = 0.10, dd2 = 0, am2 = 0
)

n_families <- 15

# Pre-allocate storage
add_list <- vector("list", n_families)
cn_list <- vector("list", n_families)
mt_list <- vector("list", n_families)
obs_ids_list <- vector("list", n_families)
pheno_list <- vector("list", n_families)

for (i in seq_len(n_families)) {
  ped_i <- simulatePedigree(kpc = 3, Ngen = 4, marR = 0.6)

  A_i <- as.matrix(ped2add(ped_i, sparse = FALSE))
  Cn_i <- as.matrix(ped2cn(ped_i, sparse = FALSE))
  Mt_i <- as.matrix(ped2mit(ped_i, sparse = FALSE))
  n_i <- nrow(A_i)

  # Implied covariance from true parameters
  V_i <- true_var$ad2 * A_i +
    true_var$cn2 * Cn_i +
    true_var$mt2 * Mt_i +
    true_var$ee2 * diag(1, n_i)

  y_i <- rmvnorm(1, sigma = V_i)

  ids_i <- make.names(rownames(A_i))
  rownames(A_i) <- colnames(A_i) <- ids_i
  rownames(Cn_i) <- colnames(Cn_i) <- ids_i
  rownames(Mt_i) <- colnames(Mt_i) <- ids_i

  add_list[[i]] <- A_i
  cn_list[[i]] <- Cn_i
  mt_list[[i]] <- Mt_i
  obs_ids_list[[i]] <- ids_i
  pheno_list[[i]] <- matrix(y_i, nrow = 1, dimnames = list(NULL, ids_i))
}

cat("Family sizes:", vapply(add_list, nrow, integer(1)), "\n")
#> Family sizes: 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
```

### Build and fit the multi-family model

``` r

group_models <- lapply(seq_len(n_families), function(i) {
  buildOneFamilyGroup(
    group_name  = paste0("ped", i),
    Addmat      = add_list[[i]],
    Nucmat      = cn_list[[i]],
    Mtdmat      = mt_list[[i]],
    full_df_row = pheno_list[[i]],
    obs_ids     = obs_ids_list[[i]]
  )
})

multi_model <- buildPedigreeMx(
  model_name   = "MultiPedigreeModel",
  vars         = start_vars,
  group_models = group_models
)

fitted_multi <- mxRun(multi_model)
#> Running MultiPedigreeModel with 5 parameters
```

### Compare estimates to true values

``` r

data.frame(
  Component = c(
    "Additive genetic (Vad)",
    "Common nuclear  (Vcn)",
    "Mitochondrial (Vmt)",
    "Unique environ. (Ver)"
  ),
  True = c(
    true_var$ad2, true_var$cn2,
    true_var$mt2,
    true_var$ee2
  ),
  Estimated = round(c(
    fitted_multi$ModelOne$Vad$values,
    fitted_multi$ModelOne$Vcn$values,
    fitted_multi$ModelOne$Vmt$values,
    fitted_multi$ModelOne$Ver$values
  ), 4)
)
#>                Component True Estimated
#> 1 Additive genetic (Vad)  0.4    0.5990
#> 2  Common nuclear  (Vcn)  0.1    0.1461
#> 3    Mitochondrial (Vmt)  0.1    0.0398
#> 4  Unique environ. (Ver)  0.4    0.2870
```

With ten families, estimates should be substantially closer to the true
values than with a single family.

### Using the high-level wrapper

For convenience,
[`fitPedigreeModel()`](https://r-computing-lab.github.io/BGmisc/reference/fitPedigreeModel.md)
wraps the build and fit steps together, and uses
[`mxTryHard()`](https://rdrr.io/pkg/OpenMx/man/mxTryHard.html) for
robust optimization:

``` r

fitted_easy <- fitPedigreeModel(
  model_name   = "EasyFit",
  vars         = start_vars,
  group_models = group_models,
  tryhard      = TRUE
)
#> Beginning initial fit attemptFit attempt 0, fit=1008.36320769182, new current best! (was 1013.57665124202)Final run, for Hessian and/or standard errors and/or confidence intervals                                                                             
#> 
#>  Solution found!  Final fit=1008.3632 (started at 1013.5767)  (1 attempt(s): 1 valid, 0 errors)
summary(fitted_easy)
#> Summary of EasyFit 
#>  
#> free parameters:
#>     name       matrix row    col    Estimate  Std.Error A lbound ubound
#> 1    vad ModelOne.Vad   1      1  0.59903086 0.16291591    1e-10       
#> 2    vcn ModelOne.Vcn   1      1  0.14613696 0.06795751    1e-10       
#> 3    vmt ModelOne.Vmt   1      1  0.03980215 0.06904713 !     0!       
#> 4    ver ModelOne.Ver   1      1  0.28696799 0.09940299    1e-10       
#> 5 meanLI       ped1.M   1 X10102 -0.01901727 0.09329989                
#> 
#> confidence intervals:
#>           lbound   estimate    ubound note
#> vad 0.3042803327 0.59903086 0.9551061     
#> vcn           NA 0.14613696        NA  !!!
#> vmt 0.0000000001 0.03980215 0.2105368     
#> ver 0.1036228265 0.28696799 0.4836285     
#>   To investigate missing CIs, run summary() again, with verbose=T, to see CI details. 
#> 
#> Model Statistics: 
#>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
#>        Model:              5                    370              1008.363
#>    Saturated:             NA                     NA                    NA
#> Independence:             NA                     NA                    NA
#> Number of observations/statistics: 15/375
#> 
#> Information Criteria: 
#>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
#> AIC:     268.363208               1018.363                 1025.030
#> BIC:       6.384633               1021.903                 1006.639
#> To get additional fit indices, see help(mxRefModels)
#> timestamp: 2026-07-06 18:04:58 
#> Wall clock time: 1.713894 secs 
#> optimizer:  SLSQP 
#> OpenMx version number: 2.22.11 
#> Need help?  See help(mxSummary)
```

## Understanding the Covariance Structure

The model estimated above is defined by:

``` math
V = \sigma^2_a \mathbf{A} + \sigma^2_{cn} \mathbf{C}_n + \sigma^2_e \mathbf{I}
```

where $`\mathbf{A}`$ is the additive genetic relatedness matrix,
$`\mathbf{C}_n`$ is the common nuclear environment matrix,
$`\mathbf{I}`$ is the identity matrix (unique environment), and the
$`\sigma^2`$ terms are the variance components estimated by the model.

This is an extension of the classical twin model to arbitrary pedigree
structures. Additional components can be added by passing `Extmat`
(common extended environment), `Mtdmat` (mitochondrial), or `Dmgmat`
(dominance) to
[`buildOneFamilyGroup()`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md).
See
[`vignette("v1_modelingvariancecomponents", package = "BGmisc")`](https://r-computing-lab.github.io/BGmisc/articles/v1_modelingvariancecomponents.md)
for background on identification.

## Summary

| Part | Data | Key point |
|----|----|----|
| 1 — Single family | `hazard`, family 1 | Full pipeline, one group model |
| 2 — Two families | `hazard`, families 1 & 2 | Same steps twice; [`buildPedigreeMx()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md) combines them |
| 3 — Many families | Simulated | Loop over families; verify parameter recovery |

The helper functions
([`buildOneFamilyGroup()`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md),
[`buildPedigreeMx()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md),
[`fitPedigreeModel()`](https://r-computing-lab.github.io/BGmisc/reference/fitPedigreeModel.md))
handle the translation from pedigree relatedness matrices into OpenMx
model specifications. The pattern is always the same: one call to
[`buildOneFamilyGroup()`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md)
per family, then one call to
[`buildPedigreeMx()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md)
to combine them.
