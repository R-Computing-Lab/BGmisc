# Extended: Fitting Pedigree-Based Variance Component Models

``` r
run_models
#> [1] FALSE
has_openmx
#> [1] TRUE
interactive()
#> [1] FALSE
bgmisc_testing
#> [1] FALSE
bgmisc_testing_env
#> [1] ""
```

## Introduction

This vignette extends the example from
[`vignette("v60_pedigree_model_fitting", package = "BGmisc")`](https://r-computing-lab.github.io/BGmisc/articles/v60_pedigree_model_fitting.md)
to show how to fit models to multiple families simultaneously. The key
functions are
[`buildOneFamilyGroup()`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md)
and
[`buildPedigreeMx()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md),
which translate pedigree data into OpenMx model specifications.

## Scaling Up to Many Families

Here we replicate several estimates of heritability across multiple
families of red squirrels. We use the `redsquirrels_full` dataset from
the `ggpedigree` package, which contains pedigree and phenotypic data on
red squirrels from the Kluane region of the Yukon, Canada. The phenotype
we analyze is lifetime reproductive success (LRS), which is a count of
the number of offspring that survive to a certain age.

``` r
library(ggpedigree) # for pedigree data)
library(tidyverse)
data("redsquirrels_full")

ped_krsp <- redsquirrels_full |>
  transmute(
    ID = as.integer(personID),
    momID = as.integer(momID),
    dadID = as.integer(dadID),
    sex = sex,
    famID = as.integer(famID),
    lrs = lrs
  )


summarizeFamilies(ped_krsp, famID = "famID")$family_summary |> arrange(desc(count))
#>       famID count lrs_mean lrs_median lrs_min lrs_max   lrs_sd
#>       <int> <int>    <num>      <num>   <num>   <num>    <num>
#>    1:     8  3803 1.173895          0       0      31 3.599582
#>    2:   729  1249      NaN         NA      NA      NA       NA
#>    3:   160   103 1.482143          0       0      16 3.411164
#>    4:    38    85 1.304348          0       0      26 4.345468
#>    5:   226    71 1.421053          0       0      21 4.365910
#>   ---                                                         
#> 1096:  1096     1      NaN         NA      NA      NA       NA
#> 1097:  1097     1      NaN         NA      NA      NA       NA
#> 1098:  1098     1      NaN         NA      NA      NA       NA
#> 1099:  1099     1      NaN         NA      NA      NA       NA
#> 1100:  1100     1      NaN         NA      NA      NA       NA
```

As you can see, there are 7799 squirells in 1100 families in this
dataset. However, but we are missing several phenotyped individuals
(i.e., individuals with non-missing LRS). To fit a multigroup pedigree
model, we need to subset to families that have a sufficient number of
phenotyped individuals. Here we set a minimum family size of 10
phenotyped individuals, which leaves us with a reduced number of
families to include in the analysis.

``` r
minim_family_size <- 10

ped_krsp_subset <- ped_krsp |>
  group_by(famID) |>
  filter(sum(!is.na(lrs)) >= minim_family_size) |>
  ungroup()

id_families <- unique(ped_krsp_subset$famID)
n_families <- length(id_families)
```

We now have 17 families with at least 10 phenotyped individuals each.
Once we subset to the families with sufficient phenotyped individuals,
we can prepare the relatedness matrices and phenotypic data for each
family. We pre-allocate lists to store these matrices and data for each
family, which will be used to build the OpenMx models.

``` r
# Pre-allocate storage
add_list <- vector("list", length(n_families))
cn_list <- vector("list", length(n_families))
mt_list <- vector("list", length(n_families))
obs_ids_list <- vector("list", length(n_families))
pheno_list <- vector("list", length(n_families))
```

Creating starting values for the variance components is important for
model convergence. Here we set some reasonable starting values based on
prior knowledge or expectations about the trait and the population.
These starting values can be adjusted based on the specific dataset and
trait being analyzed.

``` r
# Starting values for variance components
start_vars <- list(
  ad2 = 0.1, # additive genetic
  cn2 = 0.1, # common nuclear environment
  ce2 = 0, # common extended (not estimated here)
  mt2 = 0.1, # mitochondrial
  dd2 = 0, # dominance (not estimated here)
  am2 = 0, # A x Mt interaction (not estimated here)
  ee2 = 0.7 # unique environment
)
```

Now we loop through each family, extract the pedigree and phenotypic
data, and prepare the relatedness matrices (additive genetic, common
nuclear environment, and mitochondrial) for each family. We also prepare
the phenotypic data in the format required for OpenMx. Finally, we build
the group models for ACE, MACE, and CE models for each family. The
[`buildOneFamilyGroup()`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md)
function is used to create the model specification for each family, and
the
[`buildPedigreeMx()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md)
function is used to combine these group models into a single multigroup
model that can be fitted in OpenMx. As you can see, we are fitting three
different models: ACE (additive genetic, common environment, unique
environment), MACE (additive genetic, common environment, mitochondrial,
unique environment), and CE (common environment, unique environment).
This allows us to compare the models and assess the contribution of each
variance component to the trait of interest (LRS) in these squirrels.

``` r
for (i in seq_len(n_families)) {
  ped_i <- subset(ped_krsp_subset, famID == id_families[i])
  phenotypic_ids <- ped_i$ID[!is.na(ped_i$lrs)]
  A_i <- as.matrix(ped2add(ped_i, sparse = FALSE, keep_ids = phenotypic_ids))
  Cn_i <- as.matrix(ped2cn(ped_i, sparse = FALSE, keep_ids = phenotypic_ids))
  Mt_i <- as.matrix(ped2mit(ped_i, sparse = FALSE, keep_ids = phenotypic_ids))

  n_i <- nrow(A_i)
  id_order_i <- rownames(A_i)

  pheno_vals <- ped_i$lrs[match(id_order_i, as.character(ped_i$ID))]

  obs_ids_i <- make.names(id_order_i[!is.na(pheno_vals)])
  pheno_row_i <- matrix(as.double(pheno_vals[!is.na(pheno_vals)]),
    nrow = 1,
    dimnames = list(NULL, obs_ids_i)
  )

  rownames(A_i) <- colnames(A_i) <- obs_ids_i
  rownames(Cn_i) <- colnames(Cn_i) <- obs_ids_i
  rownames(Mt_i) <- colnames(Mt_i) <- obs_ids_i

  add_list[[i]] <- A_i
  cn_list[[i]] <- Cn_i
  mt_list[[i]] <- Mt_i
  obs_ids_list[[i]] <- obs_ids_i
  pheno_list[[i]] <- pheno_row_i
}
```

For convenience, we build the group models for each family separately
for the ACE, MACE, and CE models. This allows us to easily compare the
models and assess the contribution of each variance component to the
trait of interest (LRS) in these squirrels. The
[`buildOneFamilyGroup()`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md)
function is used to create the model specification for each family, and
the
[`buildPedigreeMx()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md)
function is used to combine these group models into a single multigroup
model that can be fitted in OpenMx.

``` r
group_models_mace <- lapply(seq_len(n_families), function(i) {
  buildOneFamilyGroup(
    group_name  = paste0("ped", i),
    Addmat      = add_list[[i]],
    Nucmat      = cn_list[[i]],
    Mtdmat      = mt_list[[i]],
    full_df_row = pheno_list[[i]],
    obs_ids     = obs_ids_list[[i]]
  )
})

group_models_ace <- lapply(seq_len(n_families), function(i) {
  buildOneFamilyGroup(
    group_name  = paste0("ped", i),
    Addmat      = add_list[[i]],
    Nucmat      = cn_list[[i]],
    Mtdmat      = NULL,
    full_df_row = pheno_list[[i]],
    obs_ids     = obs_ids_list[[i]]
  )
})


group_models_ce <- lapply(seq_len(n_families), function(i) {
  buildOneFamilyGroup(
    group_name  = paste0("ped", i),
    Addmat      = NULL,
    Nucmat      = cn_list[[i]],
    Mtdmat      = NULL,
    full_df_row = pheno_list[[i]],
    obs_ids     = obs_ids_list[[i]]
  )
})
```

The [`lapply()`](https://rdrr.io/r/base/lapply.html) function is used to
create a list of group models for each family, which are then combined
into a single multigroup model using the buildPedigreeMx() function. The
resulting models can then be fitted in OpenMx to estimate the variance
components for each family and compare the ACE, MACE, and CE models.

``` r
multi_model_mace <- buildPedigreeMx(
  model_name = "MultiPedigreeModel",
  vars = start_vars,
  group_models = group_models_mace,
  ci = TRUE
)

multi_model_ace <- buildPedigreeMx(
  model_name = "MultiPedigreeModel",
  vars = start_vars,
  group_models = group_models_ace,
  ci = TRUE
)

multi_model_ce <- buildPedigreeMx(
  model_name = "MultiPedigreeModel",
  vars = start_vars,
  group_models = group_models_ce,
  ci = TRUE
)
```

Note that fitting these models can take some time, especially with many
families and large pedigrees. The
[`mxTryHard()`](https://rdrr.io/pkg/OpenMx/man/mxTryHard.html) function
can be used to attempt to find better-fitting solutions if the initial
optimization does not converge well. In practice, you may want to
experiment with different starting values or optimization settings to
improve convergence.

    fitted_multi_mace <- mxRun(multi_model_mace)
    fitted_multi_ace <- mxRun(multi_model_ace)
    fitted_multi_ce <- mxRun(multi_model_ce)

As you can see, we have fit a multigroup pedigree model using 17
families of several thousand squirrels. The model includes additive
genetic variance (Vad), common nuclear environmental variance (Vcn), and
unique environmental variance (Ver). The mitochondrial variance
component (Vmt) was included in the MACE model but not estimated in the
ACE model. The results show the proportion of total variance attributed
to each component, which can be interpreted as heritability and
environmental contributions to the phenotype of interest (LRS) in these
squirrels.

``` r
summary(fitted_multi_mace)
#> Summary of MultiPedigreeModel 
#>  
#> free parameters:
#>     name       matrix row  col     Estimate  Std.Error A lbound ubound
#> 1    vad ModelOne.Vad   1    1 0.0000000001 0.30183118 !     0!       
#> 2    vcn ModelOne.Vcn   1    1 4.1144290806 1.19868649    1e-10       
#> 3    vmt ModelOne.Vmt   1    1 0.0000000001 0.09662163 !     0!       
#> 4    ver ModelOne.Ver   1    1 9.2999051171 0.98364306    1e-10       
#> 5 meanLI       ped1.M   1 X208 1.2569248071 0.11367465                
#> 
#> confidence intervals:
#>     lbound     estimate ubound note
#> vad     NA 0.0000000001     NA  !!!
#> vcn     NA 4.1144290806     NA  !!!
#> vmt     NA 0.0000000001     NA  !!!
#> ver     NA 9.2999051171     NA  !!!
#>   To investigate missing CIs, run summary() again, with verbose=T, to see CI details. 
#> 
#> Model Statistics: 
#>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
#>        Model:              5                   2670              14465.02
#>    Saturated:             NA                     NA                    NA
#> Independence:             NA                     NA                    NA
#> Number of observations/statistics: 17/2675
#> 
#> Information Criteria: 
#>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
#> AIC:       9125.024               14475.02                 14480.48
#> BIC:       6900.344               14479.19                 14463.86
#> To get additional fit indices, see help(mxRefModels)
#> timestamp: 2026-03-30 22:09:23 
#> Wall clock time: 9521.762 secs 
#> optimizer:  SLSQP 
#> OpenMx version number: 2.22.11 
#> Need help?  See help(mxSummary)
summary(fitted_multi_mace)$CI
#>     lbound     estimate ubound note
#> vad     NA 0.0000000001     NA  !!!
#> vcn     NA 4.1144290806     NA  !!!
#> vmt     NA 0.0000000001     NA  !!!
#> ver     NA 9.2999051171     NA  !!!
total_var_mace <- sum(
  fitted_multi_mace$ModelOne$Vad$values,
  fitted_multi_mace$ModelOne$Vcn$values,
  fitted_multi_mace$ModelOne$Vmt$values,
  fitted_multi_mace$ModelOne$Ver$values
)
```

``` r
cat("Additive genetic (Vad):", fitted_multi_mace$ModelOne$Vad$values / total_var_mace, "\n")
#> Additive genetic (Vad): 7.454712e-12
cat("Common nuclear  (Vcn):", fitted_multi_mace$ModelOne$Vcn$values / total_var_mace, "\n")
#> Common nuclear  (Vcn): 0.3067188
cat("Mitochondrial (Vmt):", fitted_multi_mace$ModelOne$Vmt$values / total_var_mace, "\n")
#> Mitochondrial (Vmt): 7.454712e-12
cat("Unique environ. (Ver):", fitted_multi_mace$ModelOne$Ver$values / total_var_mace, "\n")
#> Unique environ. (Ver): 0.6932812
```

``` r
summary(fitted_multi_ace, verbose = T)
#> Summary of MultiPedigreeModel 
#>  
#> data:
#> $ped1
#>       X208        X347        X760         X761        X950        X951  
#>  Min.   :0   Min.   :0   Min.   :14   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:14   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :14   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :14   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:14   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :14   Max.   :0   Max.   :0   Max.   :0  
#>       X953       X1436       X1437       X1636       X1637       X1641  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X1932  
#>  Min.   :0  
#>  1st Qu.:0  
#>  Median :0  
#>  Mean   :0  
#>  3rd Qu.:0  
#>  Max.   :0  
#> 
#> $ped2
#>       X220        X221        X222        X228        X232        X369   
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :13  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:13  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :13  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :13  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:13  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :13  
#>       X450        X451        X452        X467        X468        X496  
#>  Min.   :0   Min.   :0   Min.   :2   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:2   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :2   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :2   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:2   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :2   Max.   :0   Max.   :0   Max.   :0  
#>       X637        X638        X647        X652        X656        X658  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>       X666         X667        X668        X674        X752        X753  
#>  Min.   :14   Min.   :7   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:14   1st Qu.:7   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :14   Median :7   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :14   Mean   :7   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:14   3rd Qu.:7   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :14   Max.   :7   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>       X756         X758        X759        X765        X766         X767  
#>  Min.   :11   Min.   :4   Min.   :0   Min.   :9   Min.   :10   Min.   :0  
#>  1st Qu.:11   1st Qu.:4   1st Qu.:0   1st Qu.:9   1st Qu.:10   1st Qu.:0  
#>  Median :11   Median :4   Median :0   Median :9   Median :10   Median :0  
#>  Mean   :11   Mean   :4   Mean   :0   Mean   :9   Mean   :10   Mean   :0  
#>  3rd Qu.:11   3rd Qu.:4   3rd Qu.:0   3rd Qu.:9   3rd Qu.:10   3rd Qu.:0  
#>  Max.   :11   Max.   :4   Max.   :0   Max.   :9   Max.   :10   Max.   :0  
#>       X768         X771         X773         X774        X775        X796  
#>  Min.   :14   Min.   :27   Min.   :28   Min.   :0   Min.   :8   Min.   :0  
#>  1st Qu.:14   1st Qu.:27   1st Qu.:28   1st Qu.:0   1st Qu.:8   1st Qu.:0  
#>  Median :14   Median :27   Median :28   Median :0   Median :8   Median :0  
#>  Mean   :14   Mean   :27   Mean   :28   Mean   :0   Mean   :8   Mean   :0  
#>  3rd Qu.:14   3rd Qu.:27   3rd Qu.:28   3rd Qu.:0   3rd Qu.:8   3rd Qu.:0  
#>  Max.   :14   Max.   :27   Max.   :28   Max.   :0   Max.   :8   Max.   :0  
#>       X798        X800        X806        X820        X838        X839  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>       X948        X959        X960        X964        X965        X967  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>       X969        X970        X971        X972        X975        X976  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X1000       X1003       X1009       X1015       X1017       X1025   
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :11  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:11  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :11  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :11  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:11  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :11  
#>      X1028       X1029       X1030       X1031       X1032       X1034  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :5   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:5   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :5   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :5   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:5   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :5   Max.   :0   Max.   :0  
#>      X1042        X1043       X1044       X1068       X1095       X1241  
#>  Min.   :10   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:10   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :10   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :10   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:10   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :10   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X1243        X1247       X1250       X1262       X1265        X1269  
#>  Min.   :28   Min.   :8   Min.   :0   Min.   :0   Min.   :20   Min.   :4  
#>  1st Qu.:28   1st Qu.:8   1st Qu.:0   1st Qu.:0   1st Qu.:20   1st Qu.:4  
#>  Median :28   Median :8   Median :0   Median :0   Median :20   Median :4  
#>  Mean   :28   Mean   :8   Mean   :0   Mean   :0   Mean   :20   Mean   :4  
#>  3rd Qu.:28   3rd Qu.:8   3rd Qu.:0   3rd Qu.:0   3rd Qu.:20   3rd Qu.:4  
#>  Max.   :28   Max.   :8   Max.   :0   Max.   :0   Max.   :20   Max.   :4  
#>      X1273       X1274       X1276        X1284       X1285       X1286  
#>  Min.   :0   Min.   :0   Min.   :31   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:31   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :31   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :31   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:31   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :31   Max.   :0   Max.   :0   Max.   :0  
#>      X1290       X1296       X1298        X1304       X1305       X1307  
#>  Min.   :5   Min.   :3   Min.   :15   Min.   :3   Min.   :3   Min.   :0  
#>  1st Qu.:5   1st Qu.:3   1st Qu.:15   1st Qu.:3   1st Qu.:3   1st Qu.:0  
#>  Median :5   Median :3   Median :15   Median :3   Median :3   Median :0  
#>  Mean   :5   Mean   :3   Mean   :15   Mean   :3   Mean   :3   Mean   :0  
#>  3rd Qu.:5   3rd Qu.:3   3rd Qu.:15   3rd Qu.:3   3rd Qu.:3   3rd Qu.:0  
#>  Max.   :5   Max.   :3   Max.   :15   Max.   :3   Max.   :3   Max.   :0  
#>      X1308       X1310       X1311       X1314        X1316       X1322  
#>  Min.   :0   Min.   :0   Min.   :2   Min.   :19   Min.   :3   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:2   1st Qu.:19   1st Qu.:3   1st Qu.:0  
#>  Median :0   Median :0   Median :2   Median :19   Median :3   Median :0  
#>  Mean   :0   Mean   :0   Mean   :2   Mean   :19   Mean   :3   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:2   3rd Qu.:19   3rd Qu.:3   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :2   Max.   :19   Max.   :3   Max.   :0  
#>      X1323        X1324        X1325       X1327        X1332       X1335   
#>  Min.   :11   Min.   :22   Min.   :6   Min.   :26   Min.   :0   Min.   :10  
#>  1st Qu.:11   1st Qu.:22   1st Qu.:6   1st Qu.:26   1st Qu.:0   1st Qu.:10  
#>  Median :11   Median :22   Median :6   Median :26   Median :0   Median :10  
#>  Mean   :11   Mean   :22   Mean   :6   Mean   :26   Mean   :0   Mean   :10  
#>  3rd Qu.:11   3rd Qu.:22   3rd Qu.:6   3rd Qu.:26   3rd Qu.:0   3rd Qu.:10  
#>  Max.   :11   Max.   :22   Max.   :6   Max.   :26   Max.   :0   Max.   :10  
#>      X1336        X1337        X1341       X1342       X1343       X1345   
#>  Min.   :15   Min.   :12   Min.   :0   Min.   :0   Min.   :0   Min.   :16  
#>  1st Qu.:15   1st Qu.:12   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:16  
#>  Median :15   Median :12   Median :0   Median :0   Median :0   Median :16  
#>  Mean   :15   Mean   :12   Mean   :0   Mean   :0   Mean   :0   Mean   :16  
#>  3rd Qu.:15   3rd Qu.:12   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:16  
#>  Max.   :15   Max.   :12   Max.   :0   Max.   :0   Max.   :0   Max.   :16  
#>      X1347       X1355        X1358       X1363       X1364       X1365  
#>  Min.   :0   Min.   :15   Min.   :8   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:15   1st Qu.:8   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :15   Median :8   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :15   Mean   :8   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:15   3rd Qu.:8   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :15   Max.   :8   Max.   :0   Max.   :0   Max.   :0  
#>      X1379       X1384       X1385       X1386       X1387       X1389  
#>  Min.   :9   Min.   :0   Min.   :3   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:9   1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :9   Median :0   Median :3   Median :0   Median :0   Median :0  
#>  Mean   :9   Mean   :0   Mean   :3   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:9   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :9   Max.   :0   Max.   :3   Max.   :0   Max.   :0   Max.   :0  
#>      X1408       X1410       X1458       X1459       X1528       X1531  
#>  Min.   :3   Min.   :5   Min.   :6   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:3   1st Qu.:5   1st Qu.:6   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :3   Median :5   Median :6   Median :0   Median :0   Median :0  
#>  Mean   :3   Mean   :5   Mean   :6   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:3   3rd Qu.:5   3rd Qu.:6   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :3   Max.   :5   Max.   :6   Max.   :0   Max.   :0   Max.   :0  
#>      X1533       X1535       X1536       X1537       X1538       X1540  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X1546       X1550       X1551       X1552       X1556       X1560  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X1565       X1566       X1568       X1569        X1579       X1580  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :11   Min.   :0   Min.   :2  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:11   1st Qu.:0   1st Qu.:2  
#>  Median :0   Median :0   Median :0   Median :11   Median :0   Median :2  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :11   Mean   :0   Mean   :2  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:11   3rd Qu.:0   3rd Qu.:2  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :11   Max.   :0   Max.   :2  
#>      X1587       X1589       X1595       X1596       X1597       X1600  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X1601       X1602       X1608       X1612       X1613        X1614   
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :20   Min.   :11  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:20   1st Qu.:11  
#>  Median :0   Median :0   Median :0   Median :0   Median :20   Median :11  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :20   Mean   :11  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:20   3rd Qu.:11  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :20   Max.   :11  
#>      X1621       X1622       X1650       X1651       X1652       X1653  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X1654       X1655       X1660       X1661       X1663       X1664  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X1665       X1666       X1668       X1669        X1671       X1672  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :19   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:19   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :19   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :19   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:19   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :19   Max.   :0   Max.   :0  
#>      X1673       X1675       X1678       X1691       X1693       X1716  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X1844       X1845       X1846       X1847        X1848       X1852  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :12   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:12   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :12   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :12   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:12   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :12   Max.   :0   Max.   :0  
#>      X1854       X1874       X1875       X1876        X1881       X1883   
#>  Min.   :0   Min.   :0   Min.   :4   Min.   :18   Min.   :8   Min.   :15  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:4   1st Qu.:18   1st Qu.:8   1st Qu.:15  
#>  Median :0   Median :0   Median :4   Median :18   Median :8   Median :15  
#>  Mean   :0   Mean   :0   Mean   :4   Mean   :18   Mean   :8   Mean   :15  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:4   3rd Qu.:18   3rd Qu.:8   3rd Qu.:15  
#>  Max.   :0   Max.   :0   Max.   :4   Max.   :18   Max.   :8   Max.   :15  
#>      X1884       X1885       X1887        X1892       X1893       X1894   
#>  Min.   :9   Min.   :7   Min.   :16   Min.   :0   Min.   :0   Min.   :12  
#>  1st Qu.:9   1st Qu.:7   1st Qu.:16   1st Qu.:0   1st Qu.:0   1st Qu.:12  
#>  Median :9   Median :7   Median :16   Median :0   Median :0   Median :12  
#>  Mean   :9   Mean   :7   Mean   :16   Mean   :0   Mean   :0   Mean   :12  
#>  3rd Qu.:9   3rd Qu.:7   3rd Qu.:16   3rd Qu.:0   3rd Qu.:0   3rd Qu.:12  
#>  Max.   :9   Max.   :7   Max.   :16   Max.   :0   Max.   :0   Max.   :12  
#>      X1895       X1898       X1901       X1904        X1905       X1911  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :19   Min.   :9   Min.   :1  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:19   1st Qu.:9   1st Qu.:1  
#>  Median :0   Median :0   Median :0   Median :19   Median :9   Median :1  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :19   Mean   :9   Mean   :1  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:19   3rd Qu.:9   3rd Qu.:1  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :19   Max.   :9   Max.   :1  
#>      X1912       X1916       X1917       X1926        X1927       X1936  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :15   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:15   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :15   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :15   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:15   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :15   Max.   :0   Max.   :0  
#>      X1940       X1942       X1943       X1945       X1946       X1947  
#>  Min.   :0   Min.   :3   Min.   :0   Min.   :0   Min.   :5   Min.   :9  
#>  1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:5   1st Qu.:9  
#>  Median :0   Median :3   Median :0   Median :0   Median :5   Median :9  
#>  Mean   :0   Mean   :3   Mean   :0   Mean   :0   Mean   :5   Mean   :9  
#>  3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:5   3rd Qu.:9  
#>  Max.   :0   Max.   :3   Max.   :0   Max.   :0   Max.   :5   Max.   :9  
#>      X1948       X1949       X1950       X1958       X1959        X1960   
#>  Min.   :0   Min.   :3   Min.   :0   Min.   :8   Min.   :15   Min.   :13  
#>  1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:8   1st Qu.:15   1st Qu.:13  
#>  Median :0   Median :3   Median :0   Median :8   Median :15   Median :13  
#>  Mean   :0   Mean   :3   Mean   :0   Mean   :8   Mean   :15   Mean   :13  
#>  3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:8   3rd Qu.:15   3rd Qu.:13  
#>  Max.   :0   Max.   :3   Max.   :0   Max.   :8   Max.   :15   Max.   :13  
#>      X1963       X1972       X1973       X1977       X1978       X1979  
#>  Min.   :0   Min.   :0   Min.   :4   Min.   :0   Min.   :5   Min.   :2  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:4   1st Qu.:0   1st Qu.:5   1st Qu.:2  
#>  Median :0   Median :0   Median :4   Median :0   Median :5   Median :2  
#>  Mean   :0   Mean   :0   Mean   :4   Mean   :0   Mean   :5   Mean   :2  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:4   3rd Qu.:0   3rd Qu.:5   3rd Qu.:2  
#>  Max.   :0   Max.   :0   Max.   :4   Max.   :0   Max.   :5   Max.   :2  
#>      X1991       X1992        X2001        X2002       X2009       X2010  
#>  Min.   :0   Min.   :10   Min.   :15   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:10   1st Qu.:15   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :10   Median :15   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :10   Mean   :15   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:10   3rd Qu.:15   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :10   Max.   :15   Max.   :0   Max.   :0   Max.   :0  
#>      X2014        X2016       X2017        X2119       X2120       X2121  
#>  Min.   :16   Min.   :8   Min.   :18   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:16   1st Qu.:8   1st Qu.:18   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :16   Median :8   Median :18   Median :0   Median :0   Median :0  
#>  Mean   :16   Mean   :8   Mean   :18   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:16   3rd Qu.:8   3rd Qu.:18   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :16   Max.   :8   Max.   :18   Max.   :0   Max.   :0   Max.   :0  
#>      X2124       X2139       X2140       X2141       X2146       X2147  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2149       X2151       X2152       X2153       X2154        X2155  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :15   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:15   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :15   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :15   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:15   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :15   Max.   :0  
#>      X2156       X2157       X2162       X2163       X2165       X2177  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2194       X2195       X2196       X2197       X2198       X2200  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2201       X2202       X2204       X2205       X2206       X2207   
#>  Min.   :6   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :16  
#>  1st Qu.:6   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:16  
#>  Median :6   Median :0   Median :0   Median :0   Median :0   Median :16  
#>  Mean   :6   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :16  
#>  3rd Qu.:6   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:16  
#>  Max.   :6   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :16  
#>      X2208       X2209       X2219       X2220       X2221       X2230  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2231       X2232       X2234       X2235       X2238       X2239  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2252       X2254       X2257       X2258       X2259       X2275  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :6  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:6  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :6  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :6  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:6  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :6  
#>      X2278       X2287       X2290       X2292       X2294       X2298  
#>  Min.   :7   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:7   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :7   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :7   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:7   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :7   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2299       X2300       X2305       X2308       X2312       X2313  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :2  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:2  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :2  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :2  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:2  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :2  
#>      X2314       X2315       X2316       X2320       X2323       X2324  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2330       X2335       X2336       X2339       X2344       X2346  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2348       X2355       X2361       X2445       X2448       X2454  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2458       X2460       X2461        X2463       X2466       X2467  
#>  Min.   :2   Min.   :0   Min.   :10   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:2   1st Qu.:0   1st Qu.:10   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :2   Median :0   Median :10   Median :0   Median :0   Median :0  
#>  Mean   :2   Mean   :0   Mean   :10   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:2   3rd Qu.:0   3rd Qu.:10   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :2   Max.   :0   Max.   :10   Max.   :0   Max.   :0   Max.   :0  
#>      X2469       X2475       X2482       X2484       X2486        X2488  
#>  Min.   :3   Min.   :3   Min.   :7   Min.   :0   Min.   :19   Min.   :0  
#>  1st Qu.:3   1st Qu.:3   1st Qu.:7   1st Qu.:0   1st Qu.:19   1st Qu.:0  
#>  Median :3   Median :3   Median :7   Median :0   Median :19   Median :0  
#>  Mean   :3   Mean   :3   Mean   :7   Mean   :0   Mean   :19   Mean   :0  
#>  3rd Qu.:3   3rd Qu.:3   3rd Qu.:7   3rd Qu.:0   3rd Qu.:19   3rd Qu.:0  
#>  Max.   :3   Max.   :3   Max.   :7   Max.   :0   Max.   :19   Max.   :0  
#>      X2489       X2494        X2496       X2500       X2507       X2508  
#>  Min.   :9   Min.   :18   Min.   :0   Min.   :4   Min.   :3   Min.   :0  
#>  1st Qu.:9   1st Qu.:18   1st Qu.:0   1st Qu.:4   1st Qu.:3   1st Qu.:0  
#>  Median :9   Median :18   Median :0   Median :4   Median :3   Median :0  
#>  Mean   :9   Mean   :18   Mean   :0   Mean   :4   Mean   :3   Mean   :0  
#>  3rd Qu.:9   3rd Qu.:18   3rd Qu.:0   3rd Qu.:4   3rd Qu.:3   3rd Qu.:0  
#>  Max.   :9   Max.   :18   Max.   :0   Max.   :4   Max.   :3   Max.   :0  
#>      X2509       X2511       X2512       X2514       X2522       X2526   
#>  Min.   :0   Min.   :8   Min.   :4   Min.   :0   Min.   :0   Min.   :13  
#>  1st Qu.:0   1st Qu.:8   1st Qu.:4   1st Qu.:0   1st Qu.:0   1st Qu.:13  
#>  Median :0   Median :8   Median :4   Median :0   Median :0   Median :13  
#>  Mean   :0   Mean   :8   Mean   :4   Mean   :0   Mean   :0   Mean   :13  
#>  3rd Qu.:0   3rd Qu.:8   3rd Qu.:4   3rd Qu.:0   3rd Qu.:0   3rd Qu.:13  
#>  Max.   :0   Max.   :8   Max.   :4   Max.   :0   Max.   :0   Max.   :13  
#>      X2544       X2545       X2547       X2548       X2553       X2556  
#>  Min.   :9   Min.   :0   Min.   :0   Min.   :0   Min.   :3   Min.   :0  
#>  1st Qu.:9   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0  
#>  Median :9   Median :0   Median :0   Median :0   Median :3   Median :0  
#>  Mean   :9   Mean   :0   Mean   :0   Mean   :0   Mean   :3   Mean   :0  
#>  3rd Qu.:9   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0  
#>  Max.   :9   Max.   :0   Max.   :0   Max.   :0   Max.   :3   Max.   :0  
#>      X2557       X2558       X2559       X2560       X2561       X2565  
#>  Min.   :0   Min.   :5   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:5   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :5   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :5   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:5   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :5   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2577       X2581       X2590       X2591       X2592       X2594  
#>  Min.   :0   Min.   :6   Min.   :5   Min.   :6   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:6   1st Qu.:5   1st Qu.:6   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :6   Median :5   Median :6   Median :0   Median :0  
#>  Mean   :0   Mean   :6   Mean   :5   Mean   :6   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:6   3rd Qu.:5   3rd Qu.:6   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :6   Max.   :5   Max.   :6   Max.   :0   Max.   :0  
#>      X2600       X2602       X2610       X2611       X2625        X2781  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :17   Min.   :7  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:17   1st Qu.:7  
#>  Median :0   Median :0   Median :0   Median :0   Median :17   Median :7  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :17   Mean   :7  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:17   3rd Qu.:7  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :17   Max.   :7  
#>      X2783       X2784       X2786       X2789       X2790       X2791  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :3  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :3  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :3  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :3  
#>      X2794       X2795        X2797       X2798       X2799       X2813  
#>  Min.   :0   Min.   :12   Min.   :0   Min.   :8   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:12   1st Qu.:0   1st Qu.:8   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :12   Median :0   Median :8   Median :0   Median :0  
#>  Mean   :0   Mean   :12   Mean   :0   Mean   :8   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:12   3rd Qu.:0   3rd Qu.:8   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :12   Max.   :0   Max.   :8   Max.   :0   Max.   :0  
#>      X2816       X2817       X2820       X2826       X2828       X2829  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :2   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:2   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :2   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :2   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:2   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :2   Max.   :0  
#>      X2831       X2832       X2835       X2839       X2841       X2844  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2849       X2853       X2855        X2856       X2857       X2867  
#>  Min.   :0   Min.   :0   Min.   :13   Min.   :7   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:13   1st Qu.:7   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :13   Median :7   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :13   Mean   :7   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:13   3rd Qu.:7   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :13   Max.   :7   Max.   :0   Max.   :0  
#>      X2868        X2869       X2870       X2871       X2872       X2874  
#>  Min.   :18   Min.   :9   Min.   :6   Min.   :4   Min.   :0   Min.   :0  
#>  1st Qu.:18   1st Qu.:9   1st Qu.:6   1st Qu.:4   1st Qu.:0   1st Qu.:0  
#>  Median :18   Median :9   Median :6   Median :4   Median :0   Median :0  
#>  Mean   :18   Mean   :9   Mean   :6   Mean   :4   Mean   :0   Mean   :0  
#>  3rd Qu.:18   3rd Qu.:9   3rd Qu.:6   3rd Qu.:4   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :18   Max.   :9   Max.   :6   Max.   :4   Max.   :0   Max.   :0  
#>      X2875       X2878        X2888       X2899       X2901        X2903  
#>  Min.   :0   Min.   :11   Min.   :0   Min.   :4   Min.   :16   Min.   :0  
#>  1st Qu.:0   1st Qu.:11   1st Qu.:0   1st Qu.:4   1st Qu.:16   1st Qu.:0  
#>  Median :0   Median :11   Median :0   Median :4   Median :16   Median :0  
#>  Mean   :0   Mean   :11   Mean   :0   Mean   :4   Mean   :16   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:11   3rd Qu.:0   3rd Qu.:4   3rd Qu.:16   3rd Qu.:0  
#>  Max.   :0   Max.   :11   Max.   :0   Max.   :4   Max.   :16   Max.   :0  
#>      X2904       X2907       X2908       X2909       X2912       X2917   
#>  Min.   :0   Min.   :9   Min.   :0   Min.   :0   Min.   :0   Min.   :12  
#>  1st Qu.:0   1st Qu.:9   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:12  
#>  Median :0   Median :9   Median :0   Median :0   Median :0   Median :12  
#>  Mean   :0   Mean   :9   Mean   :0   Mean   :0   Mean   :0   Mean   :12  
#>  3rd Qu.:0   3rd Qu.:9   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:12  
#>  Max.   :0   Max.   :9   Max.   :0   Max.   :0   Max.   :0   Max.   :12  
#>      X2918       X2921       X2922       X2924       X2925       X2927  
#>  Min.   :3   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :3   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :3   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :3   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2928       X2929       X2930       X2933       X2936       X2937  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :6  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:6  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :6  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :6  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:6  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :6  
#>      X2938        X2939       X2940       X2943       X2946       X2947  
#>  Min.   :12   Min.   :3   Min.   :9   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:12   1st Qu.:3   1st Qu.:9   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :12   Median :3   Median :9   Median :0   Median :0   Median :0  
#>  Mean   :12   Mean   :3   Mean   :9   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:12   3rd Qu.:3   3rd Qu.:9   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :12   Max.   :3   Max.   :9   Max.   :0   Max.   :0   Max.   :0  
#>      X2952       X2955        X2957       X2958       X2959       X2960  
#>  Min.   :0   Min.   :11   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:11   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :11   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :11   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:11   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :11   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2961       X2962       X2964       X2965       X2966        X2967  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :2   Min.   :10   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:2   1st Qu.:10   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :2   Median :10   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :2   Mean   :10   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:2   3rd Qu.:10   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :2   Max.   :10   Max.   :0  
#>      X2969       X2970       X2975       X2976       X2978       X2979  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :5   Min.   :9   Min.   :3  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:5   1st Qu.:9   1st Qu.:3  
#>  Median :0   Median :0   Median :0   Median :5   Median :9   Median :3  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :5   Mean   :9   Mean   :3  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:5   3rd Qu.:9   3rd Qu.:3  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :5   Max.   :9   Max.   :3  
#>      X2981        X2982       X2990       X2992       X2996       X2998  
#>  Min.   :14   Min.   :0   Min.   :7   Min.   :0   Min.   :2   Min.   :3  
#>  1st Qu.:14   1st Qu.:0   1st Qu.:7   1st Qu.:0   1st Qu.:2   1st Qu.:3  
#>  Median :14   Median :0   Median :7   Median :0   Median :2   Median :3  
#>  Mean   :14   Mean   :0   Mean   :7   Mean   :0   Mean   :2   Mean   :3  
#>  3rd Qu.:14   3rd Qu.:0   3rd Qu.:7   3rd Qu.:0   3rd Qu.:2   3rd Qu.:3  
#>  Max.   :14   Max.   :0   Max.   :7   Max.   :0   Max.   :2   Max.   :3  
#>      X3000       X3002       X3003       X3004       X3013       X3014  
#>  Min.   :0   Min.   :6   Min.   :7   Min.   :0   Min.   :0   Min.   :4  
#>  1st Qu.:0   1st Qu.:6   1st Qu.:7   1st Qu.:0   1st Qu.:0   1st Qu.:4  
#>  Median :0   Median :6   Median :7   Median :0   Median :0   Median :4  
#>  Mean   :0   Mean   :6   Mean   :7   Mean   :0   Mean   :0   Mean   :4  
#>  3rd Qu.:0   3rd Qu.:6   3rd Qu.:7   3rd Qu.:0   3rd Qu.:0   3rd Qu.:4  
#>  Max.   :0   Max.   :6   Max.   :7   Max.   :0   Max.   :0   Max.   :4  
#>      X3017       X3026       X3028       X3034       X3035       X3036  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :4   Min.   :0   Min.   :3  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:4   1st Qu.:0   1st Qu.:3  
#>  Median :0   Median :0   Median :0   Median :4   Median :0   Median :3  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :4   Mean   :0   Mean   :3  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:4   3rd Qu.:0   3rd Qu.:3  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :4   Max.   :0   Max.   :3  
#>      X3037       X3038       X3044       X3046       X3047       X3048  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :6   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:6   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :6   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :6   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:6   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :6   Max.   :0   Max.   :0  
#>      X3053       X3054       X3065       X3066       X3068       X3080  
#>  Min.   :0   Min.   :0   Min.   :3   Min.   :2   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:2   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :3   Median :2   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :3   Mean   :2   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:2   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :3   Max.   :2   Max.   :0   Max.   :0  
#>      X3082       X3083       X3084       X3085       X3089       X3095  
#>  Min.   :0   Min.   :0   Min.   :2   Min.   :0   Min.   :4   Min.   :3  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:2   1st Qu.:0   1st Qu.:4   1st Qu.:3  
#>  Median :0   Median :0   Median :2   Median :0   Median :4   Median :3  
#>  Mean   :0   Mean   :0   Mean   :2   Mean   :0   Mean   :4   Mean   :3  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:2   3rd Qu.:0   3rd Qu.:4   3rd Qu.:3  
#>  Max.   :0   Max.   :0   Max.   :2   Max.   :0   Max.   :4   Max.   :3  
#>      X3096       X3101       X3102       X3105       X3107       X3113  
#>  Min.   :3   Min.   :0   Min.   :0   Min.   :3   Min.   :0   Min.   :2  
#>  1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:2  
#>  Median :3   Median :0   Median :0   Median :3   Median :0   Median :2  
#>  Mean   :3   Mean   :0   Mean   :0   Mean   :3   Mean   :0   Mean   :2  
#>  3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:2  
#>  Max.   :3   Max.   :0   Max.   :0   Max.   :3   Max.   :0   Max.   :2  
#>      X3131       X3132       X3135        X3136       X3138       X3150  
#>  Min.   :0   Min.   :0   Min.   :12   Min.   :0   Min.   :4   Min.   :3  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:12   1st Qu.:0   1st Qu.:4   1st Qu.:3  
#>  Median :0   Median :0   Median :12   Median :0   Median :4   Median :3  
#>  Mean   :0   Mean   :0   Mean   :12   Mean   :0   Mean   :4   Mean   :3  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:12   3rd Qu.:0   3rd Qu.:4   3rd Qu.:3  
#>  Max.   :0   Max.   :0   Max.   :12   Max.   :0   Max.   :4   Max.   :3  
#>      X3151       X3153       X3229       X3230       X3232       X3234  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3235       X3239       X3241       X3245       X3246       X3261  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3262       X3263       X3264       X3270       X3272       X3273  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3275       X3277       X3280       X3282       X3284       X3286  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3287       X3289       X3300       X3302       X3306       X3307  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3309       X3312       X3313       X3314       X3315       X3318  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3319       X3320       X3327       X3328       X3331       X3334  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3337       X3338       X3339       X3349       X3350       X3355  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3363       X3365       X3368       X3370       X3371       X3372  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3373       X3374       X3375       X3377       X3380       X3384  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3391       X3392       X3395       X3397       X3404       X3407  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3419       X3420       X3427       X3428       X3431       X3432  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3434       X3439       X3441       X3443       X3444       X3448  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3450       X3453       X3455       X3458       X3464       X3468  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3469       X3471       X3475       X3477       X3478       X3479  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3485       X3492       X3494       X3503       X3504       X3505  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3506       X3508       X3511       X3514       X3516       X3524  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3525       X3526       X3537       X3538       X3564       X3568  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3570       X3573       X3577       X3584       X3585       X3587  
#>  Min.   :0   Min.   :0   Min.   :8   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:8   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :8   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :8   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:8   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :8   Max.   :0   Max.   :0   Max.   :0  
#>      X3589       X3591       X3592       X3594       X3600       X3601  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3602       X3605       X3618       X3619       X3621       X3625  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3630       X3632       X3635       X3639       X3640       X3644  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3660       X3731       X3734       X3742       X3743       X3746  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3750       X3751       X3758       X3759       X3761       X3762  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3769       X3772       X3773       X3778       X3783       X3785  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :3  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :3  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :3  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :3  
#>      X3786       X3787       X3795       X3796       X3797       X3799  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3802       X3803       X3806       X3807       X3810       X3812  
#>  Min.   :6   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:6   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :6   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :6   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:6   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :6   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3813       X3815       X3816       X3820       X3832       X3837  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3838       X3853       X3854       X3862       X3864        X3868  
#>  Min.   :0   Min.   :0   Min.   :6   Min.   :0   Min.   :15   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:6   1st Qu.:0   1st Qu.:15   1st Qu.:0  
#>  Median :0   Median :0   Median :6   Median :0   Median :15   Median :0  
#>  Mean   :0   Mean   :0   Mean   :6   Mean   :0   Mean   :15   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:6   3rd Qu.:0   3rd Qu.:15   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :6   Max.   :0   Max.   :15   Max.   :0  
#>      X3870       X3871       X3876       X3881       X3882       X3883  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3888       X4055       X4056       X4060       X4062       X4063  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4064       X4079       X4082        X4086       X4087       X4089  
#>  Min.   :0   Min.   :0   Min.   :10   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:10   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :10   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :10   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:10   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :10   Max.   :0   Max.   :0   Max.   :0  
#>      X4091       X4096       X4097       X4101       X4102       X4106  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4107       X4108       X4110       X4112       X4114       X4117  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4121       X4123        X4128       X4129       X4136       X4142  
#>  Min.   :0   Min.   :26   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:26   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :26   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :26   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:26   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :26   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4147       X4150       X4156       X4157       X4158       X4159   
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :7   Min.   :0   Min.   :14  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:7   1st Qu.:0   1st Qu.:14  
#>  Median :0   Median :0   Median :0   Median :7   Median :0   Median :14  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :7   Mean   :0   Mean   :14  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:7   3rd Qu.:0   3rd Qu.:14  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :7   Max.   :0   Max.   :14  
#>      X4161        X4162       X4166       X4167       X4168       X4171  
#>  Min.   :19   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:19   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :19   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :19   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:19   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :19   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4173       X4176       X4177       X4186       X4188       X4192  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4193       X4196       X4203       X4205       X4208       X4211  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4212       X4213       X4214       X4215       X4223       X4225  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4226       X4231       X4248       X4250       X4258       X4262  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4263       X4264       X4266       X4268       X4270       X4272  
#>  Min.   :6   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:6   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :6   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :6   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:6   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :6   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4273       X4276       X4277        X4279       X4281       X4283  
#>  Min.   :0   Min.   :0   Min.   :11   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:11   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :11   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :11   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:11   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :11   Max.   :0   Max.   :0   Max.   :0  
#>      X4284       X4285       X4286       X4287       X4288       X4289  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4299       X4300       X4302       X4313       X4314       X4315  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4317       X4318       X4323       X4324       X4325       X4327  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4328       X4329        X4334       X4335       X4338       X4422  
#>  Min.   :0   Min.   :10   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:10   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :10   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :10   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:10   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :10   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4423       X4424       X4427       X4428       X4429       X4434  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :4  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:4  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :4  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :4  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:4  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :4  
#>      X4435       X4436       X4437       X4438       X4439       X4440  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4441       X4442       X4443       X4444       X4445       X4446  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :3   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :3   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :3   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :3   Max.   :0   Max.   :0  
#>      X4447       X4448       X4449       X4450       X4451       X4452  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4456       X4457       X4458       X4459       X4460       X4461   
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :21  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:21  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :21  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :21  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:21  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :21  
#>      X4462       X4463       X4464       X4465       X4466       X4467  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4468       X4469       X4470       X4471       X4472       X4473  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4474        X4475       X4476       X4477       X4478       X4479   
#>  Min.   :10   Min.   :3   Min.   :4   Min.   :3   Min.   :0   Min.   :13  
#>  1st Qu.:10   1st Qu.:3   1st Qu.:4   1st Qu.:3   1st Qu.:0   1st Qu.:13  
#>  Median :10   Median :3   Median :4   Median :3   Median :0   Median :13  
#>  Mean   :10   Mean   :3   Mean   :4   Mean   :3   Mean   :0   Mean   :13  
#>  3rd Qu.:10   3rd Qu.:3   3rd Qu.:4   3rd Qu.:3   3rd Qu.:0   3rd Qu.:13  
#>  Max.   :10   Max.   :3   Max.   :4   Max.   :3   Max.   :0   Max.   :13  
#>      X4480       X4481       X4482       X4483       X4484       X4485  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4486       X4487       X4488       X4489       X4490       X4491  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4492       X4494       X4495       X4496        X4498       X4499  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :15   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:15   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :15   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :15   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:15   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :15   Max.   :0   Max.   :0  
#>      X4500       X4504       X4505       X4506       X4507       X4508  
#>  Min.   :0   Min.   :1   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:1   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :1   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :1   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:1   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :1   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4509       X4510       X4512       X4513       X4514       X4515  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4516       X4517       X4518       X4519       X4523       X4527  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :8   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:8   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :8   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :8   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:8   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :8   Max.   :0  
#>      X4528       X4529       X4530       X4531       X4532       X4533  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4534       X4542       X4543       X4544       X4548       X4549   
#>  Min.   :9   Min.   :0   Min.   :5   Min.   :0   Min.   :0   Min.   :15  
#>  1st Qu.:9   1st Qu.:0   1st Qu.:5   1st Qu.:0   1st Qu.:0   1st Qu.:15  
#>  Median :9   Median :0   Median :5   Median :0   Median :0   Median :15  
#>  Mean   :9   Mean   :0   Mean   :5   Mean   :0   Mean   :0   Mean   :15  
#>  3rd Qu.:9   3rd Qu.:0   3rd Qu.:5   3rd Qu.:0   3rd Qu.:0   3rd Qu.:15  
#>  Max.   :9   Max.   :0   Max.   :5   Max.   :0   Max.   :0   Max.   :15  
#>      X4550       X4551       X4552       X4553       X4554       X4555  
#>  Min.   :0   Min.   :0   Min.   :9   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:9   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :9   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :9   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:9   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :9   Max.   :0   Max.   :0   Max.   :0  
#>      X4556       X4557       X4558       X4559       X4560       X4561  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4562       X4563       X4564       X4565       X4566       X4567  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4568       X4569       X4570       X4571        X4576       X4577  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :14   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:14   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :14   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :14   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:14   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :14   Max.   :0   Max.   :0  
#>      X4578       X4579       X4580       X4581       X4582       X4583  
#>  Min.   :0   Min.   :0   Min.   :7   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:7   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :7   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :7   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:7   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :7   Max.   :0   Max.   :0   Max.   :0  
#>      X4590       X4591       X4592       X4593       X4594       X4595  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4596       X4597       X4718       X4719       X4720       X4721  
#>  Min.   :0   Min.   :4   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:4   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :4   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :4   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:4   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :4   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4722        X4723       X4724       X4725       X4726       X4727  
#>  Min.   :10   Min.   :0   Min.   :0   Min.   :7   Min.   :0   Min.   :0  
#>  1st Qu.:10   1st Qu.:0   1st Qu.:0   1st Qu.:7   1st Qu.:0   1st Qu.:0  
#>  Median :10   Median :0   Median :0   Median :7   Median :0   Median :0  
#>  Mean   :10   Mean   :0   Mean   :0   Mean   :7   Mean   :0   Mean   :0  
#>  3rd Qu.:10   3rd Qu.:0   3rd Qu.:0   3rd Qu.:7   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :10   Max.   :0   Max.   :0   Max.   :7   Max.   :0   Max.   :0  
#>      X4728       X4729       X4730       X4731       X4732       X4733  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4734       X4735       X4736       X4737       X4738       X4739  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4740       X4741       X4742       X4743       X4744       X4745  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :3   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :3   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :3   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :3   Max.   :0   Max.   :0  
#>      X4746       X4747       X4748       X4749       X4750       X4751  
#>  Min.   :0   Min.   :8   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:8   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :8   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :8   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:8   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :8   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4752       X4753       X4754       X4755       X4756       X4757  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4758       X4759       X4760       X4761       X4762       X4763  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4764       X4765       X4766       X4767       X4768       X4769  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4773       X4774       X4775       X4776        X4777       X4778  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :17   Min.   :0   Min.   :3  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:17   1st Qu.:0   1st Qu.:3  
#>  Median :0   Median :0   Median :0   Median :17   Median :0   Median :3  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :17   Mean   :0   Mean   :3  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:17   3rd Qu.:0   3rd Qu.:3  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :17   Max.   :0   Max.   :3  
#>      X4779       X4780       X4781       X4782       X4783       X4784  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :2  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:2  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :2  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :2  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:2  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :2  
#>      X4785       X4789       X4790       X4791       X4792       X4793  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4794        X4795       X4796       X4797       X4798       X4799  
#>  Min.   :17   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:17   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :17   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :17   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:17   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :17   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4800       X4801       X4802       X4803       X4804       X4805   
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :10  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:10  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :10  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :10  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:10  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :10  
#>      X4808       X4809       X4810       X4811       X4812       X4813  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4831       X4832       X4833       X4834       X4835       X4836  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :1   Min.   :1   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:1   1st Qu.:1   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :1   Median :1   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :1   Mean   :1   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:1   3rd Qu.:1   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :1   Max.   :1   Max.   :0  
#>      X4837       X4838       X4841       X4847       X4848       X4849  
#>  Min.   :0   Min.   :0   Min.   :4   Min.   :0   Min.   :6   Min.   :9  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:4   1st Qu.:0   1st Qu.:6   1st Qu.:9  
#>  Median :0   Median :0   Median :4   Median :0   Median :6   Median :9  
#>  Mean   :0   Mean   :0   Mean   :4   Mean   :0   Mean   :6   Mean   :9  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:4   3rd Qu.:0   3rd Qu.:6   3rd Qu.:9  
#>  Max.   :0   Max.   :0   Max.   :4   Max.   :0   Max.   :6   Max.   :9  
#>      X4850       X4851       X4852       X4853       X4854       X4855  
#>  Min.   :2   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:2   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :2   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :2   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:2   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :2   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4856       X4857       X4858       X4859       X4860       X4861  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4862       X4863       X4864       X4865       X4866       X4867  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4868        X4869       X4870       X4871       X4872       X4873  
#>  Min.   :14   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:14   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :14   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :14   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:14   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :14   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4874       X4875       X4876       X4877       X4878       X4879  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4880       X4881       X4882       X4883       X4884       X4885  
#>  Min.   :5   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:5   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :5   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :5   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:5   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :5   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4886       X4887       X4888       X4889       X4890       X4891  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4892       X4893       X4894        X4895       X4896       X4897  
#>  Min.   :2   Min.   :3   Min.   :10   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:2   1st Qu.:3   1st Qu.:10   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :2   Median :3   Median :10   Median :0   Median :0   Median :0  
#>  Mean   :2   Mean   :3   Mean   :10   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:2   3rd Qu.:3   3rd Qu.:10   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :2   Max.   :3   Max.   :10   Max.   :0   Max.   :0   Max.   :0  
#>      X4898       X4899       X5001       X5002       X5003       X5004  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :3  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :3  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :3  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :3  
#>      X5005        X5006       X5007       X5008       X5009       X5010  
#>  Min.   :20   Min.   :0   Min.   :0   Min.   :3   Min.   :0   Min.   :0  
#>  1st Qu.:20   1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0  
#>  Median :20   Median :0   Median :0   Median :3   Median :0   Median :0  
#>  Mean   :20   Mean   :0   Mean   :0   Mean   :3   Mean   :0   Mean   :0  
#>  3rd Qu.:20   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :20   Max.   :0   Max.   :0   Max.   :3   Max.   :0   Max.   :0  
#>      X5011       X5012       X5013       X5014       X5018       X5019  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :8   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:8   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :8   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :8   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:8   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :8   Max.   :0  
#>      X5020       X5021       X5022       X5023       X5024       X5025  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5026       X5027       X5028       X5029        X5030       X5031  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :12   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:12   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :12   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :12   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:12   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :12   Max.   :0   Max.   :0  
#>      X5032       X5033       X5034       X5035       X5036       X5037  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5038       X5039       X5040       X5041       X5042       X5043  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5044       X5045       X5046       X5047       X5048       X5049  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5050        X5051       X5052       X5053       X5054       X5055  
#>  Min.   :21   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:21   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :21   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :21   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:21   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :21   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5056        X5061       X5062       X5063       X5064       X5065  
#>  Min.   :10   Min.   :0   Min.   :0   Min.   :0   Min.   :5   Min.   :0  
#>  1st Qu.:10   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:5   1st Qu.:0  
#>  Median :10   Median :0   Median :0   Median :0   Median :5   Median :0  
#>  Mean   :10   Mean   :0   Mean   :0   Mean   :0   Mean   :5   Mean   :0  
#>  3rd Qu.:10   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:5   3rd Qu.:0  
#>  Max.   :10   Max.   :0   Max.   :0   Max.   :0   Max.   :5   Max.   :0  
#>      X5066       X5070        X5071        X5072       X5073       X5074   
#>  Min.   :0   Min.   :10   Min.   :11   Min.   :4   Min.   :0   Min.   :16  
#>  1st Qu.:0   1st Qu.:10   1st Qu.:11   1st Qu.:4   1st Qu.:0   1st Qu.:16  
#>  Median :0   Median :10   Median :11   Median :4   Median :0   Median :16  
#>  Mean   :0   Mean   :10   Mean   :11   Mean   :4   Mean   :0   Mean   :16  
#>  3rd Qu.:0   3rd Qu.:10   3rd Qu.:11   3rd Qu.:4   3rd Qu.:0   3rd Qu.:16  
#>  Max.   :0   Max.   :10   Max.   :11   Max.   :4   Max.   :0   Max.   :16  
#>      X5075       X5076       X5077       X5078       X5079       X5081  
#>  Min.   :4   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:4   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :4   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :4   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:4   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :4   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5083       X5084       X5085       X5086       X5087       X5088  
#>  Min.   :0   Min.   :1   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:1   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :1   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :1   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:1   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :1   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5089       X5090       X5091       X5092       X5093       X5094  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :2   Min.   :2   Min.   :7  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:2   1st Qu.:2   1st Qu.:7  
#>  Median :0   Median :0   Median :0   Median :2   Median :2   Median :7  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :2   Mean   :2   Mean   :7  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:2   3rd Qu.:2   3rd Qu.:7  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :2   Max.   :2   Max.   :7  
#>      X5095       X5096       X5097       X5098       X5099       X5100  
#>  Min.   :0   Min.   :0   Min.   :1   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:1   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :1   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :1   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:1   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :1   Max.   :0   Max.   :0   Max.   :0  
#>      X5101       X5102       X5103       X5104       X5105       X5106  
#>  Min.   :0   Min.   :0   Min.   :9   Min.   :0   Min.   :0   Min.   :1  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:9   1st Qu.:0   1st Qu.:0   1st Qu.:1  
#>  Median :0   Median :0   Median :9   Median :0   Median :0   Median :1  
#>  Mean   :0   Mean   :0   Mean   :9   Mean   :0   Mean   :0   Mean   :1  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:9   3rd Qu.:0   3rd Qu.:0   3rd Qu.:1  
#>  Max.   :0   Max.   :0   Max.   :9   Max.   :0   Max.   :0   Max.   :1  
#>      X5108       X5109       X5110       X5111       X5112       X5116  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5117       X5118       X5119       X5120       X5121       X5122  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :2   Min.   :6  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:2   1st Qu.:6  
#>  Median :0   Median :0   Median :0   Median :0   Median :2   Median :6  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :2   Mean   :6  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:2   3rd Qu.:6  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :2   Max.   :6  
#>      X5123       X5124       X5125       X5126       X5127       X5128  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5129       X5130       X5131       X5132       X5133       X5134  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5135       X5136       X5137       X5138       X5200       X5201  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :4   Min.   :4   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:4   1st Qu.:4   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :4   Median :4   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :4   Mean   :4   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:4   3rd Qu.:4   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :4   Max.   :4   Max.   :0  
#>      X5202       X5203       X5205       X5206       X5207       X5208  
#>  Min.   :0   Min.   :0   Min.   :4   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:4   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :4   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :4   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:4   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :4   Max.   :0   Max.   :0   Max.   :0  
#>      X5209       X5210        X5211       X5212        X5213       X5214  
#>  Min.   :0   Min.   :22   Min.   :3   Min.   :10   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:22   1st Qu.:3   1st Qu.:10   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :22   Median :3   Median :10   Median :0   Median :0  
#>  Mean   :0   Mean   :22   Mean   :3   Mean   :10   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:22   3rd Qu.:3   3rd Qu.:10   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :22   Max.   :3   Max.   :10   Max.   :0   Max.   :0  
#>      X5215       X5216       X5217       X5218       X5219       X5220  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5221       X5222       X5223       X5224       X5225       X5226   
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :13  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:13  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :13  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :13  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:13  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :13  
#>      X5227       X5228       X5229       X5230       X5231       X5232   
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :20  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:20  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :20  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :20  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:20  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :20  
#>      X5233       X5234       X5235       X5236       X5237       X5238  
#>  Min.   :0   Min.   :1   Min.   :4   Min.   :8   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:1   1st Qu.:4   1st Qu.:8   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :1   Median :4   Median :8   Median :0   Median :0  
#>  Mean   :0   Mean   :1   Mean   :4   Mean   :8   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:1   3rd Qu.:4   3rd Qu.:8   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :1   Max.   :4   Max.   :8   Max.   :0   Max.   :0  
#>      X5239       X5240       X5241       X5242       X5243       X5244  
#>  Min.   :0   Min.   :0   Min.   :1   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:1   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :1   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :1   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:1   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :1   Max.   :0   Max.   :0   Max.   :0  
#>      X5245       X5246       X5247        X5248       X5249       X5250  
#>  Min.   :1   Min.   :0   Min.   :12   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:1   1st Qu.:0   1st Qu.:12   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :1   Median :0   Median :12   Median :0   Median :0   Median :0  
#>  Mean   :1   Mean   :0   Mean   :12   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:1   3rd Qu.:0   3rd Qu.:12   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :1   Max.   :0   Max.   :12   Max.   :0   Max.   :0   Max.   :0  
#>      X5251       X5252       X5253       X5254       X5255       X5256   
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :15  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:15  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :15  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :15  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:15  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :15  
#>      X5257       X5258       X5259       X5260       X5261       X5262  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5263       X5264       X5265       X5266       X5268       X5269  
#>  Min.   :0   Min.   :0   Min.   :7   Min.   :2   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:7   1st Qu.:2   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :7   Median :2   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :7   Mean   :2   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:7   3rd Qu.:2   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :7   Max.   :2   Max.   :0   Max.   :0  
#>      X5270       X5271       X5272       X5273       X5274       X5275  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :1  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:1  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :1  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :1  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:1  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :1  
#>      X5276       X5277       X5278       X5279       X5280       X5281  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :2   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:2   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :2   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :2   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:2   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :2   Max.   :0   Max.   :0  
#>      X5282       X5283       X5284       X5285       X5286       X5288  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5289       X5290       X5291       X5292       X5293       X5294  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5295       X5296       X5297       X5298       X5299       X5300  
#>  Min.   :0   Min.   :0   Min.   :1   Min.   :4   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:1   1st Qu.:4   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :1   Median :4   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :1   Mean   :4   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:1   3rd Qu.:4   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :1   Max.   :4   Max.   :0   Max.   :0  
#>      X5301       X5302       X5303       X5304       X5305       X5306  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :9   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:9   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :9   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :9   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:9   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :9   Max.   :0   Max.   :0  
#>      X5307       X5309       X5310       X5311       X5312       X5313  
#>  Min.   :9   Min.   :0   Min.   :5   Min.   :0   Min.   :2   Min.   :0  
#>  1st Qu.:9   1st Qu.:0   1st Qu.:5   1st Qu.:0   1st Qu.:2   1st Qu.:0  
#>  Median :9   Median :0   Median :5   Median :0   Median :2   Median :0  
#>  Mean   :9   Mean   :0   Mean   :5   Mean   :0   Mean   :2   Mean   :0  
#>  3rd Qu.:9   3rd Qu.:0   3rd Qu.:5   3rd Qu.:0   3rd Qu.:2   3rd Qu.:0  
#>  Max.   :9   Max.   :0   Max.   :5   Max.   :0   Max.   :2   Max.   :0  
#>      X5314       X5315       X5316       X5317       X5318        X5319  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :13   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:13   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :13   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :13   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:13   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :13   Max.   :0  
#>      X5320       X5321       X5322       X5323       X5324        X5325  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :12   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:12   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :12   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :12   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:12   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :12   Max.   :0  
#>      X5326       X5327       X5328       X5329       X5330       X5331  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :5   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:5   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :5   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :5   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:5   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :5   Max.   :0  
#>      X5332       X5333       X5334       X5335       X5336       X5337  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :9  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:9  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :9  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :9  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:9  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :9  
#>      X5338       X5339       X5340       X5341       X5342       X5343  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :6   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:6   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :6   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :6   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:6   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :6   Max.   :0  
#>      X5344       X5345       X5346       X5347       X5348       X5349  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :5   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:5   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :5   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :5   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:5   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :5   Max.   :0   Max.   :0  
#>      X5350       X5351       X5352       X5353       X5354       X5355  
#>  Min.   :3   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :3   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :3   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :3   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5356       X5357       X5358       X5359       X5363       X5364  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5365       X5366       X5367        X5368       X5369       X5370  
#>  Min.   :0   Min.   :0   Min.   :27   Min.   :7   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:27   1st Qu.:7   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :27   Median :7   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :27   Mean   :7   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:27   3rd Qu.:7   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :27   Max.   :7   Max.   :0   Max.   :0  
#>      X5371       X5372       X5373       X5374       X5375       X5376  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :3   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :3   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :3   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :3   Max.   :0  
#>      X5377       X5378       X5379       X5380       X5381       X5382  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :7   Min.   :0   Min.   :7  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:7   1st Qu.:0   1st Qu.:7  
#>  Median :0   Median :0   Median :0   Median :7   Median :0   Median :7  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :7   Mean   :0   Mean   :7  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:7   3rd Qu.:0   3rd Qu.:7  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :7   Max.   :0   Max.   :7  
#>      X5383       X5384       X5385       X5386       X5387       X5388  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :3  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :3  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :3  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :3  
#>      X5389       X5390       X5391       X5392       X5393       X5394  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5395       X5396       X5397       X5398       X5399       X5400  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5401       X5402       X5403       X5404       X5405       X5406  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5407       X5408       X5409       X5410       X5411       X5412  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5413       X5414       X5415       X5416       X5417       X5418  
#>  Min.   :0   Min.   :9   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:9   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :9   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :9   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:9   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :9   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5419       X5420       X5422       X5423       X5424       X5425  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :3  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :3  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :3  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :3  
#>      X5426       X5427       X5428       X5429       X5430       X5431  
#>  Min.   :7   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:7   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :7   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :7   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:7   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :7   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5432       X5433       X5434       X5435       X5436       X5437  
#>  Min.   :1   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:1   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :1   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :1   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:1   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :1   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5438       X5439       X5440       X5441       X5442       X5443  
#>  Min.   :0   Min.   :6   Min.   :3   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:6   1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :6   Median :3   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :6   Mean   :3   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:6   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :6   Max.   :3   Max.   :0   Max.   :0   Max.   :0  
#>      X5444       X5445       X5446       X5447       X5448       X5449  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :5   Min.   :0   Min.   :9  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:5   1st Qu.:0   1st Qu.:9  
#>  Median :0   Median :0   Median :0   Median :5   Median :0   Median :9  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :5   Mean   :0   Mean   :9  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:5   3rd Qu.:0   3rd Qu.:9  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :5   Max.   :0   Max.   :9  
#>      X5450       X5451       X5452       X5453       X5454       X5455  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5460       X5461       X5462       X5463       X5464       X5501  
#>  Min.   :4   Min.   :2   Min.   :1   Min.   :0   Min.   :0   Min.   :6  
#>  1st Qu.:4   1st Qu.:2   1st Qu.:1   1st Qu.:0   1st Qu.:0   1st Qu.:6  
#>  Median :4   Median :2   Median :1   Median :0   Median :0   Median :6  
#>  Mean   :4   Mean   :2   Mean   :1   Mean   :0   Mean   :0   Mean   :6  
#>  3rd Qu.:4   3rd Qu.:2   3rd Qu.:1   3rd Qu.:0   3rd Qu.:0   3rd Qu.:6  
#>  Max.   :4   Max.   :2   Max.   :1   Max.   :0   Max.   :0   Max.   :6  
#>      X5513       X5609       X5610       X5611       X5612       X5613  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5614       X5615       X5616       X5617       X5618       X5619  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5620       X5621       X5622       X5623       X5624       X5625  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5626       X5627       X5628       X5629       X5630       X5631  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :4  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:4  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :4  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :4  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:4  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :4  
#>      X5632       X5633       X5634       X5635       X5636       X5637  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5638       X5639       X5640       X5641       X5642       X5643  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5644       X5645       X5646       X5647       X5648       X5649  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5650       X5651       X5652       X5653       X5654       X5655  
#>  Min.   :0   Min.   :0   Min.   :7   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:7   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :7   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :7   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:7   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :7   Max.   :0   Max.   :0   Max.   :0  
#>      X5657       X5658       X5659       X5660       X5661       X5662  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :1   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:1   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :1   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :1   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:1   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :1   Max.   :0  
#>      X5663       X5664       X5665       X5666       X5667       X5668  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5669       X5670       X5671       X5672       X5673       X5674  
#>  Min.   :0   Min.   :4   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:4   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :4   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :4   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:4   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :4   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5675       X5676       X5677       X5678       X5679       X5680  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5681       X5682       X5683       X5684       X5685       X5686  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5687       X5688       X5689       X5690       X5691       X5693   
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :16  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:16  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :16  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :16  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:16  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :16  
#>      X5694       X5695       X5696       X5697       X5698       X5699  
#>  Min.   :0   Min.   :0   Min.   :1   Min.   :0   Min.   :0   Min.   :4  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:1   1st Qu.:0   1st Qu.:0   1st Qu.:4  
#>  Median :0   Median :0   Median :1   Median :0   Median :0   Median :4  
#>  Mean   :0   Mean   :0   Mean   :1   Mean   :0   Mean   :0   Mean   :4  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:1   3rd Qu.:0   3rd Qu.:0   3rd Qu.:4  
#>  Max.   :0   Max.   :0   Max.   :1   Max.   :0   Max.   :0   Max.   :4  
#>      X5700        X5701       X5702       X5703       X5704       X5705  
#>  Min.   :16   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:16   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :16   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :16   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:16   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :16   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5706       X5707       X5708       X5709       X5710       X5711  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5712       X5713       X5714       X5728       X5729       X5730  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5731       X5732       X5733       X5734       X5735       X5736  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5737       X5738       X5739       X5740       X5741       X5742  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5743       X5744       X5745       X5746       X5747       X5748  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5749       X5750       X5751       X5752       X5753       X5754  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5755       X5756       X5757       X5758       X5759       X5760  
#>  Min.   :0   Min.   :0   Min.   :7   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:7   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :7   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :7   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:7   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :7   Max.   :0   Max.   :0   Max.   :0  
#>      X5761       X5762       X5763       X5764       X5765       X5766  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :2   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:2   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :2   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :2   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:2   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :2   Max.   :0  
#>      X5767       X5768       X5769       X5770       X5771       X5772  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5773       X5774       X5775       X5776       X5777       X5778  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5779       X5780       X5781       X5782       X5783       X5784  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5785       X5786       X5787       X5788       X5789       X5790  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :3   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :3   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :3   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :3   Max.   :0  
#>      X5791       X5792       X5793       X5794       X5795       X5796  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5797       X5798       X5799       X5800       X5801       X5802  
#>  Min.   :8   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:8   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :8   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :8   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:8   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :8   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5803       X5804       X5805       X5806        X5807       X5808  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :10   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:10   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :10   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :10   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:10   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :10   Max.   :0   Max.   :0  
#>      X5809       X5810       X5811       X5812       X5813       X5814  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5815       X5816       X5845       X5901       X5902       X5903  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5904       X5905       X5906       X5907       X5908       X5909   
#>  Min.   :0   Min.   :0   Min.   :3   Min.   :5   Min.   :6   Min.   :22  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:5   1st Qu.:6   1st Qu.:22  
#>  Median :0   Median :0   Median :3   Median :5   Median :6   Median :22  
#>  Mean   :0   Mean   :0   Mean   :3   Mean   :5   Mean   :6   Mean   :22  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:5   3rd Qu.:6   3rd Qu.:22  
#>  Max.   :0   Max.   :0   Max.   :3   Max.   :5   Max.   :6   Max.   :22  
#>      X5910        X5911       X5912       X5913       X5914       X5915  
#>  Min.   :13   Min.   :3   Min.   :0   Min.   :0   Min.   :1   Min.   :0  
#>  1st Qu.:13   1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:1   1st Qu.:0  
#>  Median :13   Median :3   Median :0   Median :0   Median :1   Median :0  
#>  Mean   :13   Mean   :3   Mean   :0   Mean   :0   Mean   :1   Mean   :0  
#>  3rd Qu.:13   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:1   3rd Qu.:0  
#>  Max.   :13   Max.   :3   Max.   :0   Max.   :0   Max.   :1   Max.   :0  
#>      X5916       X5917       X5918       X5919       X5920       X5921  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5922       X5923       X5924       X5925       X5926       X5927   
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :13  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:13  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :13  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :13  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:13  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :13  
#>      X5928       X5929       X5930       X5931       X5932       X5933  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :3   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :3   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :3   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :3   Max.   :0   Max.   :0  
#>      X5934       X5935       X5936       X5937       X5938       X5939  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5940       X5941       X5942       X5943       X5944       X5945  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :3  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :3  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :3  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :3  
#>      X5946       X5947       X5948       X5949       X5950       X5951  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5952       X5953       X5954        X5955       X5956       X5957  
#>  Min.   :0   Min.   :0   Min.   :19   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:19   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :19   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :19   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:19   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :19   Max.   :0   Max.   :0   Max.   :0  
#>      X5958       X5959       X5960       X5961       X5962       X5963  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5964       X5965       X5966       X5967       X5968       X5969  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5970       X5971       X5972       X5973       X5974       X5975  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5976       X5977       X5978       X5979       X5980       X5981  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5982       X5983        X5984       X5985       X5986       X5987  
#>  Min.   :4   Min.   :19   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:4   1st Qu.:19   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :4   Median :19   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :4   Mean   :19   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:4   3rd Qu.:19   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :4   Max.   :19   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5988       X5989       X5990       X5991       X5992       X5993  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X5994       X5995       X5996       X5997       X5998       X5999  
#>  Min.   :0   Min.   :0   Min.   :1   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:1   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :1   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :1   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:1   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :1   Max.   :0   Max.   :0   Max.   :0  
#>      X6000       X6001       X6002       X6003       X6004       X6005  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :1   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:1   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :1   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :1   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:1   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :1   Max.   :0  
#>      X6006       X6007       X6008       X6009       X6010       X6011  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6012       X6013       X6014       X6015       X6016       X6017  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6018       X6019       X6020       X6021       X6022       X6023  
#>  Min.   :3   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :3   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :3   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :3   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6024       X6025       X6026       X6027       X6028       X6029  
#>  Min.   :0   Min.   :0   Min.   :6   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:6   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :6   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :6   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:6   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :6   Max.   :0   Max.   :0   Max.   :0  
#>      X6030       X6031       X6032       X6033       X6034        X6035  
#>  Min.   :0   Min.   :1   Min.   :0   Min.   :0   Min.   :18   Min.   :0  
#>  1st Qu.:0   1st Qu.:1   1st Qu.:0   1st Qu.:0   1st Qu.:18   1st Qu.:0  
#>  Median :0   Median :1   Median :0   Median :0   Median :18   Median :0  
#>  Mean   :0   Mean   :1   Mean   :0   Mean   :0   Mean   :18   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:1   3rd Qu.:0   3rd Qu.:0   3rd Qu.:18   3rd Qu.:0  
#>  Max.   :0   Max.   :1   Max.   :0   Max.   :0   Max.   :18   Max.   :0  
#>      X6036       X6037       X6038       X6039       X6040        X6041  
#>  Min.   :0   Min.   :4   Min.   :2   Min.   :0   Min.   :26   Min.   :0  
#>  1st Qu.:0   1st Qu.:4   1st Qu.:2   1st Qu.:0   1st Qu.:26   1st Qu.:0  
#>  Median :0   Median :4   Median :2   Median :0   Median :26   Median :0  
#>  Mean   :0   Mean   :4   Mean   :2   Mean   :0   Mean   :26   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:4   3rd Qu.:2   3rd Qu.:0   3rd Qu.:26   3rd Qu.:0  
#>  Max.   :0   Max.   :4   Max.   :2   Max.   :0   Max.   :26   Max.   :0  
#>      X6042       X6043       X6044       X6045       X6046       X6047  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :6  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:6  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :6  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :6  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:6  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :6  
#>      X6048       X6049       X6050       X6051       X6052       X6053  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :3   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :3   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :3   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :3   Max.   :0   Max.   :0  
#>      X6054       X6055       X6056       X6057        X6058       X6059  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :16   Min.   :3   Min.   :2  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:16   1st Qu.:3   1st Qu.:2  
#>  Median :0   Median :0   Median :0   Median :16   Median :3   Median :2  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :16   Mean   :3   Mean   :2  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:16   3rd Qu.:3   3rd Qu.:2  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :16   Max.   :3   Max.   :2  
#>      X6060       X6061       X6062       X6063       X6064       X6065  
#>  Min.   :2   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:2   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :2   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :2   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:2   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :2   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6066       X6067       X6068       X6069       X6070       X6071  
#>  Min.   :5   Min.   :0   Min.   :0   Min.   :1   Min.   :0   Min.   :0  
#>  1st Qu.:5   1st Qu.:0   1st Qu.:0   1st Qu.:1   1st Qu.:0   1st Qu.:0  
#>  Median :5   Median :0   Median :0   Median :1   Median :0   Median :0  
#>  Mean   :5   Mean   :0   Mean   :0   Mean   :1   Mean   :0   Mean   :0  
#>  3rd Qu.:5   3rd Qu.:0   3rd Qu.:0   3rd Qu.:1   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :5   Max.   :0   Max.   :0   Max.   :1   Max.   :0   Max.   :0  
#>      X6072       X6073       X6074       X6075       X6076       X6077  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :3   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :3   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :3   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :3   Max.   :0   Max.   :0  
#>      X6078       X6079       X6080       X6081       X6082       X6083  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6084       X6085       X6086       X6087       X6088       X6089  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6200       X6201       X6203        X6204       X6205       X6206  
#>  Min.   :0   Min.   :4   Min.   :10   Min.   :1   Min.   :2   Min.   :0  
#>  1st Qu.:0   1st Qu.:4   1st Qu.:10   1st Qu.:1   1st Qu.:2   1st Qu.:0  
#>  Median :0   Median :4   Median :10   Median :1   Median :2   Median :0  
#>  Mean   :0   Mean   :4   Mean   :10   Mean   :1   Mean   :2   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:4   3rd Qu.:10   3rd Qu.:1   3rd Qu.:2   3rd Qu.:0  
#>  Max.   :0   Max.   :4   Max.   :10   Max.   :1   Max.   :2   Max.   :0  
#>      X6211       X6212       X6213       X6214       X6215       X6216  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6217       X6218       X6219       X6220       X6221       X6222  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :4   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:4   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :4   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :4   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:4   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :4   Max.   :0  
#>      X6223       X6224       X6225       X6226       X6227       X6228  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6229       X6230       X6231       X6232       X6233       X6234  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6235       X6236       X6237       X6238       X6239       X6240  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6241       X6242       X6243       X6244       X6245       X6246  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6247       X6248       X6249       X6250       X6251       X6252  
#>  Min.   :3   Min.   :8   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:3   1st Qu.:8   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :3   Median :8   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :3   Mean   :8   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:3   3rd Qu.:8   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :3   Max.   :8   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6253       X6254       X6255       X6256       X6257       X6258  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6259       X6260       X6261       X6262       X6263       X6264  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6265       X6266       X6267       X6268       X6269       X6270  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6271       X6272       X6273       X6274       X6275       X6276  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6277       X6278       X6279       X6280       X6281       X6282  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6283       X6284       X6285       X6286       X6287       X6288  
#>  Min.   :0   Min.   :3   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :3   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :3   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :3   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6289       X6290       X6291       X6292       X6293       X6294  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6295       X6296       X6297       X6298       X6299       X6300  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6301       X6302       X6303       X6304       X6305       X6306  
#>  Min.   :3   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :3   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :3   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :3   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6307       X6308       X6309       X6310       X6311       X6312  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6313       X6314       X6315       X6316       X6317       X6318  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6319       X6320       X6321       X6322       X6323       X6324  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6325       X6326       X6327       X6328       X6329       X6330  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6331       X6332       X6333       X6334       X6335       X6336  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6337       X6338       X6339       X6340       X6341       X6342  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6343       X6344       X6345       X6346       X6347       X6348  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6349       X6350       X6351       X6352       X6353       X6354  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6355       X6356       X6357       X6358       X6359       X6360  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6361       X6362       X6363       X6364       X6365       X6366  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6367       X6368       X6369       X6370       X6371       X6372  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6373       X6374       X6375       X6376       X6377       X6378  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6380       X6417       X6418       X6419       X6420       X6421  
#>  Min.   :1   Min.   :3   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:1   1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :1   Median :3   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :1   Mean   :3   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:1   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :1   Max.   :3   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6422       X6423       X6424       X6425       X6426       X6427  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6428       X6429       X6430       X6431       X6432       X6433  
#>  Min.   :0   Min.   :0   Min.   :4   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:4   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :4   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :4   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:4   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :4   Max.   :0   Max.   :0   Max.   :0  
#>      X6434       X6435        X6436       X6437       X6438       X6439  
#>  Min.   :0   Min.   :10   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:10   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :10   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :10   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:10   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :10   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6440       X6441       X6442       X6443       X6444       X6445  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :2   Min.   :5  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:2   1st Qu.:5  
#>  Median :0   Median :0   Median :0   Median :0   Median :2   Median :5  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :2   Mean   :5  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:2   3rd Qu.:5  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :2   Max.   :5  
#>      X6446       X6447       X6448       X6449       X6450       X6451  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :8   Min.   :9  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:8   1st Qu.:9  
#>  Median :0   Median :0   Median :0   Median :0   Median :8   Median :9  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :8   Mean   :9  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:8   3rd Qu.:9  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :8   Max.   :9  
#>      X6452       X6453       X6454       X6455       X6456       X6457  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6458       X6459       X6460       X6461       X6462       X6463  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6464       X6465        X6466       X6467       X6468       X6469  
#>  Min.   :0   Min.   :10   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:10   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :10   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :10   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:10   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :10   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6470       X6471       X6472       X6473       X6474       X6475  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6476       X6477       X6478       X6479       X6480       X6481  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6482       X6483       X6484       X6485       X6486       X6487   
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :15  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:15  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :15  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :15  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:15  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :15  
#>      X6488       X6489       X6490       X6491       X6492       X6493  
#>  Min.   :0   Min.   :0   Min.   :4   Min.   :2   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:4   1st Qu.:2   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :4   Median :2   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :4   Mean   :2   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:4   3rd Qu.:2   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :4   Max.   :2   Max.   :0   Max.   :0  
#>      X6494       X6495       X6496       X6497       X6498       X6499  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :1  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:1  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :1  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :1  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:1  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :1  
#>      X6500       X6501       X6502       X6516       X6517       X6518  
#>  Min.   :3   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :3   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :3   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :3   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6519       X6520       X6521       X6522       X6523       X6524  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6525       X6526       X6527       X6528       X6529       X6530  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6531       X6532       X6533       X6534       X6535       X6536  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6537       X6538       X6539       X6540       X6541       X6542  
#>  Min.   :0   Min.   :2   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:2   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :2   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :2   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:2   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :2   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6543       X6544       X6545       X6546       X6547       X6548  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6549       X6550       X6551       X6552       X6553       X6554  
#>  Min.   :9   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:9   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :9   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :9   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:9   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :9   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X6555       X6559       X6560       X8235       X8236       X8237  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8238       X8240       X8241       X8242       X8243       X8246  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8247       X8248       X8249       X8250       X8251       X8252  
#>  Min.   :3   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :3   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :3   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :3   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8253       X8254       X8255       X8256       X8257       X8258  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8261       X8262       X8263       X8264       X8265       X8266  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8267       X8268       X8269       X8271       X8272       X8273  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8274       X8275       X8276       X8277       X8278       X8279  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8280       X8281       X8283       X8284       X8285       X8287  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :2  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:2  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :2  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :2  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:2  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :2  
#>      X8289       X8291       X8292       X8293       X8295       X8296  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8297       X8298       X8299       X8300       X8301       X8305  
#>  Min.   :0   Min.   :3   Min.   :0   Min.   :4   Min.   :5   Min.   :0  
#>  1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:4   1st Qu.:5   1st Qu.:0  
#>  Median :0   Median :3   Median :0   Median :4   Median :5   Median :0  
#>  Mean   :0   Mean   :3   Mean   :0   Mean   :4   Mean   :5   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:4   3rd Qu.:5   3rd Qu.:0  
#>  Max.   :0   Max.   :3   Max.   :0   Max.   :4   Max.   :5   Max.   :0  
#>      X8306       X8307       X8308       X8309       X8310       X8311  
#>  Min.   :0   Min.   :2   Min.   :6   Min.   :0   Min.   :0   Min.   :1  
#>  1st Qu.:0   1st Qu.:2   1st Qu.:6   1st Qu.:0   1st Qu.:0   1st Qu.:1  
#>  Median :0   Median :2   Median :6   Median :0   Median :0   Median :1  
#>  Mean   :0   Mean   :2   Mean   :6   Mean   :0   Mean   :0   Mean   :1  
#>  3rd Qu.:0   3rd Qu.:2   3rd Qu.:6   3rd Qu.:0   3rd Qu.:0   3rd Qu.:1  
#>  Max.   :0   Max.   :2   Max.   :6   Max.   :0   Max.   :0   Max.   :1  
#>      X8312       X8313       X8314       X8315       X8316       X8317  
#>  Min.   :1   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:1   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :1   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :1   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:1   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :1   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8318       X8319       X8323       X8324       X8325       X8326  
#>  Min.   :4   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :3  
#>  1st Qu.:4   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3  
#>  Median :4   Median :0   Median :0   Median :0   Median :0   Median :3  
#>  Mean   :4   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :3  
#>  3rd Qu.:4   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3  
#>  Max.   :4   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :3  
#>      X8327       X8329       X8331       X8333       X8336       X8337  
#>  Min.   :5   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:5   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :5   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :5   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:5   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :5   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8338       X8339       X8340       X8342       X8345       X8346  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :3   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :3   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :3   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :3   Max.   :0  
#>      X8347       X8348       X8349       X8350       X8351       X8352  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8354       X8355       X8356       X8357       X8358       X8359  
#>  Min.   :0   Min.   :3   Min.   :2   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:3   1st Qu.:2   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :3   Median :2   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :3   Mean   :2   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:3   3rd Qu.:2   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :3   Max.   :2   Max.   :0   Max.   :0   Max.   :0  
#>      X8360       X8361       X8362       X8363       X8364       X8365  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :3   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :3   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :3   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :3   Max.   :0   Max.   :0  
#>      X8366       X8367       X8368       X8369       X8372       X8374  
#>  Min.   :0   Min.   :3   Min.   :3   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:3   1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :3   Median :3   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :3   Mean   :3   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:3   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :3   Max.   :3   Max.   :0   Max.   :0   Max.   :0  
#>      X8376       X8377       X8378       X8380       X8381       X8382  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8383       X8384       X8385       X8386       X8387       X8388  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8421       X8422       X8423       X8424       X8425       X8426  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8427       X8428       X8429       X8430       X8431       X8432  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8433       X8434       X8437       X8438       X8439       X8440  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8441       X8442       X8443       X8444       X8445       X8446  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8447       X8448       X8450       X8451       X8452       X8453  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8454       X8455       X8456       X8457       X8458       X8459  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8460       X8461       X8462       X8463       X8464       X8465  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8467       X8468       X8469       X8470       X8471       X8474  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :4  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:4  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :4  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :4  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:4  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :4  
#>      X8476       X8477       X8478       X8479       X8480       X8481  
#>  Min.   :0   Min.   :0   Min.   :2   Min.   :5   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:2   1st Qu.:5   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :2   Median :5   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :2   Mean   :5   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:2   3rd Qu.:5   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :2   Max.   :5   Max.   :0   Max.   :0  
#>      X8482       X8483       X8484       X8485       X8486       X8490  
#>  Min.   :0   Min.   :0   Min.   :3   Min.   :0   Min.   :4   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:4   1st Qu.:0  
#>  Median :0   Median :0   Median :3   Median :0   Median :4   Median :0  
#>  Mean   :0   Mean   :0   Mean   :3   Mean   :0   Mean   :4   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:4   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :3   Max.   :0   Max.   :4   Max.   :0  
#>      X8491       X8499       X8500       X8501       X8502       X8503  
#>  Min.   :0   Min.   :0   Min.   :3   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :3   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :3   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :3   Max.   :0   Max.   :0   Max.   :0  
#>      X8504       X8505       X8507       X8509       X8510       X8511  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :1  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:1  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :1  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :1  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:1  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :1  
#>      X8512       X8513       X8514       X8515       X8516       X8517  
#>  Min.   :0   Min.   :1   Min.   :0   Min.   :1   Min.   :4   Min.   :0  
#>  1st Qu.:0   1st Qu.:1   1st Qu.:0   1st Qu.:1   1st Qu.:4   1st Qu.:0  
#>  Median :0   Median :1   Median :0   Median :1   Median :4   Median :0  
#>  Mean   :0   Mean   :1   Mean   :0   Mean   :1   Mean   :4   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:1   3rd Qu.:0   3rd Qu.:1   3rd Qu.:4   3rd Qu.:0  
#>  Max.   :0   Max.   :1   Max.   :0   Max.   :1   Max.   :4   Max.   :0  
#>      X8518       X8519       X8520       X8521       X8522       X8523  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8524       X8525       X8526       X8527       X8528       X8529  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8530       X8531       X8532       X8533       X8534       X8535  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X8536       X8537       X8538       X8539       X8540       X8541  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#> 
#> $ped3
#>       X368        X1086       X1087       X1089       X1091       X1453  
#>  Min.   :14   Min.   :0   Min.   :3   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:14   1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :14   Median :0   Median :3   Median :0   Median :0   Median :0  
#>  Mean   :14   Mean   :0   Mean   :3   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:14   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :14   Max.   :0   Max.   :3   Max.   :0   Max.   :0   Max.   :0  
#>      X1454       X1456       X2034       X2037        X2629       X3158  
#>  Min.   :4   Min.   :0   Min.   :5   Min.   :12   Min.   :0   Min.   :0  
#>  1st Qu.:4   1st Qu.:0   1st Qu.:5   1st Qu.:12   1st Qu.:0   1st Qu.:0  
#>  Median :4   Median :0   Median :5   Median :12   Median :0   Median :0  
#>  Mean   :4   Mean   :0   Mean   :5   Mean   :12   Mean   :0   Mean   :0  
#>  3rd Qu.:4   3rd Qu.:0   3rd Qu.:5   3rd Qu.:12   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :4   Max.   :0   Max.   :5   Max.   :12   Max.   :0   Max.   :0  
#>      X3656       X4330  
#>  Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0  
#>  Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0  
#> 
#> $ped4
#>       X197        X339        X340         X836       X1076       X1077  
#>  Min.   :0   Min.   :0   Min.   :18   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:18   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :18   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :18   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:18   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :18   Max.   :0   Max.   :0   Max.   :0  
#>      X1080       X1081       X1447       X1451  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#> 
#> $ped5
#>       X492        X493        X769       X1412       X1656       X1657  
#>  Min.   :0   Min.   :0   Min.   :4   Min.   :3   Min.   :4   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:4   1st Qu.:3   1st Qu.:4   1st Qu.:0  
#>  Median :0   Median :0   Median :4   Median :3   Median :4   Median :0  
#>  Mean   :0   Mean   :0   Mean   :4   Mean   :3   Mean   :4   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:4   3rd Qu.:3   3rd Qu.:4   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :4   Max.   :3   Max.   :4   Max.   :0  
#>      X1658       X1986       X1987       X3741       X4426  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#> 
#> $ped6
#>       X374        X513        X514        X672        X801        X803   
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :2   Min.   :26  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:2   1st Qu.:26  
#>  Median :0   Median :0   Median :0   Median :0   Median :2   Median :26  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :2   Mean   :26  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:2   3rd Qu.:26  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :2   Max.   :26  
#>      X1057       X1059       X1390       X1391       X1392       X1400  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X1616       X1618       X1919       X1920       X1921       X1922  
#>  Min.   :0   Min.   :3   Min.   :0   Min.   :0   Min.   :0   Min.   :9  
#>  1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:9  
#>  Median :0   Median :3   Median :0   Median :0   Median :0   Median :9  
#>  Mean   :0   Mean   :3   Mean   :0   Mean   :0   Mean   :0   Mean   :9  
#>  3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:9  
#>  Max.   :0   Max.   :3   Max.   :0   Max.   :0   Max.   :0   Max.   :9  
#>      X1923        X2022       X2024       X2178       X2180       X2181  
#>  Min.   :10   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:10   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :10   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :10   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:10   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :10   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2340       X2341       X2606       X2608       X3018       X3019  
#>  Min.   :0   Min.   :0   Min.   :7   Min.   :0   Min.   :3   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:7   1st Qu.:0   1st Qu.:3   1st Qu.:0  
#>  Median :0   Median :0   Median :7   Median :0   Median :3   Median :0  
#>  Mean   :0   Mean   :0   Mean   :7   Mean   :0   Mean   :3   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:7   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :7   Max.   :0   Max.   :3   Max.   :0  
#>      X3055       X3126       X3128       X3387       X3388       X3390  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3415       X3416       X3417       X3543       X3571       X3856  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3857       X4252       X4253       X4254  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#> 
#> $ped7
#>       X519        X520        X829        X830        X842       X1096  
#>  Min.   :4   Min.   :3   Min.   :0   Min.   :1   Min.   :0   Min.   :0  
#>  1st Qu.:4   1st Qu.:3   1st Qu.:0   1st Qu.:1   1st Qu.:0   1st Qu.:0  
#>  Median :4   Median :3   Median :0   Median :1   Median :0   Median :0  
#>  Mean   :4   Mean   :3   Mean   :0   Mean   :1   Mean   :0   Mean   :0  
#>  3rd Qu.:4   3rd Qu.:3   3rd Qu.:0   3rd Qu.:1   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :4   Max.   :3   Max.   :0   Max.   :1   Max.   :0   Max.   :0  
#>      X1097       X1099       X1103       X2478  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#> 
#> $ped8
#>       X676        X677        X808        X809        X810        X1632  
#>  Min.   :3   Min.   :0   Min.   :0   Min.   :0   Min.   :13   Min.   :0  
#>  1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:13   1st Qu.:0  
#>  Median :3   Median :0   Median :0   Median :0   Median :13   Median :0  
#>  Mean   :3   Mean   :0   Mean   :0   Mean   :0   Mean   :13   Mean   :0  
#>  3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:13   3rd Qu.:0  
#>  Max.   :3   Max.   :0   Max.   :0   Max.   :0   Max.   :13   Max.   :0  
#>      X1634       X1635       X1928       X2183       X2185  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#> 
#> $ped9
#>       X788         X789       X1072       X1643        X1933       X1934  
#>  Min.   :16   Min.   :0   Min.   :0   Min.   :15   Min.   :0   Min.   :0  
#>  1st Qu.:16   1st Qu.:0   1st Qu.:0   1st Qu.:15   1st Qu.:0   1st Qu.:0  
#>  Median :16   Median :0   Median :0   Median :15   Median :0   Median :0  
#>  Mean   :16   Mean   :0   Mean   :0   Mean   :15   Mean   :0   Mean   :0  
#>  3rd Qu.:16   3rd Qu.:0   3rd Qu.:0   3rd Qu.:15   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :16   Max.   :0   Max.   :0   Max.   :15   Max.   :0   Max.   :0  
#>      X1998       X1999       X2187       X2189       X2270       X2271  
#>  Min.   :0   Min.   :3   Min.   :8   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:3   1st Qu.:8   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :3   Median :8   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :3   Mean   :8   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:3   3rd Qu.:8   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :3   Max.   :8   Max.   :0   Max.   :0   Max.   :0  
#>      X2272       X2273       X2317       X2319       X2479       X2480  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2539       X2540       X2891       X2892       X2893        X3042  
#>  Min.   :4   Min.   :3   Min.   :3   Min.   :4   Min.   :10   Min.   :0  
#>  1st Qu.:4   1st Qu.:3   1st Qu.:3   1st Qu.:4   1st Qu.:10   1st Qu.:0  
#>  Median :4   Median :3   Median :3   Median :4   Median :10   Median :0  
#>  Mean   :4   Mean   :3   Mean   :3   Mean   :4   Mean   :10   Mean   :0  
#>  3rd Qu.:4   3rd Qu.:3   3rd Qu.:3   3rd Qu.:4   3rd Qu.:10   3rd Qu.:0  
#>  Max.   :4   Max.   :3   Max.   :3   Max.   :4   Max.   :10   Max.   :0  
#>      X3117       X3118       X3119       X3121       X3401       X3402  
#>  Min.   :4   Min.   :0   Min.   :3   Min.   :3   Min.   :0   Min.   :0  
#>  1st Qu.:4   1st Qu.:0   1st Qu.:3   1st Qu.:3   1st Qu.:0   1st Qu.:0  
#>  Median :4   Median :0   Median :3   Median :3   Median :0   Median :0  
#>  Mean   :4   Mean   :0   Mean   :3   Mean   :3   Mean   :0   Mean   :0  
#>  3rd Qu.:4   3rd Qu.:0   3rd Qu.:3   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :4   Max.   :0   Max.   :3   Max.   :3   Max.   :0   Max.   :0  
#>      X3608       X3612       X3614       X3615       X3648       X3649  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3650       X3651       X3823       X3827       X3890       X3891  
#>  Min.   :0   Min.   :0   Min.   :4   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:4   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :4   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :4   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:4   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :4   Max.   :0   Max.   :0   Max.   :0  
#>      X4180       X4183       X4236       X4306       X4307       X4493  
#>  Min.   :0   Min.   :3   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :3   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :3   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :3   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4497       X4572       X4573       X4574       X4575       X4786  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4787       X4788  
#>  Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0  
#>  Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0  
#> 
#> $ped10
#>      X1047        X1463       X1707       X1708       X1709       X2030  
#>  Min.   :21   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:21   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :21   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :21   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:21   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :21   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2032       X2357       X2359       X2627       X3140       X3143  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :5   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:5   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :5   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :5   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:5   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :5   Max.   :0   Max.   :0  
#>      X3144       X3145       X3146       X3580       X3581  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#> 
#> $ped11
#>      X1060       X1061       X1062       X1403       X1404        X1964  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :14   Min.   :6  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:14   1st Qu.:6  
#>  Median :0   Median :0   Median :0   Median :0   Median :14   Median :6  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :14   Mean   :6  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:14   3rd Qu.:6  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :14   Max.   :6  
#>      X1965        X1967       X2223       X2227       X2229       X2351  
#>  Min.   :21   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:21   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :21   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :21   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:21   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :21   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2353       X2501       X2502       X2503       X2504       X2506   
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :10  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:10  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :10  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :10  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:10  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :10  
#>      X2617       X2618       X2619       X2845       X2846       X2847  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :3   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:3   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :3   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :3   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:3   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :3   Max.   :0   Max.   :0  
#>      X2879       X3123       X3293       X3466       X3467       X3472  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3474       X3560       X3562       X4132       X4222       X4520  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4521       X4522  
#>  Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0  
#>  Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0  
#> 
#> $ped12
#>      X1050       X1349       X1350        X1351       X1352        X1573  
#>  Min.   :0   Min.   :0   Min.   :16   Min.   :4   Min.   :16   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:16   1st Qu.:4   1st Qu.:16   1st Qu.:0  
#>  Median :0   Median :0   Median :16   Median :4   Median :16   Median :0  
#>  Mean   :0   Mean   :0   Mean   :16   Mean   :4   Mean   :16   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:16   3rd Qu.:4   3rd Qu.:16   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :16   Max.   :4   Max.   :16   Max.   :0  
#>      X1694       X1696       X1698       X2007       X2008       X2171  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2173       X2279       X2280       X2549       X2550       X2551  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2806       X2807       X2808       X2809       X2810       X2811  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3747  
#>  Min.   :0  
#>  1st Qu.:0  
#>  Median :0  
#>  Mean   :0  
#>  3rd Qu.:0  
#>  Max.   :0  
#> 
#> $ped13
#>      X1075       X1645       X1646       X1647       X1648       X1649  
#>  Min.   :6   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:6   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :6   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :6   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:6   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :6   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X1711       X1712       X1713       X1935  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#> 
#> $ped14
#>      X1988       X1990       X2262       X2263       X2264       X2266  
#>  Min.   :9   Min.   :2   Min.   :6   Min.   :0   Min.   :6   Min.   :0  
#>  1st Qu.:9   1st Qu.:2   1st Qu.:6   1st Qu.:0   1st Qu.:6   1st Qu.:0  
#>  Median :9   Median :2   Median :6   Median :0   Median :6   Median :0  
#>  Mean   :9   Mean   :2   Mean   :6   Mean   :0   Mean   :6   Mean   :0  
#>  3rd Qu.:9   3rd Qu.:2   3rd Qu.:6   3rd Qu.:0   3rd Qu.:6   3rd Qu.:0  
#>  Max.   :9   Max.   :2   Max.   :6   Max.   :0   Max.   :6   Max.   :0  
#>      X2518       X2519       X2533       X2535       X3790       X3791  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4245       X4454       X4455       X4545       X4546       X4547  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#> 
#> $ped15
#>      X1869       X1870       X1871       X1872       X2132       X2135  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2452       X3345       X3347       X3768  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#> 
#> $ped16
#>      X1982       X2240       X2241       X2242       X2243       X2260  
#>  Min.   :3   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:3   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :3   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :3   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:3   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :3   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X2515       X2516       X2529       X2531       X2860       X2863  
#>  Min.   :0   Min.   :0   Min.   :6   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:6   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :6   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :6   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:6   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :6   Max.   :0   Max.   :0   Max.   :0  
#>      X2881       X3295       X3487       X3488       X3489       X3491  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X3744       X3745  
#>  Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0  
#>  Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0  
#> 
#> $ped17
#>      X2583        X2985       X2988       X3061        X3062       X3344  
#>  Min.   :10   Min.   :0   Min.   :2   Min.   :13   Min.   :0   Min.   :0  
#>  1st Qu.:10   1st Qu.:0   1st Qu.:2   1st Qu.:13   1st Qu.:0   1st Qu.:0  
#>  Median :10   Median :0   Median :2   Median :13   Median :0   Median :0  
#>  Mean   :10   Mean   :0   Mean   :2   Mean   :13   Mean   :0   Mean   :0  
#>  3rd Qu.:10   3rd Qu.:0   3rd Qu.:2   3rd Qu.:13   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :10   Max.   :0   Max.   :2   Max.   :13   Max.   :0   Max.   :0  
#>      X3361       X3421       X4584       X4585       X4586       X4587  
#>  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
#>  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
#>      X4588       X4589  
#>  Min.   :0   Min.   :0  
#>  1st Qu.:0   1st Qu.:0  
#>  Median :0   Median :0  
#>  Mean   :0   Mean   :0  
#>  3rd Qu.:0   3rd Qu.:0  
#>  Max.   :0   Max.   :0  
#> 
#> free parameters:
#>     name       matrix row  col     Estimate Std.Error A lbound ubound
#> 1    vad ModelOne.Vad   1    1 1.000055e-10 0.1675854 !     0!       
#> 2    vcn ModelOne.Vcn   1    1 4.114450e+00 1.1233310    1e-10       
#> 3    ver ModelOne.Ver   1    1 9.299892e+00 0.9632271    1e-10       
#> 4 meanLI       ped1.M   1 X208 1.256922e+00 0.1098559                
#> 
#> confidence intervals:
#>     lbound     estimate    ubound note
#> vad  1e-10 1.000055e-10 0.2676144     
#> vcn     NA 4.114450e+00        NA  !!!
#> ver     NA 9.299892e+00        NA  !!!
#> 
#> CI details:
#>   parameter  side        value           fit            diagnostic statusCode
#> 1       vad upper 2.676144e-01    -0.2676144               success         OK
#> 2       vad lower 1.000000e-10 14465.0237599               success         OK
#> 3       vcn upper 5.994868e+00 14468.8846236 active box constraint         OK
#> 4       vcn lower 1.654734e+00     1.6547339 active box constraint         OK
#> 5       ver upper 1.180138e+01 14468.8797453 active box constraint         OK
#> 6       ver lower 7.757062e+00 14468.8670172 active box constraint         OK
#>              method          vad      vcn       ver   meanLI
#> 1     wu-neale-2012 0.2676143540 4.012815  9.202437 1.136806
#> 2     wu-neale-2012 0.2676143540 4.012815  9.202437 1.136806
#> 3 neale-miller-1997 0.0000000001 5.994868  7.926394 1.269994
#> 4     wu-neale-2012 0.0000000001 1.654734 11.454552 1.223191
#> 5 neale-miller-1997 0.0000000001 1.460326 11.801383 1.223114
#> 6 neale-miller-1997 0.0000000001 5.775866  7.757062 1.271447
#> 
#> Model Statistics: 
#>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
#>        Model:              4                   2671              14465.02
#>    Saturated:             NA                     NA                    NA
#> Independence:             NA                     NA                    NA
#> Number of observations/statistics: 17/2675
#> 
#> condition number of the information matrix:  67.44452 
#> maximum absolute gradient:  5.834809  ( vad )
#> chi-square:  χ² ( df=NA ) = NA,  p = NA
#> Information Criteria: 
#>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
#> AIC:       9123.024               14473.02                 14476.36
#> BIC:       6897.511               14476.36                 14464.09
#> CFI: NA 
#> TLI: NA   (also known as NNFI) 
#> RMSEA:  NA  [95% CI (NA, NA)]
#> Prob(RMSEA <= 0.05): NA
#> To get additional fit indices, see help(mxRefModels)
#> timestamp: 2026-03-30 19:30:34 
#> frontend time: 39.95698 secs 
#> backend time: 1.358212 hours 
#> independent submodels time: 8.106232e-06 secs 
#> cpu time: 4929.521 secs 
#> Wall clock time: 4929.521 secs 
#> OpenMx version number: 2.22.11 
#> Need help?  See help(mxSummary)
summary(fitted_multi_ace)$CI
#>     lbound     estimate    ubound note
#> vad  1e-10 1.000055e-10 0.2676144     
#> vcn     NA 4.114450e+00        NA  !!!
#> ver     NA 9.299892e+00        NA  !!!


total_var_ace <- sum(
  fitted_multi_ace$ModelOne$Vad$values,
  fitted_multi_ace$ModelOne$Vcn$values,
  # fitted_multi_ace$ModelOne$Vmt$values,
  fitted_multi_ace$ModelOne$Ver$values
)
```

``` r
cat("Additive genetic (Vad):", fitted_multi_ace$ModelOne$Vad$values / total_var_ace, "\n")
#> Additive genetic (Vad): 7.455119e-12
cat("Common nuclear  (Vcn):", fitted_multi_ace$ModelOne$Vcn$values / total_var_ace, "\n")
#> Common nuclear  (Vcn): 0.3067203
cat("Unique environ. (Ver):", fitted_multi_ace$ModelOne$Ver$values / total_var_ace, "\n")
#> Unique environ. (Ver): 0.6932797
```

Now we can compare the ACE and MACE models to see if including the
mitochondrial component changes our estimates of heritability and
environmental contributions. This can provide insights into the role of
mitochondrial inheritance in the trait of interest (LRS) in these
squirrels. In the original paper, the authors found that a maternally
inherited component (which could be due to mitochondria or maternal
effects) explained a significant portion of the variance in LRS,.

``` r
mxCompare(fitted_multi_mace, fitted_multi_ace)
#>                 base         comparison ep minus2LL   df      AIC        diffLL
#> 1 MultiPedigreeModel               <NA>  5 14465.02 2670 14475.02            NA
#> 2 MultiPedigreeModel MultiPedigreeModel  4 14465.02 2671 14473.02 -6.135451e-09
#>   diffdf  p
#> 1     NA NA
#> 2      1  1
mxCompare(fitted_multi_ace, fitted_multi_ce)
#>                 base         comparison ep minus2LL   df      AIC        diffLL
#> 1 MultiPedigreeModel               <NA>  4 14465.02 2671 14473.02            NA
#> 2 MultiPedigreeModel MultiPedigreeModel  3 14465.02 2672 14471.02 -6.202754e-10
#>   diffdf  p
#> 1     NA NA
#> 2      1  1
```

However, as you can see when we compare the ACE and MACE models, the
inclusion of the mitochondrial component does not substantially change
the estimates of additive genetic and common nuclear environmental
variance, suggesting that the mitochondrial component may not be a major
contributor to LRS in this dataset.
