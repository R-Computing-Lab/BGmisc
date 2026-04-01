# Extended: Fitting Pedigree-Based Variance Component Models

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

### Model Building

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

### Model Fitting

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

#### MACE Model Results

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

As you can see, the MACE model includes an additional variance component
for mitochondrial effects (Vmt), which allows us to assess the
contribution of mitochondrial inheritance to the trait of interest (LRS)
in these squirrels. The results show the proportion of total variance
attributed to each component, which can be interpreted as heritability
and environmental contributions to LRS in these squirrels. We can also
compare the ACE and MACE models to see if including the mitochondrial
component changes our estimates of heritability and environmental
contributions.

#### ACE Model Results

Now we can look at the results from the ACE model, which does not
include the mitochondrial component. This allows us to compare the
estimates of additive genetic and common nuclear environmental variance
between the ACE and MACE models, and assess whether including the
mitochondrial component changes our estimates of heritability and
environmental contributions to LRS in these squirrels.

``` r


summary(fitted_multi_ace)
#> Summary of MultiPedigreeModel 
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
#>   To investigate missing CIs, run summary() again, with verbose=T, to see CI details. 
#> 
#> Model Statistics: 
#>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
#>        Model:              4                   2671              14465.02
#>    Saturated:             NA                     NA                    NA
#> Independence:             NA                     NA                    NA
#> Number of observations/statistics: 17/2675
#> 
#> Information Criteria: 
#>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
#> AIC:       9123.024               14473.02                 14476.36
#> BIC:       6897.511               14476.36                 14464.09
#> To get additional fit indices, see help(mxRefModels)
#> timestamp: 2026-03-30 19:30:34 
#> Wall clock time: 4929.521 secs 
#> optimizer:  SLSQP 
#> OpenMx version number: 2.22.11 
#> Need help?  See help(mxSummary)
summary(fitted_multi_ace, verbose = T)$CI
#>     lbound     estimate    ubound note
#> vad  1e-10 1.000055e-10 0.2676144     
#> vcn     NA 4.114450e+00        NA  !!!
#> ver     NA 9.299892e+00        NA  !!!

#summary(fitted_multi_ace)$CI


total_var_ace <- sum(
  fitted_multi_ace$ModelOne$Vad$values,
  fitted_multi_ace$ModelOne$Vcn$values,
  # fitted_multi_ace$ModelOne$Vmt$values,
  fitted_multi_ace$ModelOne$Ver$values
)
```

As you can see, the ACE model includes variance components for additive
genetic effects (Vad), common nuclear environmental effects (Vcn), and
unique environmental effects (Ver), but does not include a component for
mitochondrial effects (Vmt). The results show the proportion of total
variance attributed to each component, which can be interpreted as
heritability and environmental contributions to LRS in these squirrels.
We can compare these estimates to those from the MACE model to assess
the contribution of mitochondrial inheritance to LRS in these squirrels.

``` r
cat("Additive genetic (Vad):", fitted_multi_ace$ModelOne$Vad$values / total_var_ace, "\n")
#> Additive genetic (Vad): 7.455119e-12
cat("Common nuclear  (Vcn):", fitted_multi_ace$ModelOne$Vcn$values / total_var_ace, "\n")
#> Common nuclear  (Vcn): 0.3067203
cat("Unique environ. (Ver):", fitted_multi_ace$ModelOne$Ver$values / total_var_ace, "\n")
#> Unique environ. (Ver): 0.6932797
```

#### CE Model Results

``` r

summary(fitted_multi_ce)
#> Summary of MultiPedigreeModel 
#>  
#> free parameters:
#>     name       matrix row  col Estimate  Std.Error A lbound ubound
#> 1    vcn ModelOne.Vcn   1    1 4.114448 1.08082448    1e-10       
#> 2    ver ModelOne.Ver   1    1 9.299891 0.93752032    1e-10       
#> 3 meanLI       ped1.M   1 X208 1.256923 0.07604897                
#> 
#> confidence intervals:
#>       lbound estimate    ubound note
#> vcn       NA 4.114448  5.994869  !!!
#> ver 7.756658 9.299891 11.797608     
#>   To investigate missing CIs, run summary() again, with verbose=T, to see CI details. 
#> 
#> Model Statistics: 
#>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
#>        Model:              3                   2672              14465.02
#>    Saturated:             NA                     NA                    NA
#> Independence:             NA                     NA                    NA
#> Number of observations/statistics: 17/2675
#> 
#> Information Criteria: 
#>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
#> AIC:       9121.024               14471.02                 14472.87
#> BIC:       6894.678               14473.52                 14464.32
#> To get additional fit indices, see help(mxRefModels)
#> timestamp: 2026-03-31 05:14:48 
#> Wall clock time: 2201.804 secs 
#> optimizer:  SLSQP 
#> OpenMx version number: 2.22.11 
#> Need help?  See help(mxSummary)

summary(fitted_multi_ce, verbose = T)$CI
#>       lbound estimate    ubound note
#> vcn       NA 4.114448  5.994869  !!!
#> ver 7.756658 9.299891 11.797608

total_var_ce <- sum(
  # fitted_multi_ce$ModelOne$Vad$values,
  fitted_multi_ce$ModelOne$Vcn$values,
  # fitted_multi_ce$ModelOne$Vmt$values,
  fitted_multi_ce$ModelOne$Ver$values
)
```

As you can see, the CE model includes variance components for common
nuclear environmental effects (Vcn) and unique environmental effects
(Ver), but does not include a component for additive genetic effects
(Vad) or mitochondrial effects (Vmt). The results show the proportion of
total variance attributed to each component, which can be interpreted as
environmental contributions to LRS in these squirrels. We can compare
these estimates to those from the ACE and MACE models to assess the
contribution of additive genetic and mitochondrial inheritance to LRS in
these squirrels.

``` r

cat("Common nuclear  (Vcn):", fitted_multi_ce$ModelOne$Vcn$values / total_var_ce, "\n")
#> Common nuclear  (Vcn): 0.3067201
cat("Unique environ. (Ver):", fitted_multi_ce$ModelOne$Ver$values/ total_var_ce, "\n")
#> Unique environ. (Ver): 0.6932799
```

### Model Comparison

All of these models are nested, with the CE model being the most
constrained (only common environment and unique environment), the ACE
model including additive genetic effects, and the MACE model including
both additive genetic effects and mitochondrial effects.

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
```

However, as you can see when we compare the ACE and MACE models, the
inclusion of the mitochondrial component does not substantially change
the estimates of additive genetic and common nuclear environmental
variance, suggesting that the mitochondrial component may not be a major
contributor to LRS in this dataset.

``` r

mxCompare(fitted_multi_ace, fitted_multi_ce)
#>                 base         comparison ep minus2LL   df      AIC        diffLL
#> 1 MultiPedigreeModel               <NA>  4 14465.02 2671 14473.02            NA
#> 2 MultiPedigreeModel MultiPedigreeModel  3 14465.02 2672 14471.02 -6.202754e-10
#>   diffdf  p
#> 1     NA NA
#> 2      1  1
```

When we compare the ACE and CE models, we see that the ACE model does
not fit significantly better than the CE model. This suggests that the
additive genetic component may not be a major contributor to LRS in this
dataset, and that the variance in LRS may be primarily due to
environmental factors. However, it’s important to interpret these
results in the context of the specific dataset and trait being analyzed,
and to consider other factors such as sample size, pedigree structure,
and potential confounding variables that could influence the estimates
of variance components.
