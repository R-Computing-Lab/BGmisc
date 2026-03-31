# Extended: Fitting Pedigree-Based Variance Component Models

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
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.2.0     ✔ readr     2.2.0
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
#> ✔ purrr     1.2.1     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
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

cat(
  "KRSP pedigree:", nrow(ped_krsp), "individuals,",
  n_distinct(ped_krsp$famID), "grids\n"
)
#> KRSP pedigree: 7799 individuals, 1100 grids
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

``` r
minim_family_size <- 10

ped_krsp_subset <- ped_krsp |>
  group_by(famID) |>
  filter(sum(!is.na(lrs)) >= minim_family_size) |>
  ungroup()

id_families <- unique(ped_krsp_subset$famID)
n_families <- length(id_families)

# Pre-allocate storage
add_list <- vector("list", length(n_families))
cn_list <- vector("list", length(n_families))
mt_list <- vector("list", length(n_families))
obs_ids_list <- vector("list", length(n_families))
pheno_list <- vector("list", length(n_families))


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
```

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


multi_model_ace <- buildPedigreeMx(
  model_name   = "MultiPedigreeModel",
  vars         = start_vars,
  group_models = group_models_ace,
  ci = TRUE
)

multi_model_mace <- buildPedigreeMx(
  model_name   = "MultiPedigreeModel",
  vars         = start_vars,
  group_models = group_models_mace,
  ci = TRUE
)

multi_model_ce <- buildPedigreeMx(
  model_name   = "MultiPedigreeModel",
  vars         = start_vars,
  group_models = group_models_ce,
  ci = TRUE
)
```

``` r
fitted_multi_ace <- mxRun(multi_model_ace,
                          intervals= TRUE)
saveRDS(fitted_multi_ace, "inst/extdata/fitted_multi_ace.rds")

fitted_multi_mace <- mxRun(multi_model_mace, intervals= TRUE)
saveRDS(fitted_multi_mace, "inst/extdata/fitted_multi_mace.rds")

fitted_multi_ce <- mxRun(multi_model_ce, intervals= TRUE)
saveRDS(fitted_multi_ce, "inst/extdata/fitted_multi_ce.rds")
```

Note that fitting these models can take some time, especially with many
families and large pedigrees. The
[`mxTryHard()`](https://rdrr.io/pkg/OpenMx/man/mxTryHard.html) function
can be used to attempt to find better-fitting solutions if the initial
optimization does not converge well. In practice, you may want to
experiment with different starting values or optimization settings to
improve convergence.

    fitted_multi_ace <- mxRun(multi_model_ace)
    fitted_multi_mace <- mxRun(multi_model_mace)
    fitted_multi_ce <- mxRun(multi_model_ce)

``` r
summary(fitted_multi_ace,verbose=T)
summary(fitted_multi_ace)$CI



total_var_ace <- sum(fitted_multi_ace$ModelOne$Vad$values, 
                 fitted_multi_ace$ModelOne$Vcn$values,
                 #fitted_multi_ace$ModelOne$Vmt$values, 
                 fitted_multi_ace$ModelOne$Ver$values)
```

``` r
cat("Additive genetic (Vad):", fitted_multi_ace$ModelOne$Vad$values/total_var_ace, "\n")
cat("Common nuclear  (Vcn):", fitted_multi_ace$ModelOne$Vcn$values/total_var_ace, "\n")
cat("Unique environ. (Ver):", fitted_multi_ace$ModelOne$Ver$values/total_var_ace, "\n")
```

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
summary(fitted_multi_mace)$CI
total_var_mace <- sum(fitted_multi_mace$ModelOne$Vad$values, 
                 fitted_multi_mace$ModelOne$Vcn$values,
                 fitted_multi_mace$ModelOne$Vmt$values, 
                 fitted_multi_mace$ModelOne$Ver$values)
```

``` r
cat("Additive genetic (Vad):", fitted_multi_mace$ModelOne$Vad$values/total_var_mace, "\n")
cat("Common nuclear  (Vcn):", fitted_multi_mace$ModelOne$Vcn$values/total_var_mace, "\n")
cat("Mitochondrial (Vmt):", fitted_multi_mace$ModelOne$Vmt$values/total_var_mace, "\n")
cat("Unique environ. (Ver):", fitted_multi_mace$ModelOne$Ver$values/total_var_mace, "\n")
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
mxCompare(fitted_multi_ace, fitted_multi_ce)
```

However, as you can see when we compare the ACE and MACE models, the
inclusion of the mitochondrial component does not substantially change
the estimates of additive genetic and common nuclear environmental
variance, suggesting that the mitochondrial component may not be a major
contributor to LRS in this dataset.
