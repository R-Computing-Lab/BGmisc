# Pedigree Simulation and Visualization with BGmisc

## Introduction

Unlike Tolstoy, where *only* happy families are alike, all pedigrees are
alike – or at least, all simulated pedigrees are alike. The
`simulatePedigree` function generates a pedigree with a user-specified
number of generations and individuals per generation. This function
provides users the opportunity to test family models in pedigrees with a
customized pedigree length and width.

These pedigrees can be simulated as a function of several parameters,
including the number of children per mate, generations, sex ratio of
newborns, and mating rate. Given that large family pedigrees are
difficult to collect or access, simulated pedigrees serve as an
efficient tool for researchers. These simulated pedigrees are useful for
building family-based statistical models, and evaluating their
statistical properties, such as power, bias, and computational
efficiency.

To illustrate this functionality, let us generate a pedigree. This
pedigree has a total of four generations (`Ngen`), in which each person
who “mates”, grows a family with four offspring (`kpc`). In our
scenario, the number of male and female newborns is equal, but can be
adjusted via (`sexR`). In this illustration 70% of individuals will mate
and bear offspring (`marR`). Such a pedigree structure can be simulated
by running the following code:

``` r
## Loading Required Libraries
library(BGmisc)
library(ggpedigree)
set.seed(5)
df_ped <- simulatePedigree(
  kpc = 4,
  Ngen = 4,
  sexR = .5,
  marR = .7
)
summary(df_ped)
#>      fam                  ID             gen            dadID      
#>  Length:57          Min.   :10101   Min.   :1.000   Min.   :10102  
#>  Class :character   1st Qu.:10306   1st Qu.:3.000   1st Qu.:10204  
#>  Mode  :character   Median :10320   Median :3.000   Median :10307  
#>                     Mean   :10342   Mean   :3.298   Mean   :10263  
#>                     3rd Qu.:10416   3rd Qu.:4.000   3rd Qu.:10311  
#>                     Max.   :10432   Max.   :4.000   Max.   :10320  
#>                                                     NA's   :13     
#>      momID            spID           sex           
#>  Min.   :10101   Min.   :10101   Length:57         
#>  1st Qu.:10202   1st Qu.:10205   Class :character  
#>  Median :10306   Median :10306   Mode  :character  
#>  Mean   :10263   Mean   :10266                     
#>  3rd Qu.:10316   3rd Qu.:10311                     
#>  Max.   :10318   Max.   :10320                     
#>  NA's   :13      NA's   :33
```

The simulation output is a `data.frame` with 57 rows and 7 columns. Each
row corresponds to a simulated individual.

``` r
df_ped[21, ]
#>      fam    ID gen dadID momID  spID sex
#> 21 fam 1 10312   3 10204 10202 10317   M
```

The columns represents the individual’s family ID, the individual’s
personal ID, the generation the individual is in, the IDs of their
father and mother, the ID of their spouse, and the biological sex of the
individual, respectively.

### Summarizing Pedigrees

``` r
summarizeFamilies(df_ped, famID = "fam")$family_summary
#>       fam count gen_mean gen_median gen_min gen_max    gen_sd spID_mean
#>    <char> <int>    <num>      <num>   <num>   <num>     <num>     <num>
#> 1:  fam 1    57 3.298246          3       1       4 0.8229935     10266
#>    spID_median spID_min spID_max  spID_sd
#>          <num>    <num>    <num>    <num>
#> 1:     10305.5    10101    10320 68.79206
```

### Plotting Pedigree

Pedigrees are visual diagrams that represent family relationships across
generations. They are commonly used in genetics to trace the inheritance
of specific traits or conditions. This vignette will guide you through
visualizing simulated pedigrees using the `plotPedigree` function. This
function is a wrapper function for `Kinship2`’s base R plotting. The
sister package ggpedigree has a much nicer plotting function. It’s also
available on CRAN, but it is not a dependency of BGmisc. If you want to
use ggpedigree, you can install it with `install.packages("ggpedigree")`
and then use `ggplot2` syntax to plot pedigrees.

#### Single Pedigree Visualization

To visualize a single simulated pedigree, use the the `plotPedigree`
function allows you to visualize the pedigree structure, including
family relationships and individual characteristics. The plot displays
individuals across generations, with lines connecting parents to their
children, and spouses connected by horizontal lines.

``` r
library(ggpedigree)

df_ped_recoded <- recodeSex(df_ped, code_male = "M", recode_male = 1, recode_female = 0)

ggpedigree::ggpedigree(df_ped_recoded,
  personID = "ID",
  code_male = 1
)
```

![](v2_pedigree_files/figure-html/unnamed-chunk-5-1.png)

In the resulting plot, biological males are represented by squares,
while biological females are represented by circles, following the
standard pedigree conventions.

#### Visualizing Multiple Pedigrees Side-by-Side

If you wish to compare different pedigrees side by side, you can plot
them together. For instance, let’s visualize pedigrees for families
spanning three and four generations, respectively.

``` r
set.seed(8)
# Simulate a family with 3 generations
df_ped_3 <- simulatePedigree(Ngen = 3)

# Simulate a family with 4 generations
df_ped_4 <- simulatePedigree(Ngen = 4)
```

You can use the `ggpedigree` package to plot multiple pedigrees side by
side. This package allows for more customization and better aesthetics
in pedigree visualization.

![](v2_pedigree_files/figure-html/unnamed-chunk-7-1.png)

By examining the side-by-side plots, you can contrast and analyze the
structures of different families, tracing the inheritance of specific
traits or conditions if needed.
