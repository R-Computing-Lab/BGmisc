# Build the temporal loading sub-model (internal)

Builds the parent `ModelOne` sub-model for a time-varying pedigree
model: for each requested component, a vector of polynomial-time
loadings (`B_k`) and, when `p_hist > 0`, a vector of
historical-moderator loadings (`G_k`). Also builds the mean's
coefficient vector (`B_mean`, and `G_mean` when `p_hist > 0`), which
lets the phenotypic mean vary over the same basis as the loadings.

## Usage

``` r
.buildTemporalLoadingModel(
  p_hist = 0,
  components = c("a", "e"),
  start_beta0 = 0.5,
  start_beta_time = 0,
  start_gamma = 0,
  time_point_max = NULL,
  mean_degree = 0,
  start_mean = 0,
  mean_hist_free = FALSE,
  start_mean_hist = 0
)
```

## Arguments

- p_hist:

  Integer. Number of historical moderator columns. Default is 0.

- components:

  Character vector of component keys (any of "a", "d", "cn", "ce", "mt",
  "am", "e").

- start_beta0:

  Numeric starting value for each component's intercept loading.

- start_beta_time:

  Numeric starting value for each component's time-slope loadings.

- start_gamma:

  Numeric starting value for historical-moderator loadings.

- time_point_max:

  Integer degree of the polynomial time basis. Defaults to 3 when NULL.

- mean_degree:

  Integer degree of the temporal mean. 0 (the default) frees only the
  intercept, which reproduces a single constant mean. Must not exceed
  `time_point_max`, since the mean shares the components' `Tpoly` basis.

- start_mean:

  Numeric starting value for the mean's intercept. Set this near the
  observed phenotype mean; the optimizer starts from it.

- mean_hist_free:

  Logical, length 1 or `p_hist`. Whether each historical moderator is
  allowed to shift the phenotypic mean. Defaults to FALSE, which
  reproduces the previous behaviour of fixing every mean shift at zero.
  Set TRUE to estimate them. Note that leaving these fixed while the
  components' own `G_*` coefficients are free forces any event-driven
  change in the mean to be absorbed by the variance components.

- start_mean_hist:

  Numeric starting value for the free elements of `G_mean`.

## Value

An OpenMx model containing the `B_*`/`G_*` parameter matrices.

## Details

Named for what it builds rather than mirroring the exported
[`buildTemporalPedigreeModelCovariance`](https://r-computing-lab.github.io/BGmisc/reference/buildTemporalPedigreeModelCovariance.md),
which differed from it by a single leading dot.
