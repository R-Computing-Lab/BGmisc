# Build a Temporal Covariance Sub-model

Wrapper around
[`buildPedigreeModelCovariance`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeModelCovariance.md)
with `temporal = TRUE`.

## Usage

``` r
buildTemporalPedigreeModelCovariance(
  p_hist,
  components = c("a", "d", "cn", "ce", "mt", "e"),
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

  Integer. Number of historical moderator columns.

- components:

  Character vector of component keys to include when `temporal = TRUE`
  (any of "a", "d", "cn", "ce", "mt", "am", "e"). Default is
  `c("a", "e")`.

- start_beta0:

  Numeric. Starting value for each component's intercept loading. Only
  used when `temporal = TRUE`. Default is 0.5.

- start_beta_time:

  Numeric. Starting value for each component's time-slope loadings. Only
  used when `temporal = TRUE`. Default is 0.

- start_gamma:

  Numeric. Starting value for historical-moderator loadings. Only used
  when `temporal = TRUE`. Default is 0.

- time_point_max:

  Integer. Degree of the polynomial birth-year basis. Only used when
  `temporal = TRUE`. Default is 3.

- mean_degree:

  Integer. Degree of the polynomial temporal mean. The default, 0,
  estimates a constant mean. Must not exceed `time_point_max`.

- start_mean:

  Numeric. Starting value for the mean's intercept. Set this near the
  observed phenotype mean.

- mean_hist_free:

  Logical, length 1 or `p_hist`. Whether historical moderators can shift
  the phenotypic mean. Defaults to FALSE, fixing those shifts at zero.

- start_mean_hist:

  Numeric. Starting value for the free elements of `G_mean`.

## Value

An OpenMx model containing the `B_*`/`G_*` parameter matrices.
