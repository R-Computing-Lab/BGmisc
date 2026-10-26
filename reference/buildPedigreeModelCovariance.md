# Create an mxModel for a pedigree

This function builds an OpenMx model for a pedigree with specified
variance components. It requires the OpenMx package.

## Usage

``` r
buildPedigreeModelCovariance(
  vars = list(ad2 = 0.5, dd2 = 0.3, cn2 = 0.2, ce2 = 0.4, mt2 = 0.1, am2 = 0.25, ee2 =
    0.6),
  Vad = TRUE,
  Vdd = FALSE,
  Vcn = TRUE,
  Vce = TRUE,
  Vmt = TRUE,
  Vam = FALSE,
  Ver = TRUE,
  temporal = FALSE,
  p_hist = 0,
  components = c("a", "e"),
  start_beta0 = 0.5,
  start_beta_time = 0,
  start_gamma = 0,
  time_point_max = NULL,
  mean_degree = 0,
  start_mean = 0,
  mean_hist_free = FALSE,
  start_mean_hist = 0,
  lbound = 1e-10
)
```

## Arguments

- vars:

  A named list or vector of initial variance component values. Names
  should include ad2 (additive), dd2 (dominance), cn2 (common nuclear),
  ce2 (common extended), mt2 (mitochondrial), am2
  (additive-mitochondrial interaction), and ee2 (unique environment).
  Default values are provided. Only used when `temporal = FALSE`.

- Vad:

  Logical. Include additive genetic variance component. Default is TRUE.

- Vdd:

  Logical. Include dominance genetic variance component. Default is
  FALSE.

- Vcn:

  Logical. Include common nuclear family environment variance component.
  Default is TRUE.

- Vce:

  Logical. Include common extended family environment variance
  component. Default is TRUE.

- Vmt:

  Logical. Include mitochondrial genetic variance component. Default is
  TRUE.

- Vam:

  Logical. Include additive by mitochondrial interaction variance
  component. Default is FALSE.

- Ver:

  Logical. Include unique environmental variance component. Default is
  TRUE.

- temporal:

  Logical. If TRUE, build a time-varying covariance sub-model in which
  each component's weight is a function of birth year (and, optionally,
  historical moderators) rather than a fixed scalar. See `p_hist`,
  `components`, `start_beta0`, `start_beta_time`, `start_gamma`, and
  `time_point_max`. Default is FALSE.

- p_hist:

  Integer. Number of historical moderator columns. Only used when
  `temporal = TRUE`. Default is 0.

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

- lbound:

  Numeric. A lower bound for the variance components to ensure they
  remain positive during optimization. Default is 1e-10

## Value

An OpenMx model representing the pedigree with specified variance
components.
