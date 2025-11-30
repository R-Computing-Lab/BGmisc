# Confidence Intervals for Correlations with Optional Design-Effect Adjustment

Compute confidence intervals (CIs) for correlation coefficients using
either Fisher's \\r \rightarrow z\\ approach (Raykov-style on the \\z\\
scale) or a Wald CI on the \\r\\ scale. Standard errors are first
\*\*adjusted\*\* by a design-effect factor when available, and
optionally for double entry. The adjusted standard errors are used for
all calculations, including CIs, z-tests, and p-values.

## Usage

``` r
calculateCIs(
  tbl,
  rho_var,
  se_var,
  doubleentered = FALSE,
  method = "raykov",
  adjust_base = 1,
  design_effect_m = NULL,
  design_effect_rho = NULL,
  design_effect_m_col = NULL,
  design_effect_rho_col = NULL,
  conf_level = 0.95
)
```

## Arguments

- tbl:

  A data frame or tibble containing the correlation coefficient and
  standard error variables.

- rho_var:

  The name of the column in `tbl` that contains the correlation
  coefficients.

- se_var:

  The name of the column in `tbl` that contains the standard errors.

- doubleentered:

  Logical. If `TRUE`, the function assumes that the correlation
  coefficients are double-entered, which adjusts the standard errors
  accordingly. Default is `FALSE`.

- method:

  Character; CI method selector. Supported values:

  - `"raykov"` — Fisher \\r \rightarrow z\\ CI (back-transformed).

  - `"fisherz"` — alias of `"raykov"`.

  - `"wald"` — Wald CI on the \\r\\ scale.

  - `"doubleentered"` — like `"raykov"` and, if `doubleentered` was not
    explicitly provided, it is set to `TRUE` (applies the \\\sqrt{2}\\
    multiplier).

  - `"doubleenteredconserv"` — like `"wald"` and, if `doubleentered` was
    not explicitly provided, it is set to `TRUE`.

- adjust_base:

  A numeric value to adjust the standard errors. Default is 1.

- design_effect_m:

  A numeric value for the design effect related to the mean. Default is
  `NULL`.

- design_effect_rho:

  A numeric value for the design effect related to the correlation.
  Default is `NULL`.

- design_effect_m_col:

  A character string specifying the column name for the design effect
  related to the mean. Default is `NULL`.

- design_effect_rho_col:

  A character string specifying the column name for the design effect
  related to the correlation. Default is `NULL`.

- conf_level:

  The confidence level for the intervals. Default is 0.95.

## Value

A modified version of `tbl` with additional columns for the confidence
intervals and related statistics. Everything uses adjusted standard
errors, including confidence intervals, z-tests, and p-values.

## Note

Double-entry handling and design effects are governed by
`doubleentered`, `design_effect_m`/`design_effect_rho` (or their `*_col`
variants), and `adjust_base`. The `"doubleentered*"` method values
simply provide convenient aliases: they toggle `doubleentered` to `TRUE`
only when the user hasn't explicitly set it, and map to `"raykov"` or
`"wald"` as described.

## Examples

``` r
tbl <- data.frame(rho = c(0.5, 0.7, 0.3), se = c(0.1, 0.2, 0.05))
calculateCIs(tbl, rho_var = "rho", se_var = "se", method = "raykov")
#>   rho   se     rho_z     se_sez se_se_adjusted se_sez_adjusted rho_plusse
#> 1 0.5 0.10 0.5493061 0.13333333           0.10      0.13333333  0.6699402
#> 2 0.7 0.20 0.8673005 0.39215686           0.20      0.39215686  0.9268992
#> 3 0.3 0.05 0.3095196 0.05494505           0.05      0.05494505  0.3945774
#>   rho_minusse rho_ztest  rho_zp1tail  rho_zp2tail rho_wald rho_waldp1tail
#> 1  0.28027235  4.119796 1.896039e-05 3.792079e-05      5.0   2.866516e-07
#> 2  0.09836807  2.211616 1.349659e-02 2.699319e-02      3.5   2.326291e-04
#> 3  0.19913270  5.633257 8.841892e-09 1.768378e-08      6.0   9.865876e-10
#>   rho_waldp2tail
#> 1   5.733031e-07
#> 2   4.652582e-04
#> 3   1.973175e-09
```
