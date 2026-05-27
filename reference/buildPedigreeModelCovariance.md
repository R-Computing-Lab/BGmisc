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
  lbound = 1e-10
)
```

## Arguments

- vars:

  A named list or vector of initial variance component values. Names
  should include ad2 (additive), dd2 (dominance), cn2 (common nuclear),
  ce2 (common extended), mt2 (mitochondrial), am2
  (additive-mitochondrial interaction), and ee2 (unique environment).
  Default values are provided.

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

- lbound:

  Numeric. A lower bound for the variance components to ensure they
  remain positive during optimization. Default is 1e-10

## Value

An OpenMx model representing the pedigree with specified variance
components.
