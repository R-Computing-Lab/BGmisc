# Build the temporal mean part for a family group (internal)

Build the temporal mean part for a family group (internal)

## Usage

``` r
.pedigreeMeanPart(
  fsize,
  obs_ids,
  label,
  mean_basis = NULL,
  mean_hist = NULL,
  parent = "ModelOne"
)
```

## Arguments

- fsize:

  Family size.

- obs_ids:

  Character vector of individual IDs.

- label:

  The mean parameter's label, used only for the constant-mean case.

- mean_basis:

  NULL for a single free mean (default), or the name of a fixed basis
  matrix in the group model whose columns the mean is regressed on.

- mean_hist:

  NULL, or the name of the historical-moderator matrix to add to the
  mean.

- parent:

  Name of the sub-model holding `B_mean`/`G_mean`.

## Value

An mxMatrix or mxAlgebra named "M".
