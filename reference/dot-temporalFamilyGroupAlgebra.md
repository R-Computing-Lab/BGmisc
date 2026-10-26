# Build the temporal covariance algebra for one family group (internal)

Given a family's already-detected size, already-filtered component
table, and already-built relatedness mxMatrix objects (all shared with
the static branch via
[`buildOneFamilyGroup`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md)),
builds the time-varying covariance algebra: each component's relatedness
matrix is combined (via a Hadamard product) with a loading matrix
derived from a polynomial birth-year basis and, optionally, historical
moderators.

## Usage

``` r
.temporalFamilyGroupAlgebra(
  group_name,
  fsize,
  active,
  relmat_list,
  full_df_row,
  obs_ids,
  param_year,
  H = NULL,
  use_exp_loadings = TRUE,
  condenseMatrixSlots = TRUE,
  time_point_max = NULL,
  retain_eta = TRUE,
  retain_loadings = TRUE,
  retain_loading_covariances = TRUE,
  retain_component_covariances = TRUE,
  residual_covariance_form = c("outer_product", "diagonal"),
  mean_degree = 0,
  start_mean = 0,
  mean_basis = "Tpoly",
  mean_hist = "H"
)
```

## Arguments

- group_name:

  Name of the family group.

- fsize:

  Family size (number of members), as computed by
  [`buildOneFamilyGroup`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md).

- active:

  The filtered component table built by
  [`buildOneFamilyGroup`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md)
  (one entry per relatedness matrix actually supplied), used for its `k`
  loading key and `mxname` relatedness-matrix name.

- relmat_list:

  The list of already-built (and already condensed, if requested)
  `"Symm"` mxMatrix objects for `active`, in the same order.

- full_df_row:

  A 1-row matrix/vector of observed data.

- obs_ids:

  Character vector of individual IDs, matching `full_df_row`.

- param_year:

  Numeric vector of birth years, one per family member, matching
  `obs_ids`.

- H:

  Optional numeric matrix of historical moderators.

- use_exp_loadings:

  Logical. If TRUE, each component's loading is exponentiated.

- condenseMatrixSlots:

  Logical. If TRUE, condense the `Tpoly`/`H` mxMatrix objects.

- time_point_max:

  Integer degree of the polynomial birth-year basis.

- retain_eta:

  Logical. Retain named `Eta_*` algebras.

- retain_loadings:

  Logical. Retain named `L_*` algebras.

- retain_loading_covariances:

  Logical. Retain named `K*` outer-product algebras.

- retain_component_covariances:

  Logical. Retain named `Cov_*` algebras.

- residual_covariance_form:

  Character representation for the residual covariance.

## Value

An OpenMx model for the specified family group.
