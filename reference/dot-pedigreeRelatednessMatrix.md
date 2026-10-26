# Build a relatedness mxMatrix (internal)

Builds the `"Symm"` mxMatrix for one relatedness matrix. Shared by the
static and temporal branches of
[`buildOneFamilyGroup`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md):
the only difference between them is whether the input matrix is
symmetrized first (temporal) or used as-is (static, to preserve its
existing behavior exactly).

## Usage

``` r
.pedigreeRelatednessMatrix(
  mat,
  fsize,
  name,
  condense = TRUE,
  symmetrize = FALSE
)
```

## Arguments

- mat:

  The relatedness matrix.

- fsize:

  Family size.

- name:

  The mxMatrix's name (e.g. "A", "Cn").

- condense:

  Logical. If TRUE, apply
  [`condenseMatrixSlots`](https://r-computing-lab.github.io/BGmisc/reference/condenseMatrixSlots.md).

- symmetrize:

  Logical. If TRUE, symmetrize `mat` via
  [`make_symmetric`](https://r-computing-lab.github.io/BGmisc/reference/make_symmetric.md)
  before use; if FALSE, coerce via `as.matrix` only.

## Value

An mxMatrix object.
