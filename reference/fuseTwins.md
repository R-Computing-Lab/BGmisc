# Fuse MZ twin pairs in a pedigree dataset for path tracing This function identifies MZ twin pairs in the pedigree dataset and merges their IDs for path tracing purposes. The second twin in each pair is made a founder (with NA parents), and all children of the second twin are redirected to the first twin. This allows for correct relatedness calculations without diagonal or downstream artifacts.

Fuse MZ twin pairs in a pedigree dataset for path tracing This function
identifies MZ twin pairs in the pedigree dataset and merges their IDs
for path tracing purposes. The second twin in each pair is made a
founder (with NA parents), and all children of the second twin are
redirected to the first twin. This allows for correct relatedness
calculations without diagonal or downstream artifacts.

## Usage

``` r
fuseTwins(
  ped,
  df_twins = NULL,
  mz_id_pairs = NULL,
  mz_row_pairs = NULL,
  config = list(verbose = FALSE),
  test_df_twins = FALSE,
  beta = FALSE
)
```

## Arguments

- ped:

  A pedigree data.frame with columns `ID`, `momID`, `dadID`, and
  optionally `twinID` and `zygosity`. The function will look for MZ twin
  pairs based on the `twinID` column and optionally restrict to MZ pairs
  if a `zygosity` column is present.

- df_twins:

  Optional data frame with columns `twin1_id`, `twin2_id`, `twin1_row`,
  and `twin2_row` specifying the IDs and row indices of MZ twin pairs to
  fuse. If provided, this will be used instead of automatically
  identifying MZ twins from the `twinID` column. If this parameter is
  provided, it takes precedence over `mz_id_pairs` and `mz_row_pairs`.
  If `test_df_twins` is TRUE, this data frame will be returned for
  testing purposes instead of performing the fusion.

- mz_id_pairs:

  Optional list of length-2 character vectors specifying the IDs of MZ
  twin pairs to fuse. If provided, this will be used instead of
  automatically identifying MZ twins from the `twinID` column. Each
  element should be a character vector of length 2, e.g.
  `list(c("ID1", "ID2"), c("ID3", "ID4"))`.

- mz_row_pairs:

  Optional list of length-2 integer vectors specifying the row indices
  of MZ twin pairs to fuse. If provided, this will be used instead of
  automatically identifying MZ twins from the `twinID` column. Each
  element should be an integer vector of length 2, e.g.
  `list(c(1, 2), c(3, 4))`.

- config:

  A list of configuration options.

- test_df_twins:

  logical. If TRUE, return the data frame of twin pairs instead of the
  modified pedigree. Default is FALSE.

- beta:

  logical. If TRUE, use an optimized approach with O(1) lookups for
  large pedigrees when identifying MZ twins. Default is FALSE.

## Value

A modified version of the input pedigree data.frame with MZ twin pairs
fused for path tracing. If `test_df_twins` is TRUE, returns the data
frame of identified twin pairs instead.
