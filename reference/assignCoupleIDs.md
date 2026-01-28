# Assign Couple IDs

This subfunction assigns a unique couple ID to each mated pair in the
generation. Unmated individuals are assigned NA for their couple ID.

## Usage

``` r
assignCoupleIDs(df_Ngen, beta = FALSE)

assignCoupleIds(df_Ngen, beta = FALSE)

assignCoupleIDs_beta(df_Ngen)
```

## Arguments

- df_Ngen:

  The dataframe for the current generation, including columns for
  individual IDs and spouse IDs.

- beta:

  Logical, indicating whether to use the beta version of the function.

## Value

The input dataframe augmented with a 'coupleId' column, where each mated
pair has a unique identifier.
