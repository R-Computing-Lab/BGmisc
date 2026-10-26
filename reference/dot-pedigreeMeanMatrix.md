# Build the free mean mxMatrix for a family group (internal)

Used by the static branches of
[`buildOneFamilyGroup`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md).
The temporal branch instead builds its mean as an algebra over `Tpoly`
and `ModelOne.B_mean`, so that the mean can vary across historical time.

## Usage

``` r
.pedigreeMeanMatrix(fsize, obs_ids, label)
```

## Arguments

- fsize:

  Family size.

- obs_ids:

  Character vector of individual IDs.

- label:

  The mean parameter's label.

## Value

An mxMatrix object.
