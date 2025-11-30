# Simulated pedigree with two extended families and an age-related hazard

A dataset simulated to have an age-related hazard. There are two
extended families that are sampled from the same population.

## Usage

``` r
data(hazard)
```

## Format

A data frame with 43 rows and 14 variables

## Details

The variables are as follows:

- `FamID`: ID of the extended family

- `ID`: Person identification variable

- `sex`: Sex of the ID: 1 is female; 0 is male

- `dadID`: ID of the father

- `momID`: ID of the mother

- `affected`: logical. Whether the person is affected or not

- `DA1`: Binary variable signifying the meaninglessness of life

- `DA2`: Binary variable signifying the fundamental unknowability of
  existence

- `birthYr`: Birth year for person

- `onsetYr`: Year of onset for person

- `deathYr`: Death year for person

- `available`: logical. Whether

- `Gen`: Generation of the person

- `proband`: logical. Whether the person is a proband or not
