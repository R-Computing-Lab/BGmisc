# Royal pedigree data from 1992

A dataset created by Denis Reid from the Royal Families of Europe. The
data was originally published in 1992 and is available on the internet.
This version has been updated to combine duplicate entries and to
include additional information on birth and death dates, as well as
titles. This dataset is intended for educational and illustrative use in
software demonstrations involving pedigree diagrams, inheritance
structures, and kinship modeling. This dataset is not intended to
represent any real individuals or families beyond the original source
data, and it is provided solely for educational purposes.

## Usage

``` r
data(royal92)
```

## Format

A data frame with 3110 observations

## Details

The variables are as follows:

- `personID`: Person identification variable

- `momID`: ID of the mother

- `dadID`: ID of the father

- `famID`: ID of the extended family

- `twinID`: ID of the twin, if applicable

- `name`: Name of the person

- `sex`: Biological sex

- `birth_date`: Date of birth

- `death_date`: Date of death

- `title`: Title of the person
