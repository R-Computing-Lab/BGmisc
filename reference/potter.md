# Fictional pedigree data on a wizarding family

A dataset created for educational and illustrative use, containing a
fictional pedigree modeled after characters from the Harry Potter
series. This data is structured for use in software demonstrations
involving pedigree diagrams, inheritance structures, and kinship
modeling. This dataset is not intended to represent any real individuals
or families. It includes no narrative content or protected expression
from the original works and is provided solely for educational purposes.
This dataset is not endorsed by or affiliated with the creators or
copyright holders of the Harry Potter series.

## Usage

``` r
data(potter)
```

## Format

A data frame (and ped object) with 36 rows and 10 variables

## Details

The variables are as follows:

- `personID`: Person identification variable

- `famID`: Family identification variable

- `name`: Name of the person

- `first_name`: First name of the person

- `surname`: Last name of the person

- `gen`: Generation of the person

- `momID`: ID of the mother

- `dadID`: ID of the father

- `spouseID`: ID of the spouse

- `sex`: Sex of the ID: 1 is male; 0 is female

- `twinID`: ID of the twin, if applicable

- `zygosity`: Zygosity of the twin, if applicable. mz is monozygotic; dz
  is dizygotic

IDs in the 100s `momID`s and `dadID`s are for people not in the dataset.
