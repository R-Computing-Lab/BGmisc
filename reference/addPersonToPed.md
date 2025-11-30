# addPersonToPed

A function to add a new person to an existing pedigree `data.frame`.

## Usage

``` r
addPersonToPed(
  ped,
  name = NULL,
  sex = NULL,
  momID = NA,
  dadID = NA,
  twinID = NULL,
  personID = NULL,
  zygosity = NULL,
  notes = NULL,
  url = NULL,
  overwrite = FALSE
)
```

## Arguments

- ped:

  A `data.frame` representing the existing pedigree.

- name:

  Optional. A character string representing the name of the new person.
  If not provided, the name will be set to `NA`.

- sex:

  A value representing the sex of the new person.

- momID:

  Optional. The ID of the mother of the new person. If not provided, it
  will be set to `NA`.

- dadID:

  Optional. The ID of the father of the new person. If not provided, it
  will be set to `NA`.

- twinID:

  Optional. The ID of the twin of the new person. If not provided, it
  will be set to `NA`.

- personID:

  Optional. The ID of the new person. If not provided, it will be
  generated as the maximum existing personID + 1.

- zygosity:

  Optional. A character string indicating the zygosity of the new
  person. If not provided, it will be set to `NA`.

- notes:

  Optional. A character string for notes about the new person. If not
  provided, it will be set to `NA`.

- url:

  Optional. A URL column for the new person. If not provided, it will be
  set to `NA`.

- overwrite:

  Logical. If `TRUE`, the function will overwrite an existing person
  with the same `personID`. If `FALSE`, it will stop if a person with
  the same `personID` already exists.

## Value

A `data.frame` with the new person added to the existing pedigree.
