# Create a properly formatted parent row for the pedigree

Create a properly formatted parent row for the pedigree

## Usage

``` r
addParentRow(template_row, newID, sex, momID = NA, dadID = NA, famID = NA)
```

## Arguments

- template_row:

  A single row from ped, used as a template for column structure

- newID:

  The new parent's ID

- sex:

  The new parent's sex value (e.g., 0 for female, 1 for male, or
  "F"/"M")

- momID:

  The new parent's mother ID (default is NA)

- dadID:

  The new parent's father ID (default is NA)

- famID:

  The new parent's family ID (default is NA)

## Value

A single-row dataframe for the new parent
