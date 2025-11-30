# Function to prepare the pedigree for summarization This function prepares the pedigree for summarization by ensuring that the necessary IDs are present and that the pedigree is built correctly.

Function to prepare the pedigree for summarization This function
prepares the pedigree for summarization by ensuring that the necessary
IDs are present and that the pedigree is built correctly.

## Usage

``` r
prepSummarizePedigrees(
  ped,
  type,
  verbose = FALSE,
  famID,
  personID,
  momID,
  dadID,
  matID,
  patID
)
```

## Arguments

- ped:

  a pedigree dataset. Needs ID, momID, and dadID columns

- type:

  Character vector. Specifies which summaries to compute. Options:
  \`"fathers"\`, \`"mothers"\`, \`"families"\`. Default includes all
  three.

- verbose:

  Logical, if TRUE, print progress messages.

- famID:

  character. Name of the column to be created in ped for the family ID
  variable

- personID:

  character. Name of the column in ped for the person ID variable

- momID:

  character. Name of the column in ped for the mother ID variable

- dadID:

  character. Name of the column in ped for the father ID variable

- matID:

  Character. Maternal line ID variable to be created and added to the
  pedigree

- patID:

  Character. Paternal line ID variable to be created and added to the
  pedigree
