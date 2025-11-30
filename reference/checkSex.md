# Validates and Optionally Repairs Sex Coding in a Pedigree Dataframe

This function checks and optionally modifies the coding of the
biological 'sex' variable in a pedigree dataset. It serves two primary
purposes: 1. Recodes the 'sex' variable based on specified codes for
males and females, if provided. 2. Identifies and optionally repairs
inconsistencies in sex coding that could break the algorithm for
constructing genetic pedigrees.

## Usage

``` r
checkSex(
  ped,
  code_male = NULL,
  code_female = NULL,
  verbose = FALSE,
  repair = FALSE,
  momID = "momID",
  dadID = "dadID"
)
```

## Arguments

- ped:

  A dataframe representing the pedigree data with a 'sex' column.

- code_male:

  The current code used to represent males in the 'sex' column.

- code_female:

  The current code used to represent females in the 'sex' column. If
  both are NULL, no recoding is performed.

- verbose:

  A logical flag indicating whether to print progress and validation
  messages to the console.

- repair:

  A logical flag indicating whether to attempt repairs on the sex
  coding.

- momID:

  The column name for maternal IDs. Default is "momID".

- dadID:

  The column name for paternal IDs. Default is "dadID".

## Value

Depending on the value of \`repair\`, either a list containing
validation results or a repaired dataframe is returned.

## Details

The validation process identifies: - The unique sex codes present in the
dataset. - Whether individuals listed as fathers or mothers have
inconsistent sex codes. - Instances where an individual's recorded sex
does not align with their parental role.

If \`repair = TRUE\`, the function standardizes sex coding by: -
Assigning individuals listed as fathers the most common male code in the
dataset. - Assigning individuals listed as mothers the most common
female code.

This function uses the terms 'male' and 'female' in a biological
context, referring to chromosomal and other biologically-based
characteristics necessary for constructing genetic pedigrees. The
biological aspect of sex used in genetic analysis (genotype) is distinct
from the broader, richer concept of gender identity (phenotype).

We recognize the importance of using language and methodologies that
affirm and respect the full spectrum of gender identities. The
developers of this package express unequivocal support for folx in the
transgender and LGBTQ+ communities.

## Examples

``` r
if (FALSE) { # \dontrun{
ped <- data.frame(ID = c(1, 2, 3), sex = c("M", "F", "M"))
checkSex(ped, code_male = "M", verbose = TRUE, repair = FALSE)
} # }
```
