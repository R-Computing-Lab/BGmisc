# Repairs Sex Coding in a Pedigree Dataframe

This function serves as a wrapper around \`checkSex\` to specifically
handle the repair of the sex coding in a pedigree dataframe.

## Usage

``` r
repairSex(ped, verbose = FALSE, code_male = NULL, code_female = NULL)
```

## Arguments

- ped:

  A dataframe representing the pedigree data with a 'sex' column.

- verbose:

  A logical flag indicating whether to print progress and validation
  messages to the console.

- code_male:

  The current code used to represent males in the 'sex' column.

- code_female:

  The current code used to represent females in the 'sex' column. If
  both are NULL, no recoding is performed.

## Value

A modified version of the input data.frame `ped`, containing an
additional or modified 'sex_recode' column where the 'sex' values are
recoded according to `code_male`. NA values in the 'sex' column are
preserved.

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

## See also

[`checkSex`](https://r-computing-lab.github.io/BGmisc/reference/checkSex.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ped <- data.frame(ID = c(1, 2, 3), sex = c("M", "F", "M"))
repairSex(ped, code_male = "M", verbose = TRUE)
} # }
```
