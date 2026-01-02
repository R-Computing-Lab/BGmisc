# Recodes Sex Variable in a Pedigree Dataframe

This function serves as is primarily used internally, by plotting
functions etc. It sets the \`repair\` flag to TRUE automatically and
forwards any additional parameters to \`checkSex\`.

## Usage

``` r
recodeSex(
  ped,
  verbose = FALSE,
  code_male = NULL,
  code_na = NULL,
  code_female = NULL,
  code_unknown = NULL,
  recode_male = "M",
  recode_female = "F",
  recode_unknown = "U",
  recode_na = NA_character_
)
```

## Arguments

- ped:

  A dataframe representing the pedigree data with a 'sex' column.

- verbose:

  A logical flag indicating whether to print progress and validation
  messages to the console.

- code_male:

  The current code used to represent males in the 'sex' column.

- code_na:

  The current value used for missing values.

- code_female:

  The current code used to represent females in the 'sex' column. If
  both are NULL, no recoding is performed.

- code_unknown:

  The current code used to represent unknown or ambiguous sex in the
  'sex' column. Can be NA to indicate that missing values should be
  treated as unknown. If NULL and both code_male and code_female are
  provided, values not matching either will be inferred as unknown.

- recode_male:

  The value to use for males. Default is "M"

- recode_female:

  The value to use for females. Default is "F"

- recode_unknown:

  The value to use for unknown values. Default is "U"

- recode_na:

  The value to use for missing values. Default is NA_character\_

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
