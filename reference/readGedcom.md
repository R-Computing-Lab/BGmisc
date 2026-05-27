# Read a GEDCOM File

Ingests a GEDCOM genealogy file, identifies individual records, and
parses person-level identifiers, names, life events, attributes, and
family relationships into a structured data frame. Optional
post-processing can infer parental IDs from family relationships,
reconcile redundant name fields, and remove uninformative columns from
the parsed output.

## Usage

``` r
readGedcom(
  file_path,
  verbose = FALSE,
  add_parents = TRUE,
  remove_empty_cols = TRUE,
  combine_cols = TRUE,
  skinny = FALSE,
  update_rate = 1000,
  post_process = TRUE,
  ...
)

readGed(
  file_path,
  verbose = FALSE,
  add_parents = TRUE,
  remove_empty_cols = TRUE,
  combine_cols = TRUE,
  skinny = FALSE,
  update_rate = 1000,
  post_process = TRUE,
  ...
)

readgedcom(
  file_path,
  verbose = FALSE,
  add_parents = TRUE,
  remove_empty_cols = TRUE,
  combine_cols = TRUE,
  skinny = FALSE,
  update_rate = 1000,
  post_process = TRUE,
  ...
)
```

## Arguments

- file_path:

  Character string. Path to the GEDCOM file.

- verbose:

  Logical. If \`TRUE\`, print progress messages.

- add_parents:

  Logical. If \`TRUE\`, infer \`momID\` and \`dadID\` from \`FAMC\` and
  \`FAMS\` mappings during post-processing.

- remove_empty_cols:

  Logical. If \`TRUE\`, drop columns that are entirely \`NA\` during
  post-processing.

- combine_cols:

  Logical. If \`TRUE\`, combine redundant name columns, such as
  \`name_given\` with \`name_given_pieces\` and \`name_surn\` with
  \`name_surn_pieces\`, when their values do not conflict.

- skinny:

  Logical. If \`TRUE\`, return a slimmer data frame by dropping
  \`FAMC\`, \`FAMS\`, and columns that are entirely \`NA\` during
  post-processing.

- update_rate:

  Numeric. Intended rate at which progress messages should be printed.
  Currently unused.

- post_process:

  Logical. If \`TRUE\`, apply post-processing steps controlled by
  \`add_parents\`, \`combine_cols\`, \`remove_empty_cols\`, and
  \`skinny\`.

- ...:

  Additional arguments. Currently unused.

## Value

A data frame containing information about individuals, with the
following potential columns:

- personID:

  Individual ID parsed from the \`@ INDI\` line.

- momID:

  ID of the individual's mother, if inferred.

- dadID:

  ID of the individual's father, if inferred.

- sex:

  Sex of the individual.

- name:

  Cleaned full name of the individual.

- name_given:

  Given name parsed from the \`NAME\` tag.

- name_given_pieces:

  Given name parsed from a separate \`GIVN\` tag, if present.

- name_surn:

  Surname parsed from the \`NAME\` tag.

- name_surn_pieces:

  Surname parsed from a separate \`SURN\` tag, if present.

- name_marriedsurn:

  Married surname parsed from \`\_MARNM\`, if present.

- name_nick:

  Nickname parsed from \`NICK\`, if present.

- name_npfx:

  Name prefix parsed from \`NPFX\`, if present.

- name_nsfx:

  Name suffix parsed from \`NSFX\`, if present.

- birth_date:

  Birth date of the individual.

- birth_lat:

  Latitude of the birthplace.

- birth_long:

  Longitude of the birthplace.

- birth_place:

  Birthplace of the individual.

- death_caus:

  Cause of death.

- death_date:

  Death date of the individual.

- death_lat:

  Latitude of the place of death.

- death_long:

  Longitude of the place of death.

- death_place:

  Place of death of the individual.

- attribute_caste:

  Caste of the individual.

- attribute_children:

  Number of children of the individual.

- attribute_description:

  Description of the individual.

- attribute_education:

  Education of the individual.

- attribute_idnumber:

  Identification number of the individual.

- attribute_marriages:

  Number of marriages of the individual.

- attribute_nationality:

  Nationality of the individual.

- attribute_occupation:

  Occupation of the individual.

- attribute_property:

  Property owned by the individual.

- attribute_religion:

  Religion of the individual.

- attribute_residence:

  Residence of the individual.

- attribute_ssn:

  Social Security number of the individual.

- attribute_title:

  Title of the individual.

- FAMC:

  ID or IDs of the family in which the individual is a child.

- FAMS:

  ID or IDs of families in which the individual is a spouse.

If no individual records are found, the function returns \`NULL\` with a
warning.

## Details

\`readGedcom()\` is a line-oriented parser tuned to common GEDCOM 5.5
and 5.5.1 structures. Individual records are identified from blocks that
begin with an \`@ INDI\` line. Each individual block is passed to an
internal parser that uses simple GEDCOM tag pattern matches to extract
identifiers, names, life events, attributes, and family relationships.

Name information is parsed primarily from the GEDCOM \`NAME\` tag, which
often encodes given names and surnames using slash-delimited surname
notation, such as \`NAME John /Smith/\`. The parser extracts the given
name, surname, and a cleaned full name. Additional name components are
parsed when present, including name prefix, name suffix, nickname, and
married surname.

Birth and death events are recognized from \`BIRT\` and \`DEAT\` tags.
Event details are currently parsed using fixed offsets within the
individual block. For birth events, the parser expects \`DATE\` at \`i +
1\`, \`PLAC\` at \`i + 2\`, \`LATI\` at \`i + 4\`, and \`LONG\` at \`i +
5\`. For death events, the parser expects \`DATE\` at \`i + 1\`,
\`PLAC\` at \`i + 2\`, \`CAUS\` at \`i + 3\`, \`LATI\` at \`i + 4\`, and
\`LONG\` at \`i + 5\`. Missing elements leave the corresponding output
fields as \`NA\`.

Attribute tags such as \`OCCU\`, \`EDUC\`, \`RELI\`, \`CAST\`, \`NCHI\`,
\`NMR\`, \`NATI\`, \`RESI\`, \`PROP\`, \`SSN\`, \`TITL\`, \`DSCR\`, and
\`IDNO\` are parsed directly into dedicated columns prefixed with
\`attribute\_\`.

Family relationships are parsed from \`FAMC\` and \`FAMS\` tags.
\`FAMC\` identifies the family in which an individual is a child, and
\`FAMS\` identifies families in which an individual is a spouse. These
raw family identifiers are retained in the parsed output unless removed
during post-processing. When \`add_parents = TRUE\`, they are also used
to infer \`momID\` and \`dadID\`.

If \`post_process = TRUE\`, \`readGedcom()\` applies optional cleanup
steps controlled by \`add_parents\`, \`combine_cols\`,
\`remove_empty_cols\`, and \`skinny\`. These steps can infer parent IDs,
collapse redundant name fields, remove columns that are entirely
missing, and drop raw family relationship columns for a slimmer output.
