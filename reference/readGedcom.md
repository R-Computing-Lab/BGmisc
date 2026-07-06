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
  post_process = TRUE,
  add_parents = TRUE,
  remove_empty_cols = TRUE,
  combine_cols = TRUE,
  skinny = FALSE,
  parse_dates = FALSE,
  clean_names = TRUE,
  update_rate = 1000,
  ...
)

readGed(
  file_path,
  verbose = FALSE,
  post_process = TRUE,
  add_parents = TRUE,
  remove_empty_cols = TRUE,
  combine_cols = TRUE,
  skinny = FALSE,
  parse_dates = FALSE,
  clean_names = TRUE,
  update_rate = 1000,
  ...
)

readgedcom(
  file_path,
  verbose = FALSE,
  post_process = TRUE,
  add_parents = TRUE,
  remove_empty_cols = TRUE,
  combine_cols = TRUE,
  skinny = FALSE,
  parse_dates = FALSE,
  clean_names = TRUE,
  update_rate = 1000,
  ...
)
```

## Arguments

- file_path:

  Character string. Path to the GEDCOM file.

- verbose:

  Logical. If \`TRUE\`, print progress messages.

- post_process:

  Logical. If \`TRUE\`, apply post-processing steps controlled by
  \`add_parents\`, \`combine_cols\`, \`remove_empty_cols\`, \`skinny\`,
  and \`parse_dates\`.

- add_parents:

  Logical. If \`TRUE\`, infer \`momID\` and \`dadID\` from \`FAMC\` and
  \`FAMS\` mappings during post-processing.

- remove_empty_cols:

  Logical indicating whether to remove columns that are entirely
  missing.

- combine_cols:

  Logical. If \`TRUE\`, combine redundant name columns, such as
  \`name_given\` with \`name_given_pieces\` and \`name_surn\` with
  \`name_surn_pieces\`, when their values do not conflict.

- skinny:

  Logical. If \`TRUE\`, return a slimmer data frame by dropping
  \`FAMC\`, \`FAMS\`, and columns that are entirely \`NA\` during
  post-processing.

- parse_dates:

  Logical. If \`TRUE\`, attempt to parse date columns (e.g.,
  \`birth_date\`, \`death_date\`) into Date objects, after removing
  common GEDCOM date qualifiers like "ABT", "BEF", and "AFT".

- clean_names:

  Logical indicating whether to clean name columns by removing trailing
  slashes and squishing whitespace.

- update_rate:

  Numeric. Intended rate at which progress messages should be printed.
  Currently unused.

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
Event details are parsed by collecting all child lines whose GEDCOM
level equals the event level plus one (direct children), then looking up
sub-fields by tag name. \`DATE\`, \`PLAC\`, and \`CAUS\` are matched as
direct children of the event. Coordinates (\`LATI\` and \`LONG\`) are
searched across all descendant lines, which allows them to be located
whether they appear as direct children (common in some GEDCOM 5.5.x
exporters), under \`PLAC\` (standard GEDCOM 5.5.1), or under a \`MAP\`
substructure under \`PLAC\` (GEDCOM 7.x). Missing sub-fields leave the
corresponding output columns as \`NA\`.

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
