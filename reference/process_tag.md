# Process a GEDCOM Tag

Extracts and assigns a value to a specified field in \`vars\` if the
pattern is present. Returns both the updated variable list and a flag
indicating whether the tag was matched.

## Usage

``` r
process_tag(
  tag,
  field_name,
  pattern_rows,
  line,
  vars,
  extractor = NULL,
  mode = "replace"
)
```

## Arguments

- tag:

  The GEDCOM tag (e.g., "SEX", "CAST", etc.).

- field_name:

  The name of the variable to assign to in \`vars\`.

- pattern_rows:

  Output from \`countPatternRows()\`.

- line:

  The GEDCOM line to parse.

- vars:

  The current list of variables to update.

## Value

A list with updated \`vars\` and a \`matched\` flag.
