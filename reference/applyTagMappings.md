# Apply Tag Mappings to a Line

Iterates over a list of tag mappings and, if a tag matches the line,
updates the record. Stops after the first match.

## Usage

``` r
applyTagMappings(line, record, pattern_rows, tag_mappings)
```

## Arguments

- line:

  A character string from the GEDCOM file.

- record:

  A named list representing the individual's record.

- pattern_rows:

  A list with GEDCOM tag counts.

- tag_mappings:

  A list of lists. Each sublist should define: - `tag`: the GEDCOM
  tag, - `field`: the record field to update, - `mode`: either "replace"
  or "append", - `extractor`: (optional) a custom extraction function.

## Value

A list with the updated record (`record`) and a logical flag
(`matched`).
