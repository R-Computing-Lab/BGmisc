# Process Event Lines (Birth or Death)

Extracts event details (e.g., date, place, cause, latitude, longitude)
from a block of GEDCOM lines. Uses level-aware sub-block parsing so
fields are looked up by tag name rather than fixed offsets.

## Usage

``` r
processEventLine(event, block, i, record, pattern_rows)
```

## Arguments

- event:

  A character string indicating the event type ("birth" or "death").

- block:

  A character vector of GEDCOM lines.

- i:

  The current line index where the event tag is found.

- record:

  A named list representing the individual's record.

- pattern_rows:

  A list with counts of GEDCOM tag occurrences.

## Value

The updated record with parsed event information.
