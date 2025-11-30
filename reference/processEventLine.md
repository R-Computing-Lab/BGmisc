# Process Event Lines (Birth or Death)

Extracts event details (e.g., date, place, cause, latitude, longitude)
from a block of GEDCOM lines. For "birth": expect DATE on line i+1, PLAC
on i+2, LATI on i+4, LONG on i+5. For "death": expect DATE on line i+1,
PLAC on i+2, CAUS on i+3, LATI on i+4, LONG on i+5.

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

The updated record with parsed event information.#
