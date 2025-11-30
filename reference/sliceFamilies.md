# sliceFamilies

Slices up families by additive relatedness, creating CSV files grouped
by degree of relatedness. Operates on a potentially large file by
reading in chunks and binning links by additive relatedness.

## Usage

``` r
sliceFamilies(
  outcome_name = "AD_demo",
  biggest = TRUE,
  bin_width = 0.1,
  degreerelatedness = 12,
  chunk_size = 2e+07,
  max_lines = 1e+13,
  addRel_ceiling = 1.5,
  input_file = NULL,
  folder_prefix = "data",
  progress_csv = "progress.csv",
  progress_status = "progress.txt",
  data_directory = NULL,
  verbose = FALSE,
  error_handling = FALSE,
  file_column_names = c("ID1", "ID2", "addRel", "mitRel", "cnuRel")
)
```

## Arguments

- outcome_name:

  Name of the outcome variable (used for naming input/output files)

- biggest:

  Logical; whether to process the "biggest" family dataset (TRUE) or
  all-but-biggest (FALSE)

- bin_width:

  Width of additive relatedness bins (default is 0.10)

- degreerelatedness:

  Maximum degree of relatedness to consider (default 12)

- chunk_size:

  Number of lines to read in each chunk (default 2e7)

- max_lines:

  Max number of lines to process from input file (default 1e13)

- addRel_ceiling:

  Numeric. Maximum relatedness value to bin to. Default is 1.5

- input_file:

  Path to the input CSV file. If NULL, defaults to a specific file based
  on \`biggest\` flag.

- folder_prefix:

  Prefix for the output folder (default "data")

- progress_csv:

  Path to a CSV file for tracking progress (default "progress.csv")

- progress_status:

  Path to a text file for logging progress status (default
  "progress.txt")

- data_directory:

  Directory where output files will be saved. If NULL, it is constructed
  based on \`outcome_name\` and \`folder_prefix\`.

- verbose:

  Logical; whether to print progress messages (default FALSE)

- error_handling:

  Logical. Should more aggressive error handing be attemptted? Default
  is false

- file_column_names:

  Names of the columns in the input file (default c("ID1", "ID2",
  "addRel", "mitRel", "cnuRel"))

## Value

NULL. Writes CSV files to disk and updates progress logs.
