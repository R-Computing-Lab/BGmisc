big_gedcom_content <- c(
  "0 HEAD",
  "1 GEDC",
  "2 VERS 5.5",
  "2 FORM LINEAGE-LINKED",
  "1 CHAR UTF-8",
  "1 LANG English",
  "0 @I1@ INDI",
  "1 NAME John /Doe/",
  "1 SEX M",
  "1 BIRT",
  "2 DATE 1 JAN 1900",
  "2 PLAC Someplace",
  "0 @I2@ INDI",
  "1 NAME Jane /Smith/",
  "1 SEX F",
  "1 BIRT",
  "2 DATE 2 FEB 1910",
  "2 PLAC Anotherplace",
  "1 NCHI 2",
  "0 @S829105961@ SOUR",
  "1 TITL U.S., Find a Grave® Index, 1600s-Current",
  "1 NAME record",
  "1 AUTH Ancestry.com",
  "1 PUBL Ancestry.com Operations, Inc.",
  "2 DATE 2012",
  "2 PLAC Lehi, UT, USA",
  "1 _APID 1,60525::0",
  "1 REPO @R706186613@"
)

johnjane_gedcom_content <- c(
  "0 @I1@ INDI",
  "1 NAME John /Doe/",
  "1 GIVN John",
  "1 SEX M",
  "0 @I2@ INDI",
  "1 NAME Jane /Smith/",
  "1 GIVN Jane",
  "1 SEX F"
)

JD_gedcom_content <- johnjane_gedcom_content[c(1, 2, 4)]

FAMC_gedcom_content <- c(
  JD_gedcom_content,
  "1 FAMC @F1@",
  "1 FAMS @F2@"
)

deat_gedcom_content <- c(
  JD_gedcom_content,
  "1 DEAT",
  "2 DATE 31 DEC 2000",
  "2 PLAC Lastplace",
  "2 CAUS Old age",
  "2 LATI 12.3456",
  "2 LONG -65.4321"
)


test_that("readGedcom reads and parses a GEDCOM file correctly", {
  # Create a temporary GEDCOM file for testing
  gedcom_content <- big_gedcom_content
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  # Call readGedcom
  df <- readGedcom(temp_file, verbose = TRUE, skinny = FALSE)
  # note to self, the code is not reading in the 2nd person. and is also not reading in the birth date and place
  # Check that the data frame has the expected structure
  expect_true("personID" %in% colnames(df))
  expect_true("name_given" %in% colnames(df))
  expect_true("name_surn" %in% colnames(df))
  expect_true("sex" %in% colnames(df))
  expect_true("birth_date" %in% colnames(df))
  expect_true("birth_place" %in% colnames(df))
  expect_true("attribute_children" %in% colnames(df))
  # Check the contents of the data frame
  expect_equal(nrow(df), 2)
  expect_equal(df$name_given[1], "John")
  expect_equal(df$name_surn[1], "Doe")
  expect_equal(df$name[1], "John Doe")
  expect_equal(df$sex[1], "M")
  expect_equal(df$birth_date[1], "1 JAN 1900")
  expect_equal(df$birth_place[1], "Someplace")
  expect_equal(df$attribute_children[1], NA_character_)
  expect_equal(df$name_given[2], "Jane")
  expect_equal(df$name_surn[2], "Smith")
  expect_equal(df$name[2], "Jane Smith")
  expect_equal(df$sex[2], "F")
  expect_equal(df$birth_date[2], "2 FEB 1910")
  expect_equal(df$birth_place[2], "Anotherplace")
  expect_equal(df$attribute_children[2], "2")
  expect_null(df$attribute_title)

  # Clean up temporary file
  unlink(temp_file)
})

test_that("readGedcom combines duplicate columns correctly", {
  # Create a temporary GEDCOM file for testing
  gedcom_content <- johnjane_gedcom_content
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  # Call readGedcom with combine_cols = TRUE
  df <- readGedcom(temp_file, verbose = TRUE, combine_cols = TRUE)

  # Check that the data frame has the expected structure
  expect_true("name_given" %in% colnames(df))
  expect_false("name_given_pieces" %in% colnames(df))

  # Check the contents of the data frame
  expect_equal(nrow(df), 2)
  expect_equal(df$name_given[1], "John")
  expect_equal(df$name_given[2], "Jane")

  # Clean up temporary file
  unlink(temp_file)
})

test_that("readGedcom removes empty columns correctly", {
  # Create a temporary GEDCOM file for testing
  gedcom_content <- JD_gedcom_content

  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  # Call readGedcom with remove_empty_cols = TRUE
  df <- readGedcom(temp_file, verbose = TRUE, remove_empty_cols = TRUE)

  # Check that empty columns are removed
  expect_false("birth_date" %in% colnames(df))
  expect_false("birth_place" %in% colnames(df))

  # Clean up temporary file
  unlink(temp_file)
})

test_that("readGedcom handles skinny option correctly", {
  # Create a temporary GEDCOM file for testing
  gedcom_content <- FAMC_gedcom_content
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  # Call readGedcom with skinny = TRUE
  df <- readGedcom(temp_file, verbose = TRUE, skinny = TRUE)

  # Check that FAMC and FAMS columns are removed
  expect_false("FAMC" %in% colnames(df))
  expect_false("FAMS" %in% colnames(df))

  # Clean up temporary file
  unlink(temp_file)
})

test_that("processParents adds momID and dadID correctly", {
  # Create a data frame for testing
  df_temp <- data.frame(
    personID = c("I1", "I2", "I3"),
    sex = c("M", "F", "M"),
    FAMS = c("@F1@", "@F1@", NA),
    FAMC = c(NA, NA, "@F1@"),
    stringsAsFactors = FALSE
  )

  # Call processParents
  df_temp <- processParents(df_temp, datasource = "gedcom")

  # Check the structure of the data frame
  expect_true("momID" %in% colnames(df_temp))
  expect_true("dadID" %in% colnames(df_temp))

  # Check the contents of the data frame
  expect_equal(df_temp$momID[1], NA_character_)
  expect_equal(df_temp$dadID[1], NA_character_)
  expect_equal(df_temp$momID[2], NA_character_)
  expect_equal(df_temp$dadID[2], NA_character_)
  expect_equal(df_temp$momID[3], "I2")
  expect_equal(df_temp$dadID[3], "I1")

  # Create a more complex data frame for testing
  df_temp <- data.frame(
    personID = c("I1", "I2", "I3", "I4", "I5"),
    sex = c("M", "F", "M", "F", "M"),
    FAMS = c("@F1@", "@F1@", "@F2@", "@F2@", "@F3@"),
    FAMC = c(NA, NA, "@F1@", "@F1@", "@F2@"),
    stringsAsFactors = FALSE
  )

  # Call processParents
  df_temp <- processParents(df_temp, datasource = "gedcom")

  # Check the contents of the data frame
  expect_equal(df_temp$momID[3], "I2")
  expect_equal(df_temp$dadID[3], "I1")
  expect_equal(df_temp$momID[4], "I2")
  expect_equal(df_temp$dadID[4], "I1")
  expect_equal(df_temp$momID[5], "I4")
  expect_equal(df_temp$dadID[5], "I3")
})

test_that("if file does not exist, readGedcom throws an error", {
  # Call readGedcom with a non-existent file
  expect_error(readGedcom("nonexistent.ged"))
})


test_that("readGedcom parses death event correctly", {
  # Test that a GEDCOM file with a death event is parsed correctly.
  gedcom_content <- deat_gedcom_content
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  df <- readGedcom(temp_file, verbose = TRUE, clean_names = FALSE)

  expect_true("death_date" %in% colnames(df))
  expect_true("death_place" %in% colnames(df))
  expect_true("death_caus" %in% colnames(df))
  expect_true("death_lat" %in% colnames(df))
  expect_true("death_long" %in% colnames(df))

  expect_equal(df$death_date[1], "31 DEC 2000")
  expect_equal(df$death_place[1], "Lastplace")
  expect_equal(df$death_caus[1], "Old age")
  expect_equal(df$death_lat[1], "12.3456")
  expect_equal(df$death_long[1], "-65.4321")
  df_leg <- .readGedcom.legacy(temp_file, verbose = TRUE)

  expect_true("death_date" %in% colnames(df_leg))
  expect_true("death_place" %in% colnames(df_leg))
  expect_true("death_caus" %in% colnames(df_leg))
  expect_true("death_lat" %in% colnames(df_leg))
  expect_true("death_long" %in% colnames(df_leg))

  expect_equal(df_leg$death_date[1], "31 DEC 2000")
  expect_equal(df_leg$death_place[1], "Lastplace")
  expect_equal(df_leg$death_caus[1], "Old age")
  expect_equal(df_leg$death_lat[1], "12.3456")
  expect_equal(df_leg$death_long[1], "-65.4321")

  row.names(df) <- NULL
  row.names(df_leg) <- NULL
  df_leg <- dplyr::rename(df_leg, personID = id)
  # Strip the gedcom_version attribute added by readGedcom before comparing
  attr(df, "gedcom_version") <- NULL
  expect_equal(df_leg, df)

  unlink(temp_file)
})

test_that("readGedcom handles incomplete individual records gracefully", {
  # Test that an individual record missing a NAME line is handled without error.
  gedcom_content <- c(
    "0 @I1@ INDI",
    "1 SEX M"
    # No NAME or BIRT information.
  )
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  df <- readGedcom(temp_file, verbose = TRUE)

  # Expect one record with missing name fields.
  expect_equal(nrow(df), 1)
  expect_true(is.null(df$name[1]))

  unlink(temp_file)
})

test_that("readGedcom returns NULL with warning when no individual records are found", {
  gedcom_content <- c(
    "0 HEAD",
    "1 GEDC",
    "2 VERS 5.5",
    "0 TRLR"
  )

  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  expect_warning(
    df <- readGedcom(temp_file),
    "No people found in file"
  )
  expect_null(df)

  unlink(temp_file)
})

test_that("readGedcom post_process = FALSE preserves raw columns and skips cleanup", {
  gedcom_content <- c(
    "0 @I1@ INDI",
    "1 NAME John /Doe/",
    "1 GIVN Johnny",
    "1 SURN Dough",
    "1 SEX M"
  )

  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  df <- readGedcom(temp_file, post_process = FALSE)

  expect_equal(nrow(df), 1)
  expect_true("name_given" %in% colnames(df))
  expect_true("name_given_pieces" %in% colnames(df))
  expect_true("name_surn" %in% colnames(df))
  expect_true("name_surn_pieces" %in% colnames(df))
  expect_true("birth_date" %in% colnames(df))
  expect_true("death_date" %in% colnames(df))
  expect_true("FAMC" %in% colnames(df))
  expect_true("FAMS" %in% colnames(df))

  expect_equal(df$name_given[1], "John")
  expect_equal(df$name_given_pieces[1], "Johnny")
  expect_equal(df$name_surn[1], "Doe")
  expect_equal(df$name_surn_pieces[1], "Dough")
  expect_equal(df$birth_date[1], NA_character_)
  expect_equal(df$death_date[1], NA_character_)

  unlink(temp_file)
})

test_that("readGedcom parses all supported name component tags", {
  gedcom_content <- c(
    "0 @I1@ INDI",
    "1 NAME Dr. John Quincy /Doe/ Jr.",
    "1 GIVN John Quincy",
    "1 NPFX Dr.",
    "1 NICK Jack",
    "1 SURN Doe",
    "1 NSFX Jr.",
    "1 _MARNM John Quincy /MarriedDoe/",
    "1 SEX M"
  )

  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  df <- readGedcom(
    temp_file,
    add_parents = FALSE,
    combine_cols = FALSE,
    remove_empty_cols = FALSE,
    skinny = FALSE
  )

  expect_equal(df$name_given[1], "Dr. John Quincy")
  expect_equal(df$name_surn[1], "Doe")
  expect_equal(df$name_given_pieces[1], "John Quincy")
  expect_equal(df$name_npfx[1], "Dr.")
  expect_equal(df$name_nick[1], "Jack")
  expect_equal(df$name_surn_pieces[1], "Doe")
  expect_equal(df$name_nsfx[1], "Jr.")
  expect_equal(df$name_marriedsurn[1], "John Quincy /MarriedDoe")

  unlink(temp_file)
})

test_that("readGedcom parses all supported attribute tags", {
  gedcom_content <- c(
    "0 @I1@ INDI",
    "1 NAME John /Doe/",
    "1 SEX M",
    "1 CAST Example caste",
    "1 DSCR Example description",
    "1 EDUC Example education",
    "1 IDNO Example ID",
    "1 NATI Example nationality",
    "1 NCHI 3",
    "1 NMR 2",
    "1 OCCU Example occupation",
    "1 PROP Example property",
    "1 RELI Example religion",
    "1 RESI Example residence",
    "1 SSN 123-45-6789",
    "1 TITL Example title"
  )

  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  df <- readGedcom(
    temp_file,
    add_parents = FALSE,
    combine_cols = FALSE,
    remove_empty_cols = FALSE,
    skinny = FALSE
  )

  expect_equal(df$attribute_caste[1], "Example caste")
  expect_equal(df$attribute_description[1], "Example description")
  expect_equal(df$attribute_education[1], "Example education")
  expect_equal(df$attribute_idnumber[1], "Example ID")
  expect_equal(df$attribute_nationality[1], "Example nationality")
  expect_equal(df$attribute_children[1], "3")
  expect_equal(df$attribute_marriages[1], "2")
  expect_equal(df$attribute_occupation[1], "Example occupation")
  expect_equal(df$attribute_property[1], "Example property")
  expect_equal(df$attribute_religion[1], "Example religion")
  expect_equal(df$attribute_residence[1], "Example residence")
  expect_equal(df$attribute_ssn[1], "123-45-6789")
  expect_equal(df$attribute_title[1], "Example title")

  unlink(temp_file)
})

test_that("readGedcom appends multiple FAMC and FAMS tags for one individual", {
  gedcom_content <- c(
    "0 @I1@ INDI",
    "1 NAME John /Doe/",
    "1 SEX M",
    "1 FAMC @F1@",
    "1 FAMC @F2@",
    "1 FAMS @F3@",
    "1 FAMS @F4@"
  )

  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  df <- readGedcom(
    temp_file,
    post_process = FALSE
  )

  expect_equal(df$FAMC[1], "1, 2")
  expect_equal(df$FAMS[1], "3, 4")

  unlink(temp_file)
})

test_that("readGedcom infers parents end-to-end from GEDCOM FAMC and FAMS tags", {
  gedcom_content <- c(
    "0 @I1@ INDI",
    "1 NAME John /Doe/",
    "1 SEX M",
    "1 FAMS @F1@",
    "0 @I2@ INDI",
    "1 NAME Jane /Smith/",
    "1 SEX F",
    "1 FAMS @F1@",
    "0 @I3@ INDI",
    "1 NAME Child /Doe/",
    "1 SEX F",
    "1 FAMC @F1@"
  )

  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  df <- readGedcom(
    temp_file,
    add_parents = TRUE,
    combine_cols = TRUE,
    remove_empty_cols = FALSE,
    skinny = FALSE
  )

  child_row <- which(df$personID == "3")

  expect_equal(df$dadID[child_row], "1")
  expect_equal(df$momID[child_row], "2")

  unlink(temp_file)
})

test_that("readGedcom parse_dates = TRUE parses birth and death dates", {
  gedcom_content <- c(
    "0 @I1@ INDI",
    "1 NAME John /Doe/",
    "1 SEX M",
    "1 BIRT",
    "2 DATE ABT 1 JAN 1900",
    "2 PLAC Someplace",
    "1 DEAT",
    "2 DATE BEF 31 DEC 2000",
    "2 PLAC Lastplace",
    "2 CAUS Old age"
  )

  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  df <- readGedcom(
    temp_file,
    parse_dates = TRUE,
    add_parents = FALSE,
    combine_cols = TRUE,
    remove_empty_cols = FALSE,
    skinny = FALSE
  )

  expect_s3_class(df$birth_date, "Date")
  expect_s3_class(df$death_date, "Date")
  expect_equal(df$birth_date[1], as.Date("1900-01-01"))
  expect_equal(df$death_date[1], as.Date("2000-12-31"))

  unlink(temp_file)
})

test_that("postProcessGedcom parse_dates = TRUE parses date columns and removes GEDCOM date qualifiers", {
  df_temp <- data.frame(
    personID = "1",
    sex = "M",
    name = "John Doe/",
    name_given = "John",
    name_given_pieces = NA_character_,
    name_surn = "Doe",
    name_surn_pieces = NA_character_,
    birth_date = "ABT 1 JAN 1900",
    death_date = "AFT 31 DEC 2000",
    FAMC = NA_character_,
    FAMS = NA_character_,
    stringsAsFactors = FALSE
  )

  df <- postProcessGedcom(
    df_temp,
    add_parents = FALSE,
    combine_cols = FALSE,
    remove_empty_cols = FALSE,
    parse_dates = TRUE,
    skinny = FALSE
  )

  expect_s3_class(df$birth_date, "Date")
  expect_s3_class(df$death_date, "Date")
  expect_equal(df$birth_date[1], as.Date("1900-01-01"))
  expect_equal(df$death_date[1], as.Date("2000-12-31"))
})

test_that("processParents warns and returns unchanged data when required GEDCOM columns are missing", {
  df_temp <- data.frame(
    personID = c("I1", "I2"),
    sex = c("M", "F"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    out <- processParents(df_temp, datasource = "gedcom"),
    "Missing necessary columns"
  )

  expect_equal(out, df_temp)
})

test_that("processParents rejects invalid datasource values", {
  df_temp <- data.frame(
    personID = "I1",
    sex = "M",
    FAMC = NA_character_,
    FAMS = NA_character_,
    stringsAsFactors = FALSE
  )

  expect_error(
    processParents(df_temp, datasource = "unknown"),
    "Invalid datasource"
  )
})

test_that("mapFAMS2parents warns and returns NULL when required columns are missing", {
  df_temp <- data.frame(
    personID = c("I1", "I2"),
    FAMS = c("F1", "F1"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    out <- mapFAMS2parents(df_temp),
    "necessary columns"
  )

  expect_null(out)
})

birt_no_date_content <- c(
  "0 @I1@ INDI",
  "1 NAME Alice /Jones/",
  "1 SEX F",
  "1 BIRT",
  "2 PLAC Springfield",
  "2 LATI N39.7817",
  "2 LONG W89.6501"
)

birt_no_plac_content <- c(
  "0 @I1@ INDI",
  "1 NAME Bob /Smith/",
  "1 SEX M",
  "1 BIRT",
  "2 DATE 15 MAR 1955"
)

birt_reordered_content <- c(
  "0 @I1@ INDI",
  "1 NAME Carol /Lee/",
  "1 SEX F",
  "1 BIRT",
  "2 LATI N40.7128",
  "2 LONG W74.0060",
  "2 PLAC New York",
  "2 DATE 4 JUL 1976"
)

test_that("processEventLine handles missing DATE gracefully", {
  temp_file <- tempfile(fileext = ".ged")
  writeLines(birt_no_date_content, temp_file)
  df <- readGedcom(temp_file, remove_empty_cols = FALSE)
  expect_true(is.na(df$birth_date[1]))
  expect_equal(df$birth_place[1], "Springfield")
  expect_equal(df$birth_lat[1], "N39.7817")
  expect_equal(df$birth_long[1], "W89.6501")
  unlink(temp_file)
})

test_that("processEventLine handles missing PLAC gracefully", {
  temp_file <- tempfile(fileext = ".ged")
  writeLines(birt_no_plac_content, temp_file)
  df <- readGedcom(temp_file, remove_empty_cols = FALSE)
  expect_equal(df$birth_date[1], "15 MAR 1955")
  expect_true(is.na(df$birth_place[1]))
  unlink(temp_file)
})

test_that("processEventLine handles reordered subfields correctly", {
  temp_file <- tempfile(fileext = ".ged")
  writeLines(birt_reordered_content, temp_file)
  df <- readGedcom(temp_file)
  expect_equal(df$birth_date[1], "4 JUL 1976")
  expect_equal(df$birth_place[1], "New York")
  expect_equal(df$birth_lat[1], "N40.7128")
  expect_equal(df$birth_long[1], "W74.0060")
  unlink(temp_file)
})

gedcom55_header <- c(
  "0 HEAD",
  "1 GEDC",
  "2 VERS 5.5.1",
  "2 FORM LINEAGE-LINKED",
  "1 CHAR UTF-8",
  "0 @I1@ INDI",
  "1 NAME Test /Person/",
  "1 SEX M"
)

gedcom7_header <- c(
  "0 HEAD",
  "1 GEDC",
  "2 VERS 7.0",
  "1 CHAR UTF-8",
  "0 @I1@ INDI",
  "1 NAME Test /Person/",
  "1 SEX M"
)

test_that("detectGedcomVersion returns correct version string", {
  expect_equal(BGmisc:::detectGedcomVersion(gedcom55_header), "5.5.1")
  expect_equal(BGmisc:::detectGedcomVersion(gedcom7_header), "7.0")
  expect_equal(BGmisc:::detectGedcomVersion(c("0 @I1@ INDI", "1 NAME No /Head/")), "unknown")
})

test_that("readGedcom attaches gedcom_version attribute", {
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom55_header, temp_file)
  df <- readGedcom(temp_file)
  expect_equal(attr(df, "gedcom_version"), "5.5.1")
  unlink(temp_file)
})

test_that("detectGedcomVersion returns unknown when GEDC is present but VERS is missing", {
  lines <- c(
    "0 HEAD",
    "1 GEDC",
    "1 CHAR UTF-8",
    "0 @I1@ INDI",
    "1 NAME Test /Person/",
    "1 SEX M"
  )
  expect_equal(BGmisc:::detectGedcomVersion(lines), "unknown")
})

test_that("readGedcom attaches gedcom_version attribute with post_process = FALSE", {
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom55_header, temp_file)
  df <- readGedcom(temp_file, post_process = FALSE)
  expect_equal(attr(df, "gedcom_version"), "5.5.1")
  unlink(temp_file)
})

test_that("readGed and readgedcom aliases return the same output as readGedcom", {
  gedcom_content <- c(
    "0 @I1@ INDI",
    "1 NAME John /Doe/",
    "1 SEX M"
  )

  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  df_main <- readGedcom(temp_file)
  df_readGed <- readGed(temp_file)
  df_readgedcom <- readgedcom(temp_file)

  row.names(df_main) <- NULL
  row.names(df_readGed) <- NULL
  row.names(df_readgedcom) <- NULL

  expect_equal(df_readGed, df_main)
  expect_equal(df_readgedcom, df_main)

  unlink(temp_file)
})

gedcom7_map_content <- c(
  "0 HEAD",
  "1 GEDC",
  "2 VERS 7.0",
  "0 @I1@ INDI",
  "1 NAME Test /Person/",
  "1 SEX F",
  "1 BIRT",
  "2 DATE 1 JAN 2000",
  "2 PLAC London, England",
  "3 MAP",
  "4 LATI N51.5074",
  "4 LONG W0.1278",
  "1 DEAT",
  "2 DATE 31 DEC 2080",
  "2 PLAC Edinburgh, Scotland",
  "3 MAP",
  "4 LATI N55.9533",
  "4 LONG W3.1883"
)

gedcom_calendar_escape_content <- c(
  "0 HEAD",
  "1 GEDC",
  "2 VERS 5.5",
  "0 @I1@ INDI",
  "1 NAME Old /Style/",
  "1 SEX M",
  "1 BIRT",
  "2 DATE @#DGREGORIAN@ 15 JUL 1823"
)

test_that("readGedcom parses GEDCOM 7.x MAP coordinate structure", {
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom7_map_content, temp_file)
  df <- readGedcom(temp_file)
  expect_equal(attr(df, "gedcom_version"), "7.0")
  expect_equal(df$birth_lat[1], "N51.5074")
  expect_equal(df$birth_long[1], "W0.1278")
  expect_equal(df$death_lat[1], "N55.9533")
  expect_equal(df$death_long[1], "W3.1883")
  unlink(temp_file)
})

test_that("parse_dates strips GEDCOM 5.5 calendar escape before parsing", {
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_calendar_escape_content, temp_file)
  df <- readGedcom(temp_file, parse_dates = TRUE)
  expect_false(is.na(df$birth_date[1]))
  expect_equal(format(df$birth_date[1], "%d %b %Y"), "15 Jul 1823")
  unlink(temp_file)
})

test_that("gedcomLatToNumeric converts N/S notation correctly", {
  expect_equal(gedcomLatToNumeric("N51.5074"), 51.5074)
  expect_equal(gedcomLatToNumeric("S33.8688"), -33.8688)
  expect_true(is.na(gedcomLatToNumeric(NA_character_)))
})

test_that("gedcomLonToNumeric converts E/W notation correctly", {
  expect_equal(gedcomLonToNumeric("E151.2093"), 151.2093)
  expect_equal(gedcomLonToNumeric("W0.1278"), -0.1278)
  expect_true(is.na(gedcomLonToNumeric(NA_character_)))
})

test_that("gedcomLatToNumeric returns NA for unrecognised prefix", {
  expect_true(is.na(gedcomLatToNumeric("12.34")))
  expect_true(is.na(gedcomLatToNumeric("")))
})

test_that("gedcomLonToNumeric returns NA for unrecognised prefix", {
  expect_true(is.na(gedcomLonToNumeric("12.34")))
  expect_true(is.na(gedcomLonToNumeric("")))
})
