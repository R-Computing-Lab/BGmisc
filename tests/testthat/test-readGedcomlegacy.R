test_that("readGedcom parses death event correctly for legacy", {
  # Test that a GEDCOM file with a death event is parsed correctly.
  gedcom_content <- c(
    "0 @I1@ INDI",
    "1 NAME John /Doe/",
    "1 SEX M",
    "1 DEAT",
    "2 DATE 31 DEC 2000",
    "2 PLAC Lastplace",
    "2 CAUS Old age",
    "2 LATI 12.3456",
    "2 LONG -65.4321"
  )
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  df <- readGedcom(temp_file, verbose = TRUE)
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
  expect_equal(df_leg, df)

  unlink(temp_file)
})
