test_that("standardizeColnames works", {
  library(BGmisc)
  library(dplyr)
  data(potter)
  # Test with a sample DataFrame
  expect_message(standardizeColnames(potter, verbose = TRUE),
                 "Standardizing column names...")

  standardized_df <- standardizeColnames(potter)
  expect_false("spouseID" %in% names(standardized_df))
  expect_true("spID" %in% names(standardized_df))

  # Check if the column names are standardized
data_fixable_names <- potter %>% dplyr::rename(familyID = famID,
                                        dame = momID,
                                        dadid = dadID)

renamed_fixed <- standardizeColnames(data_fixable_names)
expect_false("spouseID" %in% names(renamed_fixed))
expect_true("spID" %in% names(renamed_fixed))
expect_false("familyID" %in% names(renamed_fixed))
expect_true("famID" %in% names(renamed_fixed))
expect_false("dame" %in% names(renamed_fixed))
expect_true("momID" %in% names(renamed_fixed))
expect_false("dadid" %in% names(renamed_fixed))
expect_true("dadID" %in% names(renamed_fixed))

data_unfixable_names <- potter %>% dplyr::rename(zfamID = famID,
                                          dadIDz = dadID,
                                          zmomID = momID,
                                          spID2 = spouseID)

renamed_unfixed <- standardizeColnames(data_unfixable_names)
expect_true("zfamID" %in% names(renamed_unfixed))
expect_false("famID" %in% names(renamed_unfixed))
expect_true("dadIDz" %in% names(renamed_unfixed))
expect_false("dadID" %in% names(renamed_unfixed))
expect_true("zmomID" %in% names(renamed_unfixed))
expect_false("momID" %in% names(renamed_unfixed))
expect_true("spID2" %in% names(renamed_unfixed))
expect_false("spID" %in% names(renamed_unfixed))
})


# standardizeColnames <- function(df, verbose = FALSE) {
#   # Internal mapping of standardized names to possible variants
#   mapping <- list(
#     "famID" = "^(?:fam(?:ily)?[\\.\\-_]?(?:id)?)",
#     "ID" = "^(?:i(?:d$|ndiv(?:idual)?)|p(?:erson)?[\\.\\-_]?id)",
#     "gen" = "^(?:gen(?:s|eration)?)",
#     "dadID" = "^(?:d(?:ad)?id|paid|fatherid|pid[\\.\\-_]?fath[er]*|sire)",
#     "patID" = "^(?:dat[\\.\\-_]?id|pat[\\.\\-_]?id|paternal[\\.\\-_]?(?:id)?)",
#     "momID" = "^(?:m(?:om|a|other)?[\\.\\-_]?id|pid[\\.\\-_]?moth[er]*|dame)",
#     "matID" = "^(?:mat[\\.\\-_]?id|maternal[\\.\\-_]?(?:id)?)",
#     "spID" = "^(?:s(?:pt)?id|spouse[\\.\\-_]?(?:id)?|partner[\\.\\-_]?(?:id)?|husb(?:and)?[\\.\\-_]?id|wife[\\.\\-_]?(?:id)?|pid[\\.\\-_]?spouse1?)",
#     "twinID" = "^(?:twin[\\.\\-_]?(?:id)?)",
#     "sex" = "^(?:sex|gender|female|m(?:a(?:le|n)|en)|wom[ae]n)"
#   )

