test_that("data loads silently", {
  expect_silent(data(hazard))
  expect_silent(data(inbreeding))
  expect_silent(data(potter))
})


test_that("hazard data checkis_acyclic", {
  expect_silent(data(hazard))
  expect_true(nrow(hazard) == 43)
  expect_true(nrow(hazard) == max(hazard$ID, na.rm = TRUE))
  expect_true(all(c("famID", "ID", "sex", "dadID", "momID", "affected", "DA1", "DA2", "birthYr", "onsetYr", "deathYr", "available", "gen", "proband")
  %in% names(hazard)))
  checkis_acyclic <- checkPedigreeNetwork(hazard,
    personID = "ID",
    momID = "momID",
    dadID = "dadID",
    verbose = FALSE
  )
  expect_true(checkis_acyclic$is_acyclic)
})

test_that("inbreeding data loads", {
  expect_silent(data(inbreeding))
  expect_true(nrow(inbreeding) == 134)
  expect_true(nrow(inbreeding) == max(inbreeding$ID, na.rm = TRUE))
  expect_true(all(c("ID", "sex", "dadID", "momID", "famID", "gen", "proband")
  %in% names(inbreeding)))
  checkis_acyclic <- checkPedigreeNetwork(inbreeding,
    personID = "ID",
    momID = "momID",
    dadID = "dadID",
    verbose = FALSE
  )
  expect_true(checkis_acyclic$is_acyclic)
})
test_that("potter data loads", {
  expect_silent(data(potter))
  expect_true(all(c(
    "personID", "famID", "name",
    "gen", "momID", "dadID",
    "spouseID", "sex", "twinID", "zygosity"
  )
  %in% names(potter)))
  expect_true(nrow(potter) == 36)
  expect_true(nrow(potter) < max(potter$personID, na.rm = TRUE))
  checkis_acyclic <- checkPedigreeNetwork(potter,
    personID = "personID",
    momID = "momID",
    dadID = "dadID",
    verbose = FALSE
  )
})
