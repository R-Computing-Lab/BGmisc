# Downloads the FamilySearch GEDCOM 7 spec TSVs and saves them as sysdata.
# Run once with: source("data-raw/gedcom_spec.R")

base_url <- "https://raw.githubusercontent.com/FamilySearch/GEDCOM/main/extracted-files/"

gedcom_substructures <- utils::read.table(
  paste0(base_url, "substructures.tsv"),
  sep = "\t", header = TRUE, stringsAsFactors = FALSE, quote = ""
)
gedcom_payloads <- utils::read.table(
  paste0(base_url, "payloads.tsv"),
  sep = "\t", header = TRUE, stringsAsFactors = FALSE, quote = ""
)
gedcom_enumerations <- utils::read.table(
  paste0(base_url, "enumerations.tsv"),
  sep = "\t", header = TRUE, stringsAsFactors = FALSE, quote = ""
)

usethis::use_data(
  gedcom_substructures,
  gedcom_payloads,
  gedcom_enumerations,
  internal = TRUE,
  overwrite = TRUE
)
