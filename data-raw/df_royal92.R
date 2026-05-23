# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(here)
library(readr)
library(usethis)
library(BGmisc)

date_qualifier_regex <- "\\b(?:A[BF]T|BE[TF])\\b\\s*"
text_cleanup_regex <- c(
  "/|\\(twin\\)" = "",
  "_" = " ",
  "\\s*-\\s*" = "-"
)

is_approximated_date <- function(x) {
  str_detect(x, date_qualifier_regex) | str_length(x) == 4
}

strip_date_qualifier <- function(x) {
  x %>%
    str_replace_all(date_qualifier_regex, "") %>%
    str_squish()
}

standardize_partial_date <- function(x) {
  case_when(
    str_length(x) == 0 ~ NA_character_,
    str_length(x) == 4 ~ paste0("15 JUN ", x),
    TRUE ~ x
  )
}

parse_gedcom_date <- function(x) {
  x %>%
    str_trim() %>%
    as.Date(format = "%d %b %Y")
}


## Create dataframe
royal92 <- df_raw <- readGedcom("data-raw/royal92.ged")  %>%
  addPersonToPed(
    personID = 1147,
    name = "Henry de Montfort",
    sex = "M",
    momID = 1370,
    dadID = 873,
    overwrite = TRUE
  )  %>%  addPersonToPed(
    personID = 3011,
    name = "Simon de Montfort the Younger",
    sex = "M",
    momID = 1370,
    dadID = 873
  )  %>%
  addPersonToPed(
    personID = 2848,
    name = "Michael of Greece and Denmark",
    sex = "M",
    momID = 2846,
    dadID = 465,
    overwrite = TRUE
  )

#----
# Create overrides for dates and names based on historical records and data cleaning needs
#----

date_overrides <- tribble(
  ~personID, ~birth_date_override, ~death_date_override,
  220,  "27 MAR 1819", "27 MAR 1819",
  812,  "16 JUN 1908", "11 DEC 1996",
  1147, "15 NOV 1238", "4 AUG 1265",
  1149, "7 OCT 1816",  "12 APR 1817",
  1298, "27 APR 1779", "15 JUN 1831",
  2456, "15 JUN 1070", "14 FEB 1117",
  2846, "25 DEC 1902", "25 FEB 1953",
  2848, "7 JAN 1939",  "28 JUL 2024",
  2939, "17 JUL 1838", "14 NOV 1886",
  2941, "12 JUN 1811", "5 JUN 1846",
  2942, "24 MAY 1772", "20 APR 1819",
  2948, "2 MAY 1841",  "22 NOV 1906",
  2950, "2 SEP 1746",  "11 JAN 1812",
  2955, "16 JUN 1803", "22 JUL 1844",
  2985, "26 APR 1924", "14 DEC 1997",
  3011, "15 APR 1240", "15 JUN 1271"
)


name_overrides <- tribble(
  ~personID, ~name_override,
  12,   "Alexandra of Denmark (Alix)",
  27,   "Victoria Eugenie (Ena)",
  39,   "Alexandra Fedorovna (Alix)",
  41,   "Dagmar (Marie) of Denmark",
  84,   "Elizabeth (Ella)",
  85,   "Mary (May)",
  136,  "Mary Adelaide (Fat Mary)",
  155,  "Michael (Mischa) Alexandrovich Romanov",
  220,  "Charlotte Augusta Louisa Hanover",
  785,  "Richard Curzon-Howe",
  788,  "James Hamilton",
  812,  "Marian Louisa Montagu-Douglas-Scott",
  1197, "Karl Theodor (Gackl)",
  1200, "Sophie Charlotte Auguste",
  1442, "Ferdinand Philippe Marie d'Orléans",
  1594, "John IV (the Conqueror) of Montfort",
  1709, "Henry Somerset",
  2846,  "Françoise of Orléans",
  2851, "Ernest Frederick III of Saxe-Hildburghausen",
  2944, "William Scott of Buccleuch Montagu-Douglas",
  2946, "Herbert Montagu Douglas Scott",
  2948, "Henry Robert Brand",
  2955, "Anne Amelia Keppel",
  2990, "William Legge",
  2991, "Rupert Legge",
  2992, "Charlotte Legge",
  2993, "Henry Legge"
)



royal92 <- ped2fam(royal92, personID = "personID") %>%
  select(
    -death_place, -birth_place,
    -name_given,
    -name_surn,
    -FAMC,
    -FAMS
  ) %>%
  left_join(date_overrides, by = "personID") %>%
  mutate(
    birth_date = coalesce(birth_date_override, birth_date),
    death_date = coalesce(death_date_override, death_date)
  ) %>%
  select(-birth_date_override, -death_date_override) %>%
  left_join(name_overrides, by = "personID") %>%
  mutate(name = coalesce(name_override, name)) %>%
  select(-name_override)

royal92_cleaned <- royal92 %>%
  mutate(
    momID = as.numeric(momID),
    dadID = as.numeric(dadID),
    approximated_dob = is_approximated_date(birth_date),
    approximated_dod = is_approximated_date(death_date),
    birth_date = strip_date_qualifier(birth_date),
    death_date = strip_date_qualifier(death_date),
    # if only year is given, assign 15th June as the date
    birth_date = standardize_partial_date(birth_date),
    death_date = standardize_partial_date(death_date),
    # convert to Date format
    birth_date = parse_gedcom_date(birth_date),
    death_date = parse_gedcom_date(death_date),
    twinID = case_when(
     personID == 223 ~ 222,
     personID == 222 ~ 223,
     personID == 1116 ~ 1117,
     personID == 1117 ~ 1116,
     personID == 1155 ~ 1156,
     personID == 1156 ~ 1155,
     TRUE ~ NA_real_
  ),
  attribute_title = str_replace_all(attribute_title, text_cleanup_regex) %>%
      str_squish(),
  sex = case_when(
    personID %in% c(1098,
                    1753,
                    1755,
                    1756,
                    1803,
                    2033,
                    2509,
                    2990,
                    2991,
                    2993
                    ) ~ "M",
    personID %in% c(1149,
                    2992
                    ) ~ "F",
    TRUE ~ sex
  ),
  name = str_replace_all(name, text_cleanup_regex) %>%
    str_squish()
  )



royal92 <- royal92_cleaned %>%
  select(-approximated_dob, -approximated_dod)

checkis_acyclic <- checkPedigreeNetwork(royal92,
  personID = "personID",
  momID = "momID",
  dadID = "dadID",
  verbose = TRUE
)
checkis_acyclic
if (checkis_acyclic$is_acyclic) {
  message("The pedigree is acyclic.")
  write_csv(royal92, here("data-raw", "royal92.csv"))
  usethis::use_data(royal92, overwrite = TRUE, compress = "xz")
} else {
  message("The pedigree contains cyclic relationships.")
}
