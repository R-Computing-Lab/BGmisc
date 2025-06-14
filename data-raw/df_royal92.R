# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(here)
library(readr)
library(usethis)
library(BGmisc)



## Create dataframe
royal92 <- df_raw <- readGedcom("data-raw/royal92.ged")
royal92 <- ped2fam(royal92, personID = "personID") %>%
  select(
    -death_place, -birth_place,
    -name_given,
    -name_surn,
    -FAMC,
    -FAMS
  ) %>%
  mutate(
    momID = as.numeric(momID),
    dadID = as.numeric(dadID),
    name = str_remove(name, "/"),
    aproximated_dob = case_when(
      str_detect(birth_date, "ABT|BET|AFT|BEF")==TRUE~ TRUE,
      str_length(birth_date) == 4  ~ TRUE,
    TRUE ~ FALSE
    ),
    aproximated_dod = case_when(
      str_detect(death_date, "ABT|BET|AFT|BEF")==TRUE~ TRUE,
      str_length(death_date) == 4  ~ TRUE,
    TRUE ~ FALSE
    ),
    birth_date = str_replace_all(birth_date, "ABT|BET|AFT|BEF", ""),
    death_date = str_replace_all(death_date, "ABT|BET|AFT|BEF", ""),

    birth_date = case_when(
      str_length(birth_date) == 0 ~ NA_character_,
      str_length(birth_date) == 4 ~ paste0("15 JUN ", birth_date),
      TRUE ~ birth_date
    ),
    birth_date = as.Date(str_trim(birth_date), format = "%d %b %Y"),
    death_date = case_when(
      str_length(death_date) == 0 ~ NA_character_,
      str_length(death_date) == 4 ~ paste0("15 JUN ", death_date),
      TRUE ~ death_date
    ),
    death_date = as.Date(str_trim(death_date), format = "%d %b %Y"),
    name = case_when(
      personID == 2944 ~ "William Scott of Buccleuch Montagu-Douglas",
      TRUE ~ str_replace_all(name, "_", " ")
  ))
#"22 AUG 1485"
# if missing momID or dadID, assign the next available ID

#df_NA <- df %>%
#  mutate(
#    momID = if_else(is.na(momID), max(id) + 1, momID),
#    dadID = if_else(is.na(dadID), max(id) + 2, dadID)
#  )

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



