# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(here)
library(readr)
library(usethis)
library(BGmisc)



## Create dataframe
ASOIAF <- readGedcom("data-raw/ASOIAF.ged") %>%
  mutate(name = str_remove(name, "/"))

# ASOIAF <- readGedcom("data-raw/ASOIAF_040725.ged")

df <- ped2fam(ASOIAF, personID = "personID") %>%
  select(
    -name_npfx,
    -name_nsfx,
    -name_given,
    -name_surn,
    #    -name_marriedsurn,
    -death_caus,
    -FAMC,
    -FAMS
  ) %>%
  mutate(
    momID = as.numeric(momID),
    dadID = as.numeric(dadID),
    name = case_when(
      name == "Naerys " ~ "Naerys Targaryen",
      name == "Rhaenyra " ~ "Rhaenyra Targaryen",
      name == "Betharios " ~ "Betharios of Braavos",
      personID == 257 ~ "Princess Of Dorne",
      personID == 341 ~ "Aemma Arryn",
      name == "Rowena " ~ "Rowena Arryn",
      name == "Pate " ~ "Pate of the Blue Fork",
      name == "Mellario " ~ "Mellario of Norvos",
      TRUE ~ name
    ),
    twinID = case_match(name,
      "Jaime Lannister" ~ 165,
      "Cersei Lannister" ~ 164,
      "Alyn Frey" ~ 73,
      "Androw Frey" ~ 74,
      "Aegon Blackfyre" ~ 314,
      "Aemon Blackfyre" ~ 313,
      "Dickon Frey" ~ 117,
      "Mathis Frey" ~ 116,
      "Jaime Frey" ~ 101,
      "Tywin Frey" ~ 102,
      "Sarra Frey" ~ 99,
      "Serra Frey" ~ 98,
      "Martyn Lannister" ~ 173,
      "Willem Lannister" ~ 174,
      "Hobber 'Slobber' Redwyne" ~ 390,
      "Horas 'Horror' Redwyne" ~ 391,
      .default = NA_real_
    )
  )
# add new row for Naerys Targaryen's mother

df <- addPersonToPed(ped = df, name= "Larra Rogare", sex="F", personID = 502, momID = NA, dadID = NA)
df <- addPersonToPed(ped = df, name= "Rodrik Arryn", sex="M", personID = 503, momID = NA, dadID = NA)

# fix 359 to be mom for Rhaenyra Targaryen
df <- df %>%
  mutate(
    momID = case_when(
      personID ==  354 ~ 359, # Rhaenyra Targaryen
      personID ==  355 ~ 359, # Visenya Targaryen
      personID ==  303 ~ 502, # Naerys Targaryen's mother is Larra Rogare
      personID ==  302 ~ 502, # Aegon IV Targaryen's mother is Larra Rogare
      personID ==  307 ~ 502, # Aemon Targaryen, the Dragonknight, mother is Larra Rogare
      personID == 341 ~  289, # Aemma Arryn's mother is Daella Targaryen
      personID ==  289 ~  351, # Daella Targaryen's mother is Alysanne Targaryen
      TRUE ~ momID
    ),
    dadID = case_when(
      personID ==  354 ~ 358, # Rhaenyra Targaryen
      personID ==  355 ~ 358, # Visenya Targaryen
      personID ==  341 ~ 503, # Aemma Arryn's  father is Rodrik Arryn
      TRUE ~ dadID
    )
  )

# pedADD <- ped2com(df , personID = "id", momID = "momID",
# dadID = "dadID", component = "additive", isChild_method = "partial_parent")
# com2links(ad_ped_matrix=pedADD)
# if missing momID or dadID, assign the next available ID


ASOIAF <- df %>% select(-famID) %>%
  ped2fam(personID = "personID")  %>%
  rename(
    id = personID
  ) %>% mutate(
    zygosity = case_when(
      id %in% c(164, 165 )~ "dz", # Jaime Lannister
      !is.na(twinID) ~ "unknown",
      TRUE ~ NA_character_
    ))


write_csv(ASOIAF, here("data-raw", "ASOIAF.csv"))
usethis::use_data(ASOIAF, overwrite = TRUE, compress = "xz")
