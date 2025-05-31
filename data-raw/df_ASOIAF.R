# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(here)
library(readr)
library(usethis)
library(BGmisc)



## Create dataframe
ASOIAF <- readGedcom("data-raw/ASOIAF.ged")  %>%
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
    name = case_when(name=="Naerys " ~ "Naerys Targaryen",
                     name=="Rhaenyra " ~ "Rhaenyra Targaryen",
                     name == "Betharios " ~ "Betharios of Braavos",
                              personID == 257 ~ "Princess Of Dorne",
                              name=="Rowena " ~ "Rowena Arryn",
                              name=="Pate " ~ "Pate of the Blue Fork",
                              name=="Mellario " ~ "Mellario of Norvos",
                              TRUE ~ name),
    twinID = case_match(name,
      "Jaime Lannister" ~ 165,
      "Cersei Lannister" ~ 164,

      "Alyn Frey" ~ 73,
      "Androw Frey" ~ 74,

      "Aegon Blackfyre"~ 314,
      "Aemon Blackfyre" ~ 313,

      "Dickon Frey" ~ 117,
      "Mathis Frey" ~ 116,

      "Jaime Frey" ~ 101,
      "Tywin Frey" ~ 102,

      "Sarra Frey" ~99,
      "Serra Frey" ~ 98,

      "Martyn Lannister" ~ 173,
      "Willem Lannister" ~ 174,

      "Hobber 'Slobber' Redwyne" ~ 390,
      "Horas 'Horror' Redwyne" ~ 391,

      .default = NA_real_
    ))

# pedADD <- ped2com(df , personID = "id", momID = "momID",
# dadID = "dadID", component = "additive", isChild_method = "partial_parent")
# com2links(ad_ped_matrix=pedADD)
# if missing momID or dadID, assign the next available ID


ASOIAF <- df %>%
  rename(
    id = personID
  )


write_csv(ASOIAF, here("data-raw", "ASOIAF.csv"))
usethis::use_data(ASOIAF, overwrite = TRUE, compress = "xz")
