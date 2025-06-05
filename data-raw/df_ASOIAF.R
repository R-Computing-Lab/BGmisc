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
      personID == 322 ~ "Daenaera Velaryon",
      name == "Naerys " ~ "Naerys Targaryen",
      name == "Rhaenyra " ~ "Rhaenyra Targaryen",
      name == "Betharios " ~ "Betharios of Braavos",
      personID == 257 ~ "Princess Of Dorne",
      personID == 341 ~ "Aemma Arryn",
      name == "Rowena " ~ "Rowena Arryn",
      name == "Pate " ~ "Pate of the Blue Fork",
      name == "Mellario " ~ "Mellario of Norvos",
      personID == 289 ~ "Daella Targaryen (daughter of Maekar I)",
      personID == 201 ~ "Jaehaerys II Targaryen",
      personID == 202 ~ "Aerys II Targaryen",
      personID == 200 ~ "Aegon V Targaryen",
      personID == 336 ~ "Companion of Alyn Velaryon",
      is.na(.data$name) & personID %in% c(360, 489:498) ~ "Bastard of Robert Baratheon",
      personID == 326 ~ "Viserys Plumm",
      personID == 327 ~ "Robin Penrose",
      personID == 328 ~ "Laena Penrose",
      personID == 329 ~ "Jocelyn Penrose",
      personID == 330 ~ "Joy Penrose",
      personID == 325 ~ "Ronnel Penrose",
      personID == 285 ~ "Aerion (son of Maekar) Targaryen",
      personID == 358 ~ "Aerion (son of Daemion) Targaryen",
      personID == 364 ~ "Daemion Targaryen (Lord of Dragonstone)",
      personID == 340 ~ "Viserys I Targaryen",
      personID == 283 ~ "Daeron (son of Maekar) Targaryen",
      personID == 288 ~ "Rhae Targaryen", # not Rhaelle Targaryen
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

df <- df %>%
  addPersonToPed(
    name = "Larra Rogare", sex = "F",
    personID = 502, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Rodrik Arryn", sex = "M",
    personID = 503, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Father of Princess of Dorne",
    sex = "M", personID = 504, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Mother of Princess of Dorne",
    sex = "F", personID = 505, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Alyssa Targaryen",
    sex = "F", personID = 506, momID = 351, dadID = 350
  ) %>% # https://awoiaf.westeros.org/index.php/Alyssa_Targaryen
  addPersonToPed(
    name = "Davos Baratheon",
    sex = "M", personID = 507, momID = 362, dadID = 361
  ) %>%
  # https://awoiaf.westeros.org/index.php/Davos_Baratheon
  addPersonToPed(
    name = "Unknown Baratheon",
    sex = "M", personID = 508, momID = 362, dadID = 361
  ) %>%
  # https://awoiaf.westeros.org/index.php/Davos_Baratheon#cite_note-4
  addPersonToPed(
    name = "Rogar Baratheon",
    sex = "M", personID = 509, momID = NA, # Unknown
    dadID = 508
  ) %>% # https://awoiaf.westeros.org/index.php/Rogar_Baratheon
  addPersonToPed(
    name = "Alyssa Velaryon",
    sex = "F", personID = 510, momID = 527, #
    dadID = 526
  ) %>% # https://awoiaf.westeros.org/index.php/Alyssa_Velaryon
  addPersonToPed(
    name = "Boremund Baratheon",
    sex = "M", personID = 511, momID = 510, # Alyssa Velaryon
    dadID = 509
  ) %>% # https://awoiaf.westeros.org/index.php/Boremund_Baratheon
  addPersonToPed(
    name = "Jocelyn Baratheon",
    sex = "F", personID = 512, momID = 510,
    dadID = 509
  ) %>% # https://awoiaf.westeros.org/index.php/Jocelyn_Baratheon
  addPersonToPed(
    name = "Aemon Targaryen (son of Jaehaerys I)",
    sex = "M", personID = 513, momID = 351, dadID = 350
  ) %>%
  # https://awoiaf.westeros.org/index.php/Aemon_Targaryen_(son_of_Jaehaerys_I)
  addPersonToPed(
    name = "Rhaenys Targaryen (daughter of Aemon)",
    sex = "F", personID = 514, momID = 512,
    dadID = 513
  ) %>% # https://awoiaf.westeros.org/index.php/Rhaenys_Targaryen_(daughter_of_Aemon)
  addPersonToPed(
    name = "Daella Targaryen (daughter of Jaehaerys I)",
    sex = "F", personID = 515, momID = 351, dadID = 350
  ) %>%
  # https://awoiaf.westeros.org/index.php/Daella_Targaryen_(daughter_of_Jaehaerys_I)
  addPersonToPed(
    name = "Dyanna Dayne",
    sex = "F", personID = 516, momID = NA, dadID = NA
  ) %>%
  # %>% # https://awoiaf.westeros.org/index.php/Dyanna_Dayne
  addPersonToPed(
    name = "Betha Blackwood",
    sex = "F", personID = 517, momID = NA, dadID = NA
  ) %>%
  # https://awoiaf.westeros.org/index.php/Betha_Blackwood
  addPersonToPed(
    name = "Shaera Targaryen",
    sex = "F", personID = 518, momID = 517, dadID = 200
  ) %>%
  # https://awoiaf.westeros.org/index.php/Shaera_Targaryen
  addPersonToPed(
    name = "Daeron Targaryen (son of Aegon V)",
    sex = "M", personID = 519, momID = 517, dadID = 200
  ) %>%
  # https://awoiaf.westeros.org/index.php/Daeron_Targaryen_(son_of_Aegon_V)
  addPersonToPed(
    name = "Marilda of Hull",
    sex = "F", personID = 520, momID = NA, dadID = NA
  ) %>%
  # https://awoiaf.westeros.org/index.php/Marilda_of_Hull
  addPersonToPed(
    name = "Addam Velaryon of Hull",
    sex = "M", personID = 521, momID = 520, dadID = 522
  ) %>%
  # https://awoiaf.westeros.org/index.php/Addam_Velaryon
  addPersonToPed(
    name = "Laenor Velaryon",
    sex = "M", personID = 522, momID = 514, dadID = 523
  ) %>%
  # https://awoiaf.westeros.org/index.php/Laenor_Velaryon
  addPersonToPed(
    name = "Corlys Velaryon",
    sex = "M", personID = 523, momID = 534, dadID = 524
  ) %>%
  # https://awoiaf.westeros.org/index.php/Corlys_Velaryon
  addPersonToPed(
    name = "Corwyn Velaryon",
    sex = "M", personID = 524, momID = NA, dadID = 525
  ) %>%
  # https://awoiaf.westeros.org/index.php/Corwyn_Velaryon
  addPersonToPed(
    name = "Daemon Velaryon",
    sex = "M", personID = 525, momID = 527, dadID = 526
  ) %>%
  # https://awoiaf.westeros.org/index.php/Daemon_Velaryon
  addPersonToPed(
    name = "Aethan Velaryon",
    sex = "M", personID = 526, momID = NA, dadID = NA
  ) %>%
  # https://awoiaf.westeros.org/index.php/Aethan_Velaryon
  addPersonToPed(
    name = "Alarra Massey",
    sex = "F", personID = 527, momID = NA, dadID = NA
  ) %>%
  # https://awoiaf.westeros.org/index.php/Alarra_Massey
  addPersonToPed(
    name = "Ossifer Plumm",
    sex = "M", personID = 528, momID = NA, dadID = NA
  ) %>%
  # https://awoiaf.westeros.org/index.php/Ossifer_Plumm
  addPersonToPed(
    name = "Daeron Velaryon",
    sex = "M", personID = 529, momID = 531, dadID = 532
  ) %>%
  # https://awoiaf.westeros.org/index.php/Daeron_Velaryon
  addPersonToPed(
    name = "Daemion Velaryon",
    sex = "M", personID = 530, momID = 531, dadID = 532
  ) %>%
  # https://awoiaf.westeros.org/index.php/Daemion_Velaryon
  addPersonToPed(
    name = "Wife of Vaemond Velaryon",
    sex = "F", personID = 531, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Vaemond Velaryon",
    sex = "M", personID = 532, momID = NA, dadID = 533
  ) %>%
  # https://awoiaf.westeros.org/index.php/Vaemond_Velaryon
  addPersonToPed(
    name = "Second-born son of Corwyn Velaryon",
    sex = "M", personID = 533, momID = 534, dadID = 524
  ) %>%
  addPersonToPed(
    name = "Unknown wife of Corwyn Velaryon",
    sex = "F", personID = 534, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Hazel Harte",
    sex = "F", personID = 535, momID = NA, dadID = NA
  ) %>%
  # https://awoiaf.westeros.org/index.php/Hazel_Harte
  # add person for
  addPersonToPed(
    name = "Daemon Targaryen",
    sex = "M", personID = 536, momID = 506, dadID = 537
  ) %>%
  # https://awoiaf.westeros.org/index.php/Daemon_Targaryen
  addPersonToPed(
    name = "Baelon Targaryen (son of Jaehaerys I)",
    sex = "M", personID = 537, momID = 351, dadID = 350
  ) # %>%
# https://awoiaf.westeros.org/index.php/Baelon_Targaryen_(son_of_Jaehaerys_I)
df <- df %>%
  mutate(
    sex = case_when(
      personID == 326 ~ "M",
      personID == 327 ~ "M",
      personID == 328 ~ "F",
      personID == 329 ~ "F",
      personID == 330 ~ "F",
      TRUE ~ sex
    ),
    momID = case_when(
      personID == 354 ~ 359, # Rhaenys Targaryen
      personID == 355 ~ 359, # Visenya Targaryen,
      personID == 303 ~ 502, # Naerys Targaryen's mother is Larra Rogare
      personID == 302 ~ 502, # Aegon IV Targaryen's mother is Larra Rogare
      personID == 307 ~ 502, # Aemon Targaryen, the Dragonknight, mother is Larra Rogare
      personID == 200 ~ 516, # Aegon V Targaryen's mother is Dyanna Dayne
      personID == 341 ~ 515, # Aemma Arryn's mother is Daella Targaryen (daughter of Jaehaerys I)
      personID == 289 ~ 516, # Daella Targaryen (daughter of Maekar I) has Dyanna Dayne
      personID == 257 ~ 505, # Princess of Dorne's mother is the Mother of Princess of Dorne
      personID == 259 ~ 505, # Lewyn Martell mother is the Mother of Princess of Dorne
      personID == 340 ~ 506, # Viserys I Targaryen's mother is Alyssa Targaryen
      personID == 280 ~ 517, # Betha Blackwood is mother of Duncan the Small
      personID == 201 ~ 517, # Betha Blackwood  is mother of Jaehaerys II Targaryen
      personID == 199 ~ 517, # Betha Blackwood is mother of Rhaelle Targaryen
      personID == 203 ~ 518, # Rhaella Targaryen's mother is Shaera Targaryen
      personID == 202 ~ 518, # 	Aerys II's mother is Shaera Targaryen
      personID == 333 ~ 520, # Alyn Velaryon's mother is Marilda of Hull
      personID == 322 ~ 535, # Daenaera Velaryon's mother is Hazel Harte
      personID == 306 ~ 339, #  Viserys II's mother is Rhaenyra Targaryen
      personID == 351 ~ 510, # Alysanne Targaryen's mom is Alyssa Velaryon
      personID == 283 ~ 516, # 	Dyanna Dayne is mother of Daeron Targaryen (son of Maekar I)
      personID == 287 ~ 516, # Dyanna Dayne is mother of Aemon Targaryen (son of Maekar I)
      personID == 285 ~ 516, # Dyanna Dayne is mother of Aerion (son of Maekar) Targaryen
      personID == 288 ~ 516, # Dyanna Dayne is mother of Rhae Targaryen
      TRUE ~ momID
    ),
    dadID = case_when(
      personID == 354 ~ 358, # Rhaenys Targaryen
      personID == 355 ~ 358, # Visenya Targaryen
      personID == 341 ~ 503, # Aemma Arryn's  father is Rodrik Arryn
      personID == 257 ~ 504, # Princess of Dorne's father is the Father of Princess of Dorne
      personID == 259 ~ 504, # Lewyn Martell's father is the Father of Princess of Dorne
      personID == 333 ~ 522, # Alyn Velaryon's father is officially Laenor Velaryon (but it's probably Corlys Velaryon)
      personID == 326 ~ 528, # Viserys Plumm
      personID == 322 ~ 529, #   Daenaera Velaryon's father is Daeron Velaryon
      personID == 306 ~ 536, #  Viserys II's father is Daemon Targaryen
      personID == 321 ~ 536, # Aegon III's father is Daemon Targaryen
      personID == 340 ~ 537, # Baelon Targaryen
      TRUE ~ dadID
    )
  )



ASOIAF <- df %>%
  select(-famID) %>%
  ped2fam(personID = "personID") %>%
  rename(
    id = personID
  ) %>%
  mutate(
    zygosity = case_when(
      id %in% c(164, 165) ~ "dz", # Jaime Lannister
      !is.na(twinID) ~ "unknown",
      TRUE ~ NA_character_
    )
  )


# checks

df_repaired <- checkSex(ASOIAF,
  code_male = 1,
  code_female = 0,
  verbose = FALSE, repair = TRUE
) %>%
  checkParentIDs(
    addphantoms = TRUE,
    repair = TRUE,
    parentswithoutrow = FALSE,
    repairsex = FALSE
  )


checkIDs(df_repaired)
checkis_acyclic <- checkPedigreeNetwork(df_repaired,
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  verbose = TRUE
)
checkis_acyclic
if (checkis_acyclic$is_acyclic) {
  message("The pedigree is acyclic.")
  write_csv(ASOIAF, here("data-raw", "ASOIAF.csv"))
  usethis::use_data(ASOIAF, overwrite = TRUE, compress = "xz")
} else {
  message("The pedigree contains cyclic relationships.")
}
