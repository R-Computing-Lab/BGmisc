# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
#library(here)
library(readr)
#library(usethis)
library(BGmisc)



## Create dataframe
ASOIAF <- ged <- readGedcom("data-raw/ASOIAF.ged") %>%
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
      personID == 281 ~ "Daeron Targaryen (son of Aegon V)",
      personID == 201 ~ "Jaehaerys II Targaryen",
      personID == 482 ~ "Lord Dayne (father of Edric)", # https://awoiaf.westeros.org/index.php/Lord_Dayne_(father_of_Edric)
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
      personID == 237 ~ "Youngest daughter of Elys Waynwood",
      personID == 344 ~ "Alicent Hightower",
      personID == 348 ~ "Helaena Targaryen",
      personID == 347 ~ "Daeron Targaryen",
      personID == 343 ~ "Baelon Targaryen (son of Viserys I)",
      personID == 317 ~ "Father of Maelys I Blackfyre",
      personID == 255 ~ "Lord Tully",
      personID == 468 ~ "Lyarra Stark",
      personID == 465 ~ "Willam Stark",
      personID == 469 ~ "Jocelyn Stark",
      personID == 470 ~ "Benedict Royce",
      personID == 467 ~ "Melantha Blackwood",
      personID == 179 ~ "Gerold Lannister",
      personID == 180 ~ "Tywald Lannister",
      personID == 181 ~ "Tion Lannister",
      personID == 182 ~ "Jason Lannister",
      personID %in% c(300:301) ~ "Stillborn Targaryen",
      personID == 284 ~ "Vaella Targaryen",
      personID == 499  ~ "Ormund Baratheon",
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
  ) %>%
  mutate(
    zygosity = case_when(
      personID %in% c(164, 165) ~ "dz", # Jaime Lannister

      !is.na(twinID) ~ "unknown",
      TRUE ~ NA_character_
    )
  )

# Add new people to the pedigree
df <- df %>%
  addPersonToPed(
    name = "Larra Rogare", sex = "F",
    personID = 502, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Rodrik Arryn", sex = "M",
    personID = 503, momID = NA, dadID = 519
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
    name = "Rymond Arryn",
    sex = "M", personID = 519, momID = NA, dadID = NA
  ) %>%
  # https://awoiaf.westeros.org/index.php/Rymond_Arryn
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
  ) %>%
  # https://awoiaf.westeros.org/index.php/Baelon_Targaryen_(son_of_Jaehaerys_I)
  addPersonToPed(
    name = "Wife of Jasper Arryn",
    sex = "F", personID = 538, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Otto Hightower",
    sex = "M", personID = 539, momID = 543, dadID = 542
  ) %>%
  # https://awoiaf.westeros.org/index.php/Otto_Hightower
  addPersonToPed(
    name = "Gwayne Hightower",
    sex = "M", personID = 540, momID = 541, dadID = 539
  ) %>%
  addPersonToPed(
    name = "Wife of Otto Hightower",
    sex = "F", personID = 541, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Father of Otto Hightower",
    sex = "M", personID = 542, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Mother of Otto Hightower",
    sex = "F", personID = 543, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Lord Hightower",
    sex = "M", personID = 544, momID = 543, dadID = 542
  ) %>%
  addPersonToPed(
    name = "Laena Velaryon",
    sex = "F", personID = 545, momID = 514, dadID = 523
  ) %>%
  addPersonToPed(
    name = "Baela	Targaryen",
    sex = "F", personID = 546, momID = 545, dadID = 536,
    twinID = 547, zygosity = "unknown"
  ) %>%
  addPersonToPed(
    name = "Rhaena Targaryen",
    sex = "F", personID = 547, momID = 545, dadID = 536,
    twinID = 546, zygosity = "unknown"
  ) %>%
  addPersonToPed(
    name = "Visenya Targaryen (daughter of Rhaenyra)",
    sex = "F", personID = 548, momID = 339, dadID = 536
  ) %>%
  addPersonToPed(
    name = "Harwin Strong",
    sex = "M", personID = 549, momID = NA, dadID = 553
  ) %>%
  addPersonToPed(
    name = "Jacaerys Velaryon",
    sex = "M", personID = 550, momID = 339, dadID = 549
  ) %>%
  addPersonToPed(
    name = "Lucerys Velaryon",
    sex = "M", personID = 551, momID = 339, dadID = 549
  ) %>%
  addPersonToPed(
    name = "Joffrey Velaryon",
    sex = "M", personID = 552, momID = 339, dadID = 549
  ) %>%
  addPersonToPed(
    name = "Lyonel Strong",
    sex = "M", personID = 553, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Alys Rivers",
    sex = "F", personID = 554, momID = NA, dadID = 553
  ) %>%
  addPersonToPed(
    name = "Son of Alys Rivers",
    sex = "M", personID = 555, momID = 554, dadID = 346
  ) %>%
  addPersonToPed(
    name = "Jaehaera Targaryen",
    sex = "F", personID = 556, momID = 348, dadID = 345,
    twinID = 557, zygosity = "dz"
  ) %>%
  addPersonToPed(
    name = "Jaehaerys Targaryen (son of Aegon II)",
    sex = "M", personID = 557, momID = 348, dadID = 345,
    twinID = 556, zygosity = "dz"
  ) %>%
  addPersonToPed(
    name = "Maelor Targaryen (son of Aegon II)",
    sex = "M", personID = 558, momID = 348, dadID = 345
  ) %>%
  addPersonToPed(
    name = "Laena Velaryon (daughter of Baela Targaryen)",
    sex = "F", personID = 559, momID = 546, dadID = 333
  ) %>%
  addPersonToPed(
    name = "Rohanne of Tyrosh",
    sex = "F", personID = 560, momID = NA, dadID = NA
  ) %>%
  # https://awoiaf.westeros.org/index.php/Rohanne_of_Tyrosh
  addPersonToPed(
    name = "Daemon III Blackfyre",
    sex = "M", personID = 561, momID = NA, dadID = 316
  ) %>%
  addPersonToPed(
    name = "Calla Blackfyre",
    sex = "F", personID = 562, momID = 560, dadID = 312
  ) %>%
  # https://awoiaf.westeros.org/index.php/Calla_Blackfyre

  addPersonToPed(
    name = "Barba Bracken",
    sex = "F", personID = 563, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Lord Frey",
    sex = "M", personID = 564, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Lady Butterwell Frey",
    sex = "F", personID = 565, momID = 566, dadID = 564
  ) %>%
  addPersonToPed(
    name = "Lady Frey",
    sex = "F", personID = 566, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Mother of Maron Martell", sex = "F",
    personID = 567, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Prince of Dorne (father of Maron)", sex = "M",
    personID = 568, momID = NA, dadID = NA
  ) %>%
  # https://awoiaf.westeros.org/index.php/Prince_of_Dorne_(father_of_Maron)
  addPersonToPed(
    name = "Maron Martell", sex = "M",
    personID = 569, momID = 567, dadID = 568
  ) %>%
  addPersonToPed(
    name = "Mother of Hoster Tully", sex = "F",
    personID = 570, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Marna Locke", sex = "F",
    personID = 571, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Arya Flint", sex = "F",
    personID = 572, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Rodrik Stark", sex = "M",
    personID = 573, momID = 576, dadID = 575
  ) %>%
  addPersonToPed(
    name = "Lyanne Glover", sex = "F",
    personID = 574, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Beron Stark", sex = "M",
    personID = 575, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Lorra Royce", sex = "F",
    personID = 576, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Donnor Stark", sex = "M",
    personID = 577, momID = 576, dadID = 575
  ) %>%
  addPersonToPed(
    name = "Artos Stark", sex = "M",
    personID = 578, momID = 576, dadID = 575
  ) %>%
  addPersonToPed(
    name = "Berena Stark", sex = "F",
    personID = 579, momID = 576, dadID = 575
  ) %>%
  addPersonToPed(
    name = "Alysanne Stark", sex = "F",
    personID = 580, momID = 576, dadID = 575
  ) %>%
  addPersonToPed(
    name = "Errold Stark", sex = "M",
    personID = 581, momID = 576, dadID = 575
  ) %>%
  addPersonToPed(
    name = "Branda Stark", sex = "F",
    personID = 582, momID = 572, dadID = 573
  ) %>%
  addPersonToPed(
    name = "Wife of Andros Brax", sex = "F",
    personID = 583, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Jeyne Marbrand", sex = "F",
    personID = 584, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Rohanne Webber", sex = "F",
    personID = 585, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Marla Prester", sex = "F",
    personID = 586, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Alys Stackspear", sex = "F",
    personID = 587, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Damon Lannister", sex = "M",
    personID = 588, momID = 587, dadID = 182
  ) %>%
  addPersonToPed(
    name = "Jena Dondarrion", sex = "F",
    personID = 589, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Wife of Aerys", sex = "F",
    personID = 590, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Melissa Blackwood", sex = "F",
    personID = 591, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Gwenys Rivers", sex = "F",
    personID = 592, momID = 591, dadID = 302
  ) %>%
  addPersonToPed(
    name = "Mya Rivers", sex = "F",
    personID = 593, momID = 591, dadID = 302
  ) %>%
  addPersonToPed(
    name = "Stillborn Twin Targaryen", sex = "M",
    personID = 594, momID = 303, dadID = 302,
    twinID = 304, zygosity = "dz"
  ) %>%
  addPersonToPed(
    name = "Consort of Dorne (Father of Doran Martell)", sex = "M",
    personID = 595, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Rhaena Targaryen (daughter of Aenys I)", sex = "F",
    personID = 596, momID = 510, dadID = 352
  ) %>%
  addPersonToPed(
    name = "Aegon Targaryen (son of Aenys I)", sex = "M",
    personID = 597, momID = 510, dadID = 352
  ) %>%
  addPersonToPed(
    name = "Viserys Targaryen (son of Aenys I)", sex = "M",
    personID = 598, momID = 510, dadID = 352
  ) %>%
  addPersonToPed(
    name = "Vaella Targaryen", sex = "F",
    personID = 599, momID = 510, dadID = 352
  ) %>%
  addPersonToPed(
    name = "Kiera of Tyrosh", sex = "F",
    personID = 600, momID = NA, dadID = NA
  ) %>%
  addPersonToPed(
    name = "Aerea Targaryen", sex = "F",
    personID = 601, momID = 596, dadID = 597,
    twinID = 602, zygosity = "mz"
 ) %>%
  addPersonToPed(
    name = "Rhaella Targaryen (daughter of Aegon)", sex = "F",
    personID = 602, momID = 596, dadID = 597,
    twinID = 601, zygosity = "mz"
  )  %>%
  addPersonToPed(
    name = "Erryk Cargyll", sex = "M",
    personID = 603, momID = 605, dadID = 606,
    twinID = 604, zygosity = "mz"
  )  %>%   addPersonToPed(
    name = "Arryk Cargyll", sex = "M",
    personID = 604, momID = 605, dadID = 606,
    twinID = 603, zygosity = "mz"
  )  %>%  addPersonToPed(
    name = "Father Cargyll", sex = "M",
    personID = 606, momID = NA, dadID = NA
  )  %>%   addPersonToPed(
    name = "Mother Cargyll", sex = "F",
    personID = 605, momID = NA, dadID = NA
  )

# modify existing people
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
      personID %in% c(300:301) ~ 600,
      personID %in% c(281) ~ 517,
      personID %in% c(310) ~ 591,
      personID %in% c(364, 366, 367) ~ 590,
      personID %in% c(298:299) ~ 589, # Jena Dondarrion
      personID %in% c(163, 183) ~ 586,
      personID %in% c(158, 180, 181, 182) ~ 585, # Rohanne Webber
      personID %in% c(28, 159:162) ~ 584, # Jeyne Marbrand is mother of Tywin Lannisteretc
      personID %in% c(154, 155, 121) ~ 583, # all the braxes share the same mother
      personID == 465 ~ 576, # Lorra Royce
      personID == 466 ~ 574, # Lyanne Glover is the mother of Brandon Stark
      personID == 468 ~ 572, # Arya Flint's is the Mother of Lyarra Stark
      personID %in% c(222, 256) ~ 570,
      personID == 212 ~ 571, # Marna Locke is the mother of Rickard Stark
      personID == 291 ~ 567, # Myriah Martell's mother is the Mother of Maron Martell
      personID == 309 ~ 563, # Barba Bracken is the mother of Aegor Rivers
      personID %in% c(313:319) ~ 560, # Blackfyres
      personID == 344 ~ 541, # Alicent Hightower's mother is the wife of Otto Hightower
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
      personID %in% c(351,350) ~ 510, # Alysanne Targaryen's mom is Alyssa Velaryon
      personID %in% c(
        283, # # 	Dyanna Dayne is mother of Daeron Targaryen (son of Maekar I)
        287, #  Aemon Targaryen (son of Maekar I)
        285, # Aerion (son of Maekar) Targaryen
        288
      ) ~ 516, #  Rhae Targaryen
      personID %in% c(226, 232, 231) ~ 538, # kids of  Jasper
      personID == 1 ~ 566, # Walder Frey's mother
      personID == 284 ~ 600,
      TRUE ~ momID
    ),
    dadID = case_when(
      personID %in% c(207, 260:263) ~ 595,
      personID == 465 ~ 575, # Beron Stark
      personID == 291 ~ 568, # Myriah Martell's mother is the Father of Maron Martell
      personID == 344 ~ 539, # Alicent Hightower's father is Otto Hightower
      personID == 354 ~ 358, # Rhaenys Targaryen
      personID == 355 ~ 358, # Visenya Targaryen
      personID == 341 ~ 503, # Aemma Arryn's  father is Rodrik Arryn
      personID == 257 ~ 504, # Princess of Dorne's father is the Father of Princess of Dorne
      personID == 259 ~ 504, # Lewyn Martell's father is the Father of Princess of Dorne
      personID == 333 ~ 522, # Alyn Velaryon's father is officially Laenor Velaryon (but it's probably Corlys Velaryon)
      personID == 326 ~ 528, #  Viserys Plumm
      personID == 322 ~ 529, #  Daenaera Velaryon's father is Daeron Velaryon
      personID == 306 ~ 536, #  Viserys II's father is Daemon Targaryen
      personID == 321 ~ 536, # Aegon III's father is Daemon Targaryen
      personID == 340 ~ 537, # Baelon Targaryen
      personID == 1 ~ 564, # Walder Frey's father is Lord Frey
      personID == 468 ~ 573, # Rodrik	Stark's is the Father of Lyarra Stark
      TRUE ~ dadID
    ),
    twinID = case_when(
      personID == 304 ~ 594,
      TRUE ~ twinID
    )
  )



ASOIAF <- df %>%
  select(-famID) %>%
  ped2fam(personID = "personID", famID = "famID") %>%
  rename(
    id = personID
  ) %>%
  mutate(
    zygosity = case_when(
      id == 304 ~ "dz",
      !is.na(zygosity) ~ zygosity,
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
