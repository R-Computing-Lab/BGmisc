# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(here)
library(readr)
library(usethis)
library(BGmisc)



## Create dataframe
royal92 <- readGedcom("data-raw/royal92.ged")
df <- ped2fam(royal92, personID = "id") %>%
  select(-death_place, -birth_place,
         -name_given,
         -name_surn,
         -FAMC,
         -FAMS) %>%
  mutate(momID = as.numeric(momID),
         dadID = as.numeric(dadID),
         name = str_remove(name, "/"))

# if missing momID or dadID, assign the next available ID

df_NA <- df %>%
  mutate(momID = if_else(is.na(momID), max(id) + 1, momID),
         dadID = if_else(is.na(dadID), max(id) + 2, dadID))





write_csv(royal92, here("data-raw", "royal92.csv"))
usethis::use_data(royal92, overwrite = TRUE, compress = "xz")
