# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(here)
library(readr)
library(usethis)
library(BGmisc)



## Create dataframe
ASOIAF <- readGedcom("data-raw/ASOIAF.ged")

#ASOIAF <- readGedcom("data-raw/ASOIAF_040725.ged")

df <- ped2fam(ASOIAF, personID = "id") %>%
  select(
    -name_npfx,
    -name_nsfx,
    -name_given,
    -name_surn,
    -name_marriedsurn,
    -death_caus,
    -FAMC,
    -FAMS
  ) %>%
  mutate(
    momID = as.numeric(momID),
    dadID = as.numeric(dadID),
    name = str_remove(name, "/")
  )

# pedADD <- ped2com(df , personID = "id", momID = "momID", dadID = "dadID", component = "additive", isChild_method = "partial_parent")
# com2links(ad_ped_matrix=pedADD)
# if missing momID or dadID, assign the next available ID


ASOIAF <- df


write_csv(ASOIAF, here("data-raw", "ASOIAF.csv"))
usethis::use_data(ASOIAF, overwrite = TRUE, compress = "xz")
