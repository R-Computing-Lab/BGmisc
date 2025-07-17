# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(here)
library(readr)
library(usethis)
library(BGmisc)
## Create dataframe

potter <- data.frame(
  personID = c(
    1:30
  ),
  famID = rep(1, 30),
  name = c(
    "Vernon Dursley",
    "Marjorie Dursley",
    "Petunia Evans",
    "Lily Evans",
    "James Potter",
    "Dudley Dursley",
    "Harry Potter",
    "Ginny Weasley",
    "Arthur Weasley",
    "Molly Prewett",
    "Ron Weasley",
    "Fred Weasley",
    "George Weasley",
    "Percy Weasley",
    "Charlie Weasley",
    "Bill Weasley",
    "Hermione Granger",
    "Fleur Delacour",
    "Gabrielle Delacour",
    "Audrey",
    "James Potter II",
    "Albus Potter",
    "Lily Potter",
    "Rose Weasley",
    "Hugo Weasley",
    "Victoire Weasley",
    "Dominique Weasley",
    "Louis Weasley",
    "Molly Weasley",
    "Lucy Weasley"
  ), first_name = c(
    "Vernon",
    "Marjorie",
    "Petunia",
    "Lily",
    "James",
    "Dudley",
    "Harry",
    "Ginny",
    "Arthur",
    "Molly",
    "Ron",
    "Fred",
    "George",
    "Percy",
    "Charlie",
    "Bill",
    "Hermione",
    "Fleur",
    "Gabrielle",
    "Audrey",
    "James",
    "Albus",
    "Lily",
    "Rose",
    "Hugo",
    "Victoire",
    "Dominique",
    "Louis",
    "Molly",
    "Lucy"
  ),
    surname = c(
    "Dursley",
    "Dursley",
    "Evans",
    "Evans",
    "Potter",
    "Dursley",
    "Potter",
    "Weasley",
    "Weasley",
    "Prewett",
    "Weasley",
    "Weasley",
    "Weasley",
    "Weasley",
    "Weasley",
    "Weasley",
    "Granger",
    "Delacour",
    "Delacour",
    "Unknown",
    "Potter",
    "Potter",
    "Potter",
    "Weasley",
    "Weasley",
    "Weasley",
    "Weasley",
    "Weasley",
    "Weasley",
    "Weasley"
  ),
  gen = c(
    1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3
  ),
  momID = c(
    101, 101, 103, 103, NA, 3, 4, 10, NA, NA,
    10, 10, 10, 10, 10, 10, NA, 105, 105, NA,
    8, 8, 8, 17, 17, 18, 18, 18, 20, 20
  ),
  dadID = c(
    102, 102, 104, 104, NA, 1, 5, 9, NA, NA,
    9, 9, 9, 9, 9, 9, NA, 106, 106, NA, 7,
    7, 7, 11, 11, 16, 16, 16, 14, 14
  ),
  spouseID = c(
    3, NA, 1, 5, 4, NA, 8, 7, 10, 9,
    17, NA, NA, 20, NA, 18, 11, 16, NA, 14,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
  ),
  sex = c(
    1, 0, 0, 0, 1, 1, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 0, 0, 0, 0,
    1, 1, 0, 0, 1, 0, 0, 1, 0, 0
  )
) # sex: 0 = female, 1 = male


# audrey weasley - unknown maiden name
# James Potter II might not be correct, but I used it to differentiate grandson from grandfather


### Add additional people


potter[nrow(potter) + 1, ] <- list(
  101,
  1,
  "Mother Dursley",
  "Mother",
  "Dursley",
  0,
  NA,
  NA,
  102,
  0
)
potter[nrow(potter) + 1, ] <- list(
  102,
  1,
  "Father Dursley",
  "Father",
  "Dursley",
  0,
  NA,
  NA,
  101,
  1
)
potter[nrow(potter) + 1, ] <- list(
  104,
  1,
  "Father Evans",
  "Father",
  "Evans",
  0,
  NA,
  NA,
  103,
  1
)
potter[nrow(potter) + 1, ] <- list(
  103,
  1,
  "Mother Evans",
  "Mother",
  "Evans",
  0,
  NA,
  NA,
  104,
  0
)
potter[nrow(potter) + 1, ] <- list(
  106,
  1,
  "Father Delacour",
  "Father",
  "Delacour",
  0,
  NA,
  NA,
  105,
  1
)
potter[nrow(potter) + 1, ] <- list(
  105,
  1,
  "Mother Delacour",
  "Mother",
  "Delacour",
  0,
  NA,
  NA,
  106,
  0
)


potter <- potter %>%
  mutate(
    twinID = case_when(
      name == "Fred Weasley" ~ 13,
      name == "George Weasley" ~ 12,
      TRUE ~ NA_real_
    ),
    zygosity = case_when(
      !is.na(twinID) ~ "mz",
      TRUE ~ NA_character_
    )
  )

# potter[nrow(potter) + 1,] <- list(personID,fam,name,gen,
#                                momID,dadID,spouseID,sex)


checkis_acyclic <- checkPedigreeNetwork(potter,
  personID = "personID",
  momID = "momID",
  dadID = "dadID",
  verbose = TRUE
)
checkis_acyclic
if (checkis_acyclic$is_acyclic) {
  message("The pedigree is acyclic.")
  write_csv(potter, here("data-raw", "potter.csv"))
  usethis::use_data(potter, overwrite = TRUE, compress = "xz")
} else {
  message("The pedigree contains cyclic relationships.")
}
