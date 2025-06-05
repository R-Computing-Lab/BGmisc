# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(here)
library(readr)
library(usethis)
library(BGmisc)
## Create dataframe

potter <- data.frame(
  personID = c(
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
    21, 22, 23, 24, 25, 26, 27, 28, 29, 30
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

write_csv(potter, here("data-raw", "potter.csv"))
usethis::use_data(potter, overwrite = TRUE, compress = "xz")


if (FALSE) {
  ## Create dataframe
  potter_big <- readGedcom("data-raw/potter_big.ged")


  df <- ped2fam(potter_big, personID = "personID") %>%
    select(
      -name_given,
      -name_surn,
      -death_date,
      -birth_date,
      -FAMC,
      -FAMS
    ) %>%
    # rename(personID = id) %>%
    mutate(
      personID = as.numeric(personID),
      momID = as.numeric(momID),
      dadID = as.numeric(dadID),
      name = str_remove(name, "/")
    ) %>%
    arrange(name)


  potter_clean <- potter %>%
    arrange(name) %>%
    mutate(
      personID = as.numeric(personID) * 1000,
      momID = as.numeric(momID) * 1000,
      dadID = as.numeric(dadID) * 1000
    )

  # merge  so that I have all of A and all the Bs that match A

  df_clean <- df %>%
    mutate(name = str_trim(str_to_lower(name)))



  potter_clean <- potter_clean %>%
    mutate(
      personID = case_match(
        personID,
        22000 ~ 15,
        9000 ~ 18,
        6000 ~ 10,
        18000 ~ 29,
        7000 ~ 1,
        17000 ~ 25,
        25000 ~ 27,
        13000 ~ 20,
        19000 ~ 34,
        23000 ~ 16,
        10000 ~ 17,
        14000 ~ 22,
        3000 ~ 8,
        24000 ~ 26,
        1000 ~ 9,
        26000 ~ 30,
        21000 ~ 14,
        11000 ~ 19,
        8000 ~ 13,
        5000 ~ 2,
        15000 ~ 23,
        16000 ~ 24,
        4000 ~ 3,
        2000 ~ -2000,
        101000 ~ 12,
        102000 ~ 11,
        20000 ~ -20000,
        27000 ~ -27000,
        106000 ~ 32,
        105000 ~ 33,
        29000 ~ 17,
        103000 ~ 7,
        104000 ~ 6,
        12000 ~ 21,
        30000 ~ -30000,
        28000 ~ -28000,
        .default = personID
      ), dadID =
        case_match(
          dadID,
          22000 ~ 15,
          9000 ~ 18,
          6000 ~ 10,
          18000 ~ 29,
          7000 ~ 1,
          17000 ~ 25,
          25000 ~ 27,
          13000 ~ 20,
          19000 ~ 34,
          23000 ~ 16,
          10000 ~ 17,
          14000 ~ 22,
          3000 ~ 8,
          24000 ~ 26,
          1000 ~ 9,
          26000 ~ 30,
          21000 ~ 14,
          11000 ~ 19,
          8000 ~ 13,
          5000 ~ 2,
          15000 ~ 23,
          16000 ~ 24,
          4000 ~ 3,
          2000 ~ 2000,
          101000 ~ 12,
          102000 ~ 11,
          20000 ~ 20000,
          27000 ~ 27000,
          106000 ~ 32,
          105000 ~ 33,
          29000 ~ 17,
          103000 ~ 7,
          104000 ~ 6,
          12000 ~ 21,
          30000 ~ 30000,
          28000 ~ 28000,
          .default = dadID
        ), momID =
        case_match(
          momID,
          22000 ~ 15,
          9000 ~ 18,
          6000 ~ 10,
          18000 ~ 29,
          7000 ~ 1,
          17000 ~ 25,
          25000 ~ 27,
          13000 ~ 20,
          19000 ~ 34,
          23000 ~ 16,
          10000 ~ 17,
          14000 ~ 22,
          3000 ~ 8,
          24000 ~ 26,
          1000 ~ 9,
          26000 ~ 30,
          21000 ~ 14,
          11000 ~ 19,
          8000 ~ 13,
          5000 ~ 2,
          15000 ~ 23,
          16000 ~ 24,
          4000 ~ 3,
          2000 ~ 2000,
          101000 ~ 12,
          102000 ~ 11,
          20000 ~ 20000,
          27000 ~ 27000,
          106000 ~ 32,
          105000 ~ 33,
          29000 ~ 17,
          103000 ~ 7,
          104000 ~ 6,
          12000 ~ 21,
          30000 ~ 30000,
          28000 ~ 28000,
          .default = momID
        ), spouseID =
        case_match(
          spouseID,
          22000 ~ 15,
          9000 ~ 18,
          6000 ~ 10,
          18000 ~ 29,
          7000 ~ 1,
          17000 ~ 25,
          25000 ~ 27,
          13000 ~ 20,
          19000 ~ 34,
          23000 ~ 16,
          10000 ~ 17,
          14000 ~ 22,
          3000 ~ 8,
          24000 ~ 26,
          1000 ~ 9,
          26000 ~ 30,
          21000 ~ 14,
          11000 ~ 19,
          8000 ~ 13,
          5000 ~ 2,
          15000 ~ 23,
          16000 ~ 24,
          4000 ~ 3,
          2000 ~ 2000,
          101000 ~ 12,
          102000 ~ 11,
          20000 ~ 20000,
          27000 ~ 27000,
          106000 ~ 32,
          105000 ~ 33,
          29000 ~ 17,
          103000 ~ 7,
          104000 ~ 6,
          12000 ~ 21,
          30000 ~ 30000,
          28000 ~ 28000,
          .default = spouseID
        )
    )


  # Left join by name
  potter_join <- potter_clean %>%
    full_join(df_clean, by = c("personID"), suffix = c("", "_df"))
}
