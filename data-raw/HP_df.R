
#devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)


## Create dataframe

hp_ped <- data.frame(
  ID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
         11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
         21, 22, 23, 24, 25, 26, 27, 28, 29, 30),
  FamID = c("fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1", "fam1"),
  name = c("Vernon Dursley",
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
           "Audrey UNKNOWN",
           "James Potter II",
           "Albus Potter",
           "Lily Potter",
           "Rose Weasley",
           "Hugo Weasley",
           "Victoire Weasley",
           "Dominique Weasley",
           "Louis Weasley",
           "Molly Weasley",
           "Lucy Weasley"),
  gen = c(1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3),
  motherID = c(101, 101, 103, 103, NA, 3, 4, 10, NA, NA, 10, 10, 10, 10, 10, 10, NA, 105, 105, NA, 8, 8, 8, 17, 17, 18, 18, 18, 20, 20),
  fatherID = c(102, 102, 104, 104, NA, 1, 5, 9, NA, NA, 9, 9, 9, 9, 9, 9, NA, 106, 106, NA, 7, 7, 7, 11, 11, 16, 16, 16, 14, 14),
  spouseID = c(3, NA, 1, 5, 4, NA, 8, 7, 10, 9, 17, NA, NA, 20, NA, 18, 11, 16, NA, 14, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  sex = c(1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0)
) #sex: 0 = female, 1 = male 


# audrey weasley - unknown maiden name
# James Potter II might not be correct, but I used it to differentiate grandson from grandfather


### Add additional people


hp_ped[nrow(hp_ped) + 1,] <- list(101,
                                  "fam1",
                                  "Mother Durseley",
                                  0,
                                  NA,
                                  NA,
                                  102,
                                  0)
hp_ped[nrow(hp_ped) + 1,] <- list(102,
                                  "fam1",
                                  "Father Durseley",
                                  0,
                                  NA,
                                  NA,
                                  101,
                                  1)
hp_ped[nrow(hp_ped) + 1,] <- list(104,
                                  "fam1",
                                  "Father Evans",
                                  0,
                                  NA,
                                  NA,
                                  103,
                                  1)
hp_ped[nrow(hp_ped) + 1,] <- list(103,
                                  "fam1",
                                  "Mother Evans",
                                  0,
                                  NA,
                                  NA,
                                  104,
                                  0)
hp_ped[nrow(hp_ped) + 1,] <- list(106,
                                  "fam1",
                                  "Father Delacour",
                                  0,
                                  NA,
                                  NA,
                                  105,
                                  1)
hp_ped[nrow(hp_ped) + 1,] <- list(105,
                                  "fam1",
                                  "Mother Delacour",
                                  0,
                                  NA,
                                  NA,
                                  106,
                                  0)

#hp_ped[nrow(hp_ped) + 1,] <- list(ID,FamID,name,gen,motherID,fatherID,spouseID,sex)

write_csv(hp_ped, "hp_ped.csv")
usethis::use_data(hp_ped, overwrite = TRUE, compress = 'xz')
