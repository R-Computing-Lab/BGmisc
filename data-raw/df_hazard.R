# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(usethis)

raw <- read.csv("data-raw/hazard.csv", header = TRUE, sep = ",")

hazard <- raw
##
# data processing

#write.csv(hazard, "data-raw/hazard.csv", row.names = FALSE)
usethis::use_data(hazard, overwrite = TRUE, compress = "xz")
