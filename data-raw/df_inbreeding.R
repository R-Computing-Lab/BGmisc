# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(usethis)

raw <- read.csv("data-raw/inbreeding.csv", header = TRUE, sep = ",")

inbreeding <- raw
##
# data processing

#write.csv(inbreeding, "data-raw/inbreeding.csv", row.names = FALSE)
usethis::use_data(inbreeding, overwrite = TRUE, compress = "xz")
