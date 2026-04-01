# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(usethis)
library(readr)

#  https://doi.org/10.6084/m9.figshare.31513204.v2
ped <- read_delim("data-raw/Pedigree4G.csv",
  delim = ";", escape_double = FALSE,
  trim_ws = TRUE
) %>%
  janitor::clean_names() %>%
  rename(
    ID = id,
    dadID = sire,
    momID = dam
  )


pheno <- read_delim("data-raw/FenotiposPesos90.csv",
  col_types = cols(p0 = col_character()),
  delim = ";", escape_double = FALSE, trim_ws = TRUE
) %>%
  janitor::clean_names() %>%
  rename(
    ID = id,
    dadID = sire_id,
    momID = dam_id
  )

# check the distribution of p0
pheno %>%
  pull(p0) %>%
  table()


pheno <- pheno %>%
  mutate(
    p0 = as.double(case_when(
      p0 == "ABORTO" ~ NA_character_,
      TRUE ~ p0
    ))
  )



ped_pheno <- ped %>%
  full_join(pheno, by = c("ID", "dadID", "momID"))

# find the duplicated IDs in ped_pheno
dup_id <- ped_pheno %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  pull(ID)

dup_id_df <- ped_pheno %>%
  filter(ID %in% dup_id) %>%
  arrange(ID)


ped_growth <- ped_pheno %>%
  filter(!is.na(p0)) %>%
  select(
    ID, sexo, dadID, momID,
    p0, p15, p30, p45, p60, p90
  ) %>%
  pivot_longer(
    cols = starts_with("p"), names_to = "day",
    values_to = "weight"
  ) %>%
  mutate(day = as.numeric(str_remove(day, "p")))

# growth graph over time p0, p15, p30, p45, p60, p90, where 0 is day 0, 15 is day 15, etc.



library(ggplot2)
if (F) {
  pl <- ggplot(
    ped_growth %>% filter(ID %in% sample(unique(ped_growth$ID), 1000)),
    aes(x = day, y = weight, group = ID)
  ) +
    geom_line(alpha = 0.1) +
    geom_jitter(alpha = 0.05, width = 1, height = 0) +
    geom_smooth(method = "loess", se = TRUE, aes(group = sexo, color = sexo)) +
    labs(
      title = "Growth Trajectories of Individuals Over Time",
      x = "Day",
      y = "Weight"
    ) +
    theme_minimal() +
    # nicer colors
    scale_color_discrete(palette = "Set1")
  ##
  pl
}
# data processing

write.csv(ped_pheno, "data-raw/ped_pheno.csv", row.names = FALSE)
# usethis::use_data(ped_pheno, overwrite = TRUE, compress = "xz")
