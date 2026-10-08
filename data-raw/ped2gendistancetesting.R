library(quadprog)
library(ggpedigree)
library(tidyverse)

hazard_recode <- recodeSex(hazard,
 code_male = 0,
 code_na = NA,
 code_female = 1,
  recode_male = 1,
  recode_female = 0,
  recode_unknown = NA,
)

hazard_recode$gen_bgmisc <- ped2gen(hazard_recode, mz_twins = F, sparse = FALSE)

hazard_f1 <- hazard_recode[hazard_recode$famID==1,]



ggpedigree(hazard_f1, personID = "ID",
           config = list(
             code_male = 1,
             code_female = 0,
             recode_missing_sex = FALSE,
             focal_fill_personID = 1,
             focal_fill_include = TRUE,
             #  focal_fill_high_color = "yellow",
             #  focal_fill_mid_color = "red",
             #   focal_fill_low_color = "#0D082AFF",
             focal_fill_force_zero = TRUE,
             focal_fill_na_value = "black",
             focal_fill_scale_midpoint = 0.25,
             focal_fill_component = "patID",
             focal_fill_method = "viridis_d",
             focal_fill_n_breaks = NULL,
             color_theme="black",

             # "additive",
             sex_color_include = FALSE,
             sex_legend_show = FALSE
           ))


# Full pairwise matrix of genealogical path distances

gen_dist_mat <- ped2genDist(
  hazard,
  method = "path"
)

# Keep each pair only once
keep <- upper.tri(gen_dist_mat) & !is.na(gen_dist_mat)

gen_dist <- tibble(
  ID1 = rownames(gen_dist_mat)[row(gen_dist_mat)[keep]],
  ID2 = colnames(gen_dist_mat)[col(gen_dist_mat)[keep]],
  gen_dist = gen_dist_mat[keep]
)

gen_dist


hazard_xmat <- matrix(
  c(
    1,     0,     1,     .50, .50,  .50,  0, .50, .25,  0,   0,   .25, 0, 0, 0,  .125, .125, .125,
    0,     1,     0,     .50, .50,  .50,  0, .50, .25,  0,   0,   .25, 0, 0, 0,  .125, .125, .125,
    1,     0,     1,     .50, .50,  .50,  0, .50, .25,  0,   0,   .25, 0, 0, 0,  .125, .125, .125,
    .50,   .50,   .50,   1,   .50,  .50,  0, 1,   .50,  0,   0,   .50, 0, 0, 0,  .250, .250, .250,
    .50,   .50,   .50,   .50, 1,    .50,  0, .50, .25,  0,   0,   .25, 0, 0, 0,  .125, .125, .125,
    .50,   .50,   .50,   .50, .50,  1,    0, .50, .25,  0,   0,   .25, 0, 0, 0,  .125, .125, .125,
    0,     0,     0,     0,   0,    0,    1, 0,   .50,  0,   0,   0,   0, 0, 0,  0,    0,    0,
    .50,   .50,   .50,   1,   .50,  .50,  0, 1,   .50,  0,   0,   .50, 0, 0, 0,  .250, .250, .250,
    .25,   .25,   .25,   .50, .25,  .25, .50, .50, 1,    0,   0,   .25, 0, 0, 0,  .125, .125, .125,
    0,     0,     0,     0,   0,    0,    0, 0,   0,    1,   1,   .50, 0, 0, 0,  .250, .250, .250,
    0,     0,     0,     0,   0,    0,    0, 0,   0,    1,   1,   .50, 0, 0, 0,  .250, .250, .250,
    .25,   .25,   .25,   .50, .25,  .25,  0, .50, .25,  .50, .50, 1,   0, 0, 0,  .500, .500, .500,
    0,     0,     0,     0,   0,    0,    0, 0,   0,    0,   0,   0,   1, 1, 0,  0,    0,    0,
    0,     0,     0,     0,   0,    0,    0, 0,   0,    0,   0,   0,   1, 1, 0,  0,    0,    0,
    0,     0,     0,     0,   0,    0,    0, 0,   0,    0,   0,   0,   0, 0, 1,  .500, .500, .500,
    .125,  .125,  .125,  .25, .125, .125, 0, .25, .125, .25, .25, .50, 0, 0, .50, 1,   .500, .500,
    .125,  .125,  .125,  .25, .125, .125, 0, .25, .125, .25, .25, .50, 0, 0, .50, .500, 1,   .500,
    .125,  .125,  .125,  .25, .125, .125, 0, .25, .125, .25, .25, .50, 0, 0, .50, .500, .500, 1
  ),
  nrow = 18,
  ncol = 18,
  byrow = TRUE,
  dimnames = list(as.character(1:18), as.character(1:18))
)
