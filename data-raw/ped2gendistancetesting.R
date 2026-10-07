library(quadprog)
library(ggpedigree)
library(tidyverse)

hazard$gen_bgmisc <- ped2gen(hazard, mz_twins = F, sparse = FALSE)



ggpedigree(hazard, personID = "ID", code_male = 0)


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
