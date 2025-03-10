com2links <- function(
    rel_pairs_file = "dataRelatedPairs.csv",
    ad_ped_matrix,
    mt_ped_matrix,
    cn_ped_matrix#,
 #   pat_ped_matrix,
#    mat_ped_matrix,
# mapa_id_file <- "data_mapaID.csv"
) {
  require(Matrix)
  require(igraph)

  # Convert CN matrix to symmetric if needed
  cn_ped_matrix <- as(cn_ped_matrix, "symmetricMatrix")

  # Ensure mitochondrial matrix values are binary (0/1)
  mt_ped_matrix@x[mt_ped_matrix@x > 0] <- 1

  # File names
  rel_pairs_file <- paste0(outcome_name, "_dataRelatedPairs.csv")
 # mapa_id_file <- paste0(outcome_name, "_data_mapaID.csv")

  # Initialize related pairs file
  write.table(
    data.frame(ID1 = numeric(0), ID2 = numeric(0), addRel = numeric(0), mitRel = numeric(0), cnuRel = numeric(0)),
    file = rel_pairs_file, sep = ",", append = FALSE, row.names = FALSE
  )

  # Extract IDs
  ids <- as.numeric(dimnames(cn_ped_matrix)[[1]])

  # Matrix index adjustments
  newColPos1 <- ad_ped_matrix@p + 1L
  iss1 <- ad_ped_matrix@i + 1L
  newColPos2 <- mt_ped_matrix@p + 1L
  iss2 <- mt_ped_matrix@i + 1L
  newColPos3 <- cn_ped_matrix@p + 1L
  iss3 <- cn_ped_matrix@i + 1L
  nc <- ncol(ad_ped_matrix)

  for (j in 1L:nc) {
    ID2 <- ids[j]

    # Extract column indices
    ncp1 <- newColPos1[j]
    ncp1p <- newColPos1[j + 1L]
    cond1 <- ncp1 < ncp1p
    if (cond1) {
      vv1 <- ncp1:(ncp1p - 1L)
      iss1vv <- iss1[vv1]
    }

    ncp2 <- newColPos2[j]
    ncp2p <- newColPos2[j + 1L]
    cond2 <- ncp2 < ncp2p
    if (cond2) {
      vv2 <- ncp2:(ncp2p - 1L)
      iss2vv <- iss2[vv2]
    }

    ncp3 <- newColPos3[j]
    ncp3p <- newColPos3[j + 1L]
    cond3 <- ncp3 < ncp3p
    if (cond3) {
      vv3 <- ncp3:(ncp3p - 1L)
      iss3vv <- iss3[vv3]
    }

    u <- sort(igraph::union(igraph::union(if (cond1) { iss1vv }, if (cond2) { iss2vv }), if (cond3) { iss3vv }))

    if (cond1 || cond2 || cond3) {
      ID1 <- ids[u]
      tds <- data.frame(ID1 = ID1, ID2 = ID2, addRel = 0, mitRel = 0, cnuRel = 0)

      if (cond1) { tds$addRel[u %in% iss1vv] <- ad_ped_matrix@x[vv1] }
      if (cond2) { tds$mitRel[u %in% iss2vv] <- mt_ped_matrix@x[vv2] }
      if (cond3) { tds$cnuRel[u %in% iss3vv] <- cn_ped_matrix@x[vv3] }

      write.table(tds, file = rel_pairs_file, row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
    }

    if (!(j %% 500)) { cat(paste0("Done with ", j, " of ", nc, "\n")) }
  }

  # Merge and write the parentage matrices
  df <- full_join(mat_ped_matrix %>% arrange(ID), pat_ped_matrix %>% arrange(ID))

  write.table(df, file = mapa_id_file, sep = ",", append = FALSE, row.names = FALSE)
}
