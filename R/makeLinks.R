com2links <- function(
    rel_pairs_file = "dataRelatedPairs.csv",
    ad_ped_matrix = NULL,
    mt_ped_matrix = NULL,
    cn_ped_matrix = NULL,
  pat_ped_matrix = NULL,
  mat_ped_matrix = NULL,
 mapa_id_file <- "data_mapaID.csv"
) {
  require(Matrix)
  require(igraph)

  # Ensure at least one relationship matrix is provided
  if (is.null(ad_ped_matrix) && is.null(mt_ped_matrix) && is.null(cn_ped_matrix)) {
    stop("At least one of 'ped_matrix', 'mt_ped_matrix', or 'cn_ped_matrix' must be provided.")
  }


  # Extract IDs from the first available matrix
  ids <- NULL
  if (!is.null(cn_ped_matrix)) {
    # Convert CN matrix to symmetric if needed
    cn_ped_matrix <- as(cn_ped_matrix, "symmetricMatrix")
    ids <- as.numeric(dimnames(cn_ped_matrix)[[1]])
    nc <- ncol(cn_ped_matrix)
  } else if (!is.null(ad_ped_matrix)) {
    ids <- as.numeric(dimnames(ad_ped_matrix)[[1]])
    nc <- ncol(ad_ped_matrix)
  } else if (!is.null(mt_ped_matrix)) {
    ids <- as.numeric(dimnames(mt_ped_matrix)[[1]])
    nc <- ncol(mt_ped_matrix)
  }

  if (is.null(ids)) {
    stop("Could not extract IDs from the provided matrices.")
  }
  if (!is.null(mt_ped_matrix)) {
    # Ensure mitochondrial matrix values are binary (0/1)
    mt_ped_matrix@x[mt_ped_matrix@x > 0] <- 1
  }

sum_nulls <- sum(is.null(ad_ped_matrix), is.null(mt_ped_matrix), is.null(cn_ped_matrix))


  # File names
#  rel_pairs_file <- paste0(outcome_name, "_dataRelatedPairs.csv")
 # mapa_id_file <- paste0(outcome_name, "_data_mapaID.csv")

  # Initialize related pairs file
  write.table(
    data.frame(ID1 = numeric(0), ID2 = numeric(0),
               addRel = numeric(0),
               mitRel = numeric(0),
               cnuRel = numeric(0)),
    file = rel_pairs_file, sep = ",", append = FALSE, row.names = FALSE
  )


  # Extract matrix pointers (directly)
  if (!is.null( ad_ped_matrix)){
  ad_ped_p <- ad_ped_matrix@p + 1L
  ad_ped_i <- ad_ped_matrix@i + 1L
  ad_ped_x <- ad_ped_matrix@x
  }
  if (!is.null(mt_ped_matrix)){
  mt_p <- mt_ped_matrix@p + 1L
  mt_i <- mt_ped_matrix@i + 1L
  mt_x <- mt_ped_matrix@x
  }
  if (!is.null(cn_ped_matrix)){
  cn_p <- cn_ped_matrix@p + 1L
  cn_i <- cn_ped_matrix@i + 1L
  cn_x <- cn_ped_matrix@x
  }

  # if all matrices are provided
  if (sum_nulls==3){

  # Matrix index adjustments
  newColPos1 <- ad_ped_p
  iss1 <- ad_ped_i
  x1 <- ad_ped_x
  newColPos2 <-  mt_p
  iss2 <-  mt_i
  x2 <- mt_x
  newColPos3 <- cn_p
  iss3 <- cn_i
  x3 <- cn_x

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

      if (cond1) { tds$addRel[u %in% iss1vv] <- x1[vv1] }
      if (cond2) { tds$mitRel[u %in% iss2vv] <- x2[vv2] }
      if (cond3) { tds$cnuRel[u %in% iss3vv] <- x3[vv3] }

      write.table(tds, file = rel_pairs_file, row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
    }

    if (!(j %% 500)) { cat(paste0("Done with ", j, " of ", nc, "\n")) }
  }
  } else if (sum_nulls==2){
    # one matrix is missing
    if (is.null(ad_ped_matrix)){
      newColPos1 <-  mt_p
      iss1 <-  mt_i
      newColPos2 <- cn_p
      iss2 <- cn_i
    }
    if (is.null(mt_ped_matrix)){
      newColPos1 <- ad_ped_p
      iss1 <- ad_ped_i
      newColPos2 <- cn_p
      iss2 <- cn_i
    }
    if (is.null(cn_ped_matrix)){
      newColPos1 <- ad_ped_p
      iss1 <- ad_ped_i
      newColPos2 <-  mt_p
      iss2 <-  mt_i
    }
    # Matrix index adjustments





  # Merge and write the parentage matrices
#  df <- full_join(mat_ped_matrix %>% arrange(ID), pat_ped_matrix %>% arrange(ID))

#  write.table(df, file = mapa_id_file, sep = ",", append = FALSE, row.names = FALSE)
}
