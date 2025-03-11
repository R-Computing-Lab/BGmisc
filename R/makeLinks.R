#' Take a component and turn it into kinship links
#' @param rel_pairs_file File to write related pairs to
#' @param ad_ped_matrix Matrix of additive genetic relatedness coefficients
#' @param mit_ped_matrix Matrix of mitochondrial relatedness coefficients
#' @param cn_ped_matrix Matrix of common nuclear relatedness coefficients
#' @param pat_ped_matrix Matrix of paternal relatedness coefficients
#' @param mat_ped_matrix Matrix of maternal relatedness coefficients
#' @param mapa_id_file File to write the map of parental IDs to individual IDs
#' @param gc logical. If TRUE, do frequent garbage collection via \code{\link{gc}} to save memory
#' @param writetodisk logical. If TRUE, write the related pairs to disk
#' @param verbose logical. If TRUE, print progress messages
#' @param ... Additional arguments to be passed to \code{\link{com2links}}
#' @return A data frame of related pairs

com2links <- function(
    rel_pairs_file = "dataRelatedPairs.csv",
    ad_ped_matrix = NULL,
    mit_ped_matrix = NULL,
    cn_ped_matrix = NULL,
    pat_ped_matrix = NULL,
    mat_ped_matrix = NULL,
    mapa_id_file = "data_mapaID.csv",
    gc = TRUE,
    writetodisk = TRUE,
    verbose = TRUE,
    ...) {
  require(Matrix)
  require(igraph)

  # Fast fails

  ## Check for deprecated arguments
  if (exists("mt_ped_matrix", inherits = F)) {
    mit_ped_matrix <- mt_ped_matrix
    remove(mt_ped_matrix)
  }

  # Ensure at least one relationship matrix is provided
  if (is.null(ad_ped_matrix) && is.null(mit_ped_matrix) && is.null(cn_ped_matrix)) {
    stop("At least one of 'ped_matrix', 'mit_ped_matrix', or 'cn_ped_matrix' must be provided.")
  }
  # Check for matrix type
  if (!is.null(ad_ped_matrix)) {
    if (!inherits(ad_ped_matrix, c("matrix", "dgCMatrix", "dsCMatrix"))) {
      stop("The 'ad_ped_matrix' must be a matrix or dgCMatrix.")
    }
  }
  if (!is.null(cn_ped_matrix)) {
    if (!inherits(ad_ped_matrix, c("matrix", "dgCMatrix", "dsCMatrix")) {
      stop("The 'cn_ped_matrix' must be a matrix or dgCMatrix.")
    }
  }
  if (!is.null(mit_ped_matrix)) {
    if (!inherits(ad_ped_matrix, c("matrix", "dgCMatrix", "dsCMatrix")) {
      stop("The 'mit_ped_matrix' must be a matrix or dgCMatrix.")
    }
    if (!inherits(mit_ped_matrix, "dgCMatrix")) {
      mit_ped_matrix <- as(mit_ped_matrix, "dgCMatrix")
    }
    # Ensure mitochondrial matrix values are binary (0/1)
    mit_ped_matrix@x[mit_ped_matrix@x > 0] <- 1
  }

  # build IDs
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
  } else if (!is.null(mit_ped_matrix)) {
    ids <- as.numeric(dimnames(mit_ped_matrix)[[1]])
    nc <- ncol(mit_ped_matrix)
  }

  if (is.null(ids)) {
    stop("Could not extract IDs from the provided matrices.")
  }

  # check which matrices are provided
  sum_nulls <- sum(!is.null(ad_ped_matrix),
    !is.null(mit_ped_matrix),
    !is.null(cn_ped_matrix),
    na.rm = TRUE
  )
if(verbose){
  print(sum_nulls)
}
  # Extract matrix pointers (directly)
  if (!is.null(ad_ped_matrix)) {
    ad_ped_p <- ad_ped_matrix@p + 1L
    ad_ped_i <- ad_ped_matrix@i + 1L
    ad_ped_x <- ad_ped_matrix@x
  }
  if (!is.null(mit_ped_matrix)) {
    mt_p <- mit_ped_matrix@p + 1L
    mt_i <- mit_ped_matrix@i + 1L
    mt_x <- mit_ped_matrix@x
  }
  if (!is.null(cn_ped_matrix)) {
    cn_p <- cn_ped_matrix@p + 1L
    cn_i <- cn_ped_matrix@i + 1L
    cn_x <- cn_ped_matrix@x
  }

  # if all matrices are provided
  if (sum_nulls == 3) {
    # Matrix index adjustments
    newColPos1 <- ad_ped_p
    iss1 <- ad_ped_i
    x1 <- ad_ped_x
    newColPos2 <- mt_p
    iss2 <- mt_i
    x2 <- mt_x
    newColPos3 <- cn_p
    iss3 <- cn_i
    x3 <- cn_x
    # cleanup
    relNames <- c("addRel", "mitRel", "cnuRel")
    if (gc == TRUE) {
      remove(ad_ped_p, ad_ped_i, ad_ped_x, mt_p, mt_i, mt_x, cn_p, cn_i, cn_x)
    }
    if(verbose){
      print("All 3 matrix is present")
    }

    # File names
    #  rel_pairs_file <- paste0(outcome_name, "_dataRelatedPairs.csv")
    # mapa_id_file <- paste0(outcome_name, "_data_mapaID.csv")

    # Initialize related pairs file
    df_relpairs <- data.frame(
      ID1 = numeric(0), ID2 = numeric(0)
    )
    df_relpairs[[relNames[1]]] <- numeric(0)
    df_relpairs[[relNames[2]]] <- numeric(0)
    df_relpairs[[relNames[3]]] <- numeric(0)
    if (writetodisk == TRUE) {
      write.table(
        df_relpairs,
        file = rel_pairs_file, sep = ",", append = FALSE, row.names = FALSE
      )
      remove(df_relpairs)
    }
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

      u <- sort(igraph::union(igraph::union(if (cond1) {
        iss1vv
      }, if (cond2) {
        iss2vv
      }), if (cond3) {
        iss3vv
      }))

      if (cond1 || cond2 || cond3) {
        ID1 <- ids[u]
        tds <- data.frame(ID1 = ID1, ID2 = ID2)
        tds[[relNames[1]]] <- 0
        tds[[relNames[2]]] <- 0
        tds[[relNames[3]]] <- 0

        if (cond1) {
          tds[u %in% iss1vv, relNames[1]] <- x1[vv1]
        }
        if (cond2) {
          tds[u %in% iss2vv, relNames[2]] <- x2[vv2]
        }
        if (cond3) {
          tds[u %in% iss3vv, relNames[3]] <- x3[vv3]
        }
        if (writetodisk == TRUE) {
          write.table(tds,
            file = rel_pairs_file, row.names = FALSE,
            col.names = FALSE, append = TRUE, sep = ","
          )
        } else {
          df_relpairs <- rbind(df_relpairs, tds)
        }
      }

      if (!(j %% 500)) {
        cat(paste0("Done with ", j, " of ", nc, "\n"))
      }
    }
  } else if (sum_nulls == 2) {
    # one matrix is missing
    if (is.null(ad_ped_matrix)) {
      newColPos1 <- mt_p
      iss1 <- mt_i
      x1 <- mt_x
      newColPos2 <- cn_p
      iss2 <- cn_i
      x2 <- cn_x
      relNames <- c("mitRel", "cnuRel")
      if (gc == TRUE) {
        remove(mt_p, mt_i, mt_x, cn_p, cn_i, cn_x)
      }
    }
    if (is.null(mit_ped_matrix)) {
      newColPos1 <- ad_ped_p
      iss1 <- ad_ped_i
      x1 <- ad_ped_x
      newColPos2 <- cn_p
      iss2 <- cn_i
      x2 <- cn_x
      relNames <- c("addRel", "cnuRel")
      if (gc == TRUE) {
        remove(ad_ped_p, ad_ped_i, ad_ped_x, cn_p, cn_i, cn_x)
      }
    }
    if (is.null(cn_ped_matrix)) {
      newColPos1 <- ad_ped_p
      iss1 <- ad_ped_i
      x1 <- ad_ped_x
      newColPos2 <- mt_p
      iss2 <- mt_i
      x2 <- mt_x
      relNames <- c("addRel", "mitRel")
      if (gc == TRUE) {
        remove(ad_ped_p, ad_ped_i, ad_ped_x, mt_p, mt_i, mt_x)
      }
    }
    # Matrix index adjustments
    # Initialize related pairs file
    df_relpairs <- data.frame(
      ID1 = numeric(0), ID2 = numeric(0)
    )
    df_relpairs[[relNames[1]]] <- numeric(0)
    df_relpairs[[relNames[2]]] <- numeric(0)
    if (writetodisk == TRUE) {
      write.table(
        df_relpairs,
        file = rel_pairs_file, sep = ",", append = FALSE, row.names = FALSE
      )
      remove(df_relpairs)
    }
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



      u <- sort(igraph::union(if (cond1) {
        iss1vv
      }, if (cond2) {
        iss2vv
      }))

      if (cond1 || cond2) {
        ID1 <- ids[u]
        tds <- data.frame(ID1 = ID1, ID2 = ID2)
        tds[[relNames[1]]] <- 0
        tds[[relNames[2]]] <- 0

        if (cond1) {
          tds[u %in% iss1vv, relNames[1]] <- x1[vv1]
        }
        if (cond2) {
          tds[u %in% iss2vv, relNames[2]] <- x2[vv2]
        }
        if (writetodisk == TRUE) {
          write.table(tds,
            file = rel_pairs_file, row.names = FALSE,
            col.names = FALSE, append = TRUE, sep = ","
          )
        } else {
          df_relpairs <- rbind(df_relpairs, tds)
        }
      }

      if (!(j %% 500)) {
        cat(paste0("Done with ", j, " of ", nc, "\n"))
      }
    }
  } else if (sum_nulls == 1) {
    print("Only one matrix is present")

    if (!is.null(ad_ped_matrix)) {
      newColPos1 <- ad_ped_p
      iss1 <- ad_ped_i
      x1 <- ad_ped_x
      relNames <- c("addRel")
      if (gc == TRUE) {
        remove(ad_ped_p, ad_ped_i, ad_ped_x)
      }
    }
    if (!is.null(mit_ped_matrix)) {
      newColPos1 <- mt_p
      iss1 <- mt_i
      x1 <- mt_x
      relNames <- c("mitRel")
      if (gc == TRUE) {
        remove(mt_p, mt_i, mt_x)
      }
    }
    if (!is.null(cn_ped_matrix)) {
      newColPos1 <- cn_p
      iss1 <- cn_i
      x1 <- cn_x
      relNames <- c("cnuRel")
      if (gc == TRUE) {
        remove(cn_p, cn_i, cn_x)
      }
    }

    # Initialize related pairs file
    df_relpairs <- data.frame(
      ID1 = numeric(0), ID2 = numeric(0)
    )
    df_relpairs[[relNames[1]]] <- numeric(0)
    if (writetodisk == TRUE) {
      write.table(
        df_relpairs,
        file = rel_pairs_file, sep = ",", append = FALSE, row.names = FALSE
      )
      remove(df_relpairs)
    }

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

      u <- sort(iss1vv)

      if (cond1) {
        ID1 <- ids[u]
        tds <- data.frame(ID1 = ID1, ID2 = ID2)
        tds[[relNames[1]]] <- 0

        if (cond1) {
          tds[u %in% iss1vv, relNames[1]] <- x1[vv1]
        }
        if (writetodisk == TRUE) {
          write.table(tds,
            file = rel_pairs_file, row.names = FALSE,
            col.names = FALSE, append = TRUE, sep = ","
          )
        } else {
          df_relpairs <- rbind(df_relpairs, tds)
        }
      }

      if (!(j %% 500)) {
        cat(paste0("Done with ", j, " of ", nc, "\n"))
      }
    }


  }
  if (writetodisk == FALSE) {
    return(df_relpairs)
  }
  # Merge and write the parentage matrices
  #  df <- full_join(mat_ped_matrix %>% arrange(ID), pat_ped_matrix %>% arrange(ID))

  #  write.table(df, file = mapa_id_file, sep = ",", append = FALSE, row.names = FALSE)
}
