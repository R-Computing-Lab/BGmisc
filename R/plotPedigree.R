#' plotPedigree
#' A wrapped function to plot simulated pedigree from function \code{simulatePedigree}. This function require the installation of package \code{kinship2}.
#' @import kinship2
#' @param ped The simulated pedigree data.frame from function \code{simulatePedigree}. Or a pedigree dataframe with the same colnames as the dataframe simulated from function \code{simulatePedigree}.
#' @param cex The font size of the IDs for each individual in the plot.
#' @param quiet Logical. If TRUE, suppresses the printing of the pedigree object. Default is TRUE.
#' @param code_male This optional input allows you to indicate what value in the sex variable codes for male.
#' @inheritParams kinship2::plot.pedigree
#' @return A plot of the simulated pedigree
#' @export

plotPedigree <- function(ped,
                         # optional data management
                         code_male = NULL,
                         quiet = TRUE,
                         # optional inputs for the pedigree plot
                         cex = .5,
                         col = 1,
                         symbolsize = 1, branch = 0.6,
                         packed = TRUE, align = c(1.5, 2), width = 8,
                         density = c(-1, 35, 65, 20), mar=c(2.1, 1, 2.1, 1),
                         angle = c(90, 65, 40, 0), keep.par = FALSE,
                         pconnect = .5,
                         ...) {

  # Standardize column names in the input dataframe
  ped <- standardize_colnames(ped)

  # Define required columns
  simulated_vars <- c("fam", "ID", "dadID", "momID", "sex")

  # Check if dataframe contains the required columns
  if (all(simulated_vars %in% names(ped))) {
    p <- ped[, c("fam", "ID", "dadID", "momID", "sex")]
    colnames(p) <- c("ped", "id", "father", "mother", "sex")

    # data conversation
    p[is.na(p)] <- 0
    p$affected <- 0
    p$avail <- 0
    # recode sex values
    if (!is.null(code_male)) {
      p$sex_recode <- "F"
      p$sex_recode[p$sex == code_male] <- "M"
    } else {
      p$sex_recode <- p$sex
    }
    # family id
    if (length(unique(p$ped)) == 1) { # only one family
      p$ped <- 1

    } else {
     # Assign a unique string pattern "ped #" for each unique family
      unique_families <- unique(p$ped)
      named_families <- 1:length(unique_families)
      p$ped <- named_families[match(p$ped, unique_families)]
    }
    p2 <- kinship2::pedigree(
      id = p$id,
      dadid = p$father,
      momid = p$mother,
      sex = p$sex_recode,
      famid = p$ped
    )
    p3 <- p2["1"]
    if (!quiet) {
      print(p3)
    }

    return(kinship2::plot.pedigree(p3,
      cex = cex,
      col = col,
      symbolsize = symbolsize,
      branch = branch,
      packed = packed, align = align,
      width = width,
      density = density,
      angle = angle, keep.par = keep.par,
      pconnect = pconnect,
      mar = mar
    ))
  } else {
    stop("The structure of the provided pedigree data does not match the expected structure.")
  }
}

# plotPedigree(simulatePedigree(kpc = 2, Ngen = 6, marR = .8))
