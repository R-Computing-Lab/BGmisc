#' plotPedigree
#' A wrapped function to plot simulated pedigree from function \code{simulatePedigree}. This function require the installation of package \code{kinship2}.
#' @import kinship2
#' @param ped The simulated pedigree data.frame from function \code{simulatePedigree}. Or a pedigree dataframe with the same colnames as the dataframe simulated from function \code{simulatePedigree}.
#' @param cex The font size of the IDs for each individual in the plot.
#' @param verbose logical  If TRUE, prints additional information. Default is FALSE.
#' @param code_male This optional input allows you to indicate what value in the sex variable codes for male. Will be recoded as "M" (Male). If \code{NULL}, no recoding is performed.
#' @inheritParams kinship2::plot.pedigree
#' @return A plot of the simulated pedigree
#' @export

plotPedigree <- function(ped,
                         # optional data management
                         code_male = NULL,
                         verbose = FALSE,
                         # optional inputs for the pedigree plot
                         cex = .5,
                         col = 1,
                         symbolsize = 1, branch = 0.6,
                         packed = TRUE, align = c(1.5, 2), width = 8,
                         density = c(-1, 35, 65, 20), mar = c(2.1, 1, 2.1, 1),
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
    p <- recode_sex(p, code_male = code_male)
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
    if (verbose) {
      print(p3)
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
    } else { # suppress the printing of the pedigree comments
      # Determine the null device based on the OS
      null_device <- ifelse(.Platform$OS.type == "windows", "nul", "/dev/null")

      # Start redirecting the standard output to suppress messages
      sink(file = null_device, type = "output")

      # Ensure the output is reverted back to console when function exits
      on.exit(if(sink.number() > 0) sink(), add = TRUE)

      plot_picture <- kinship2::plot.pedigree(p3,
                                              cex = cex,
                                              col = col,
                                              symbolsize = symbolsize,
                                              branch = branch,
                                              packed = packed, align = align,
                                              width = width,
                                              density = density,
                                              angle = angle, keep.par = keep.par,
                                              pconnect = pconnect,
                                              mar = mar)

      # Explicitly revert the standard output back to the console
      if(sink.number() > 0) {
        sink()
      }

      return(plot_picture)
    }


  } else {
    stop("The structure of the provided pedigree data does not match the expected structure.")
  }
}


