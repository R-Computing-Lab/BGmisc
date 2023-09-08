#' plotPedigree
#' A wrapped function to plot simulated pedigree from function \code{simulatePedigree}. This function require the installation of package \code{kinship2}.
#' @import kinship2
#' @param ped The simulated pedigree data.frame from function \code{simulatePedigree}. Or a pedigree dataframe with the same colnames as the dataframe simulated from function \code{simulatePedigree}.
#' @param cex The font size of the IDs for each individual in the plot.
#' @param code_male This optional input allows you to indicate what value in the sex variable codes for male.
#' @inheritParams kinship2::plot.pedigree
#' @return A plot of the simulated pedigree
#' @export

plotPedigree <- function(ped,
                         # optional data management
                         code_male = NULL,
                         # optional inputs for the pedigree plot
                         cex = .5,
                         col = 1,
                         symbolsize = 1, branch = 0.6,
                         packed = TRUE, align = c(1.5, 2), width = 8,
                         density = c(-1, 35, 65, 20), # mar=c(3.1, 1, 3.1, 1),
                         angle = c(90, 65, 40, 0), keep.par = FALSE,
                         pconnect = .5,
                         ...) {
  # library(kinship2)
  # check if simulated ped object
  simulated_vars <- c("fam", "ID", "gen", "dadID", "momID", "spt", "sex")

  # Check if dataframe matches the criteria
  if (identical(sort(names(ped)), sort(simulated_vars))) {
  p <- ped[, c("fam", "ID", "dadID", "momID","sex")]
  colnames(p) <- c("ped", "id", "father", "mother", "sex")
  # data conversation
  p[is.na(p)] <- 0
  p$affected <- 0
  p$avail <- 0
  if(!is.null(code_male)){
    p$sex_recode <- "F"
    p$sex_recode[p$sex==code_male] <- "M"
  }else{
    p$sex_recode <- p$sex
  }
 # family id
  if(length(unique(p$fam))==1){ # only one family
    p$ped <- 1
   }else{
     # Assign a unique string pattern "fam #" for each unique family
     unique_families <- unique(p$fam)
     named_families <- 1:length(unique_families)
     p$ped <- named_families[match(p$fam, unique_families)]
  }

  p2 <- kinship2::pedigree(
    id = p$id,
    dadid = p$father,
    momid = p$mother,
    sex = p$sex_recode,
    famid = p$ped
  )
  p3 <- p2["1"]
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
    pconnect = pconnect
  ))
  } else {
    stop("The structure of the provided pedigree data does not match the expected structure.")
  }
}

# plotPedigree(simulatePedigree(kpc = 2, Ngen = 6, marR = .8))
