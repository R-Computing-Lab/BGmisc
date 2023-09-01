#' PlotPedigree
#' A wrapped function to plot simulated pedigree from function \code{SimPed}. This function require the installation of package \code{kinship2}.
#' @import kinship2
#' @param ped The simulated pedigree data.frame from function \code{SimPed}. Or a pedigree dataframe with the same colnames as the dataframe simulated from function \code{SimPed}.
#' @param cex The font size of the IDs for each individual in the plot.
#' @inheritParams kinship2::plot.pedigree
#' @return A plot of the simulated pedigree
#' @export

PlotPedigree <- function(ped, cex = .5,
                         # optional inputs for the pedigree plot
                         col = 1,
                         symbolsize = 1, branch = 0.6,
                         packed = TRUE, align = c(1.5, 2), width = 8,
                         density = c(-1, 35, 65, 20), # mar=c(3.1, 1, 3.1, 1),
                         angle = c(90, 65, 40, 0), keep.par = FALSE,
                         pconnect = .5,
                         ...	){
      #library(kinship2)
      p <- ped[,-c(3,6)]
      colnames(p) <- c("ped","id","father","mother","sex")
      p[is.na(p)] <- 0
      p$ped <- 1
      p$affected <- 0
      p$avail <- 0
      p2 <- kinship2::pedigree(id=p$id,
                     dadid=p$father,
                     momid=p$mother,
                     sex=p$sex,
                     famid=p$ped)
      p3 <- p2['1']
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
                           pconnect = pconnect))
}

#PlotPedigree(SimPed(kpc = 2, Ngen = 6, marR = .8))
