#' PlotPedigree
#' A wrapped function to plot simulated pedigree from function \code{SimPed}. This function require the installation of package \code{kinship2}.
#' @import kinship2
#' @param ped The simulated pedigree data.frame from function \code{SimPed}. Or a pedigree dataframe with the same colnames as the dataframe simulated from function \code{SimPed}.
#' @param cex The font size of the IDs for each individual in the plot.
#' @return A plot of the simulated pedigree
#' @export

PlotPedigree <- function(ped, cex = .5){
      #library(kinship2)
      p <- ped[,-c(3,6)]
      colnames(p) <- c("ped","id","father","mother","sex")
      p[is.na(p)] <- 0
      p$ped <- 1
      p$affected <- 0
      p$avail <- 0
      p2 <- pedigree(id=p$id,
                     dadid=p$father,
                     momid=p$mother,
                     sex=p$sex,
                     famid=p$ped)
      p3 <- p2['1']
      print(p3)
      return(plot.pedigree(p3,
                           cex = cex))
}

#PlotPedigree(SimPed(kpc = 2, Ngen = 6, marR = .8))
