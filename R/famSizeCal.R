#' allGens
#' A function to return a vector for number of individuals in each generation. This is a supporting function for function \code{SimPed}
#' @param kpc kids per couple
#' @param Ngen number of generations
#' @param marR Mating rate.
#' @return Returns a vector including the number of individuals in every generation.
#' @export
allGens <- function(kpc, Ngen, marR){
   if (Ngen < 1){
      stop("The number of generations should be integers greater or equal than 1")
   }
   if (Ngen == 1){
      allGens = 2
   }
   if (Ngen >= 2){
      allGens = sizeAllGens(kpc=kpc, Ngen = Ngen, marR = marR)
   } else{
      stop()
   }
   return(allGens)
}


#' sizeAllGens
#' An internal supporting function for function \code{allGens}
#' @param kpc kids per couple
#' @param Ngen number of generations
#' @param marR Mating rate.
#' @return Returns a vector including the number of individuals in every generation.

sizeAllGens <- function(kpc, Ngen, marR){
   Nmid <- Ngen -2
   midGens <- numeric(length = Nmid)

   for(i in 2 : (Ngen - 1)){
      midGens[i-1] <- kpc^(i-1) * marR^(i-2) * (1+marR)
      midGens[i-1] <- ceiling(midGens[i-1])
   }

   lastGen <- ceiling(kpc^(Ngen-1) * marR^(Ngen-2))
   allGens <- c(2,midGens,lastGen)
   #print(allGens)
   return(allGens)
}


#' allGens
#' A function to calculate the total number of individuals in a pedigree given parameters. This is a supporting function for function \code{SimPed}
#' @param kpc kids per couple
#' @param Ngen number of generations
#' @param marR Mating rate.
#' @return Returns a numeric value indicating the pedigree size.
#' @export
famSizeCal <- function(kpc, Ngen, marR){
    if (Ngen < 1){
        stop("The number of generations should be integers greater or equal than 1")
    }
    if (Ngen == 1){
        size = 2
    }
    if (Ngen >= 2){
        allGens = sizeAllGens(kpc=kpc, Ngen = Ngen, marR = marR)
        size = sum(allGens)
    } else{
        stop()
    }
    return(size)
}
