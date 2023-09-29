#' allGens
#' A function to calculate the number of individuals in each generation. This is a supporting function for \code{simulatePedigree}.
#' @param kpc Number of kids per couple (integer >= 2).
#' @param Ngen Number of generations (integer >= 1).
#' @param marR Mating rate (numeric value ranging from 0 to 1).
#' @return Returns a vector containing the number of individuals in every generation.
#' @export
allGens <- function(kpc, Ngen, marR) {
  # Check if the number of generations is valid
  if (Ngen < 1) {
    stop("The number of generations should be an integer greater or equal than 1")
  }
  if (Ngen == 1) {
    allGens <- 2
  }
  if (Ngen >= 2) {
    allGens <- sizeAllGens(
      kpc = kpc,
      Ngen = Ngen,
      marR = marR
    )
  } else {
    stop()
  }
  return(allGens)
}


#' sizeAllGens
#' An internal supporting function for \code{simulatePedigree}.
#' @inheritParams allGens
#' @return Returns a vector including the number of individuals in every generation.

sizeAllGens <- function(kpc, Ngen, marR) {
  Nmid <- Ngen - 2
  midGens <- numeric(length = Nmid)

  # Calculate the number of individuals for middle generations
  for (i in 2:(Ngen - 1)) {
    midGens[i - 1] <- kpc^(i - 1) * marR^(i - 2) * (1 + marR)
    midGens[i - 1] <- ceiling(midGens[i - 1])
  }

  # Calculate the number of individuals for the last generation
  lastGen <- ceiling(kpc^(Ngen - 1) * marR^(Ngen - 2))
  allGens <- c(2, midGens, lastGen)
  # print(allGens)
  return(allGens)
}


#' famSizeCal
#' A function to calculate the total number of individuals in a pedigree given parameters. This is a supporting function for function \code{simulatePedigree}
#' @inheritParams allGens
#' @return Returns a numeric value indicating the total pedigree size.
#' @export
famSizeCal <- function(kpc, Ngen, marR) {
  if (Ngen < 1) {
    stop("The number of generations should be an integer greater or equal than 1")
  }
  if (Ngen == 1) {
    size <- 2
  }
  if (Ngen >= 2) {
    allGens <- sizeAllGens(
      kpc = kpc,
      Ngen = Ngen,
      marR = marR
    )
    size <- sum(allGens)
  } else {
    stop("You should never see this message.
    If you do, that means that famSizeCal is not working properly.")
  }
  return(size)
}
