#' Create Data Frame for Generation
#'
#' This function creates a data frame for a specific generation within the simulated pedigree.
#' It initializes the data frame with default values for family ID, individual ID, generation number,
#' paternal ID, maternal ID, spouse ID, and sex. All individuals are initially set with NA for paternal,
#' maternal, spouse IDs, and sex, awaiting further assignment.
#'
#' @param sizeGens A numeric vector containing the sizes of each generation within the pedigree.
#' @param genIndex An integer representing the current generation index for which the data frame is being created.
#' @param idGen A numeric vector containing the ID numbers to be assigned to individuals in the current generation.
#' @return A data frame representing the initial structure for the individuals in the specified generation
#'         before any relationships (parental, spousal) are defined. The columns include family ID (`fam`),
#'         individual ID (`id`), generation number (`gen`), father's ID (`pat`), mother's ID (`mat`),
#'         spouse's ID (`spt`), and sex (`sex`), with NA values for paternal, maternal, and spouse IDs, and sex.
#' @examples
#' sizeGens <- c(3, 5, 4) # Example sizes for 3 generations
#' genIndex <- 2 # Creating data frame for the 2nd generation
#' idGen <- 101:105 # Example IDs for the 2nd generation
#' df_Ngen <- createGenDataFrame(sizeGens, genIndex, idGen)
#' print(df_Ngen)
#' @export
createGenDataFrame <- function(sizeGens, genIndex, idGen) {
  df_Ngen <- data.frame(
    fam = rep(paste("fam", 1), sizeGens[genIndex], sep = ""),
    id = idGen[1:sizeGens[genIndex]],
    gen = rep(genIndex, sizeGens[genIndex]),
    pat = rep(NA, sizeGens[genIndex]), # father id
    mat = rep(NA, sizeGens[genIndex]), # mother id
    spt = rep(NA, sizeGens[genIndex]), # spouse id
    sex = rep(NA, sizeGens[genIndex])
  )
  return(df_Ngen)
}


#' Determine Sex of Offspring
#'
#' This function assigns sexes to the offspring in a generation based on the specified sex ratio.
#'
#' @param idGen Vector of IDs for the generation.
#' @param sexR Numeric value indicating the sex ratio (proportion of males).
#' @return Vector of sexes ("M" for male, "F" for female) for the offspring.
#' @importFrom stats runif
#' @export
determineSex <- function(idGen, sexR) {
  if (runif(1) > .5) {
    sexVec1 <- rep("M", floor(length(idGen) * sexR))
    sexVec2 <- rep("F", length(idGen) - length(sexVec1))
  } else {
    sexVec1 <- rep("F", floor(length(idGen) * (1 - sexR)))
    sexVec2 <- rep("M", length(idGen) - length(sexVec1))
  }
  sexVec <- sample(c(sexVec1, sexVec2))
  return(sexVec)
}
