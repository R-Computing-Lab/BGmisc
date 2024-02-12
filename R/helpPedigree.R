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
