#' Simulate Pedigrees
#' This function simulates "balanced" pedigrees based on a group of parameters:
#' 1) k - Kids per couple;
#' 2) G - Number of generations;
#' 3) p - Proportion of males in offspring;
#' 4) r - Mating rate.
#'
#' @importFrom stats runif
#' @param kpc Number of kids per couple. An integer >= 2 that determines how
#' many kids each fertilized mated couple will have in the pedigree. Default
#' value is 3. Returns an error when kpc equals 1.
#' @param Ngen Number of generations. An integer >= 2 that determines how many
#' generations the simulated pedigree will have. The first generation is always
#' a fertilized couple. The last generation has no mated individuals.
#' @param sexR Sex ratio of offspring. A numeric value ranging from 0 to 1 that
#' determines the proportion of males in all offspring in this pedigree. For
#' instance, 0.4 means 40 percent of the offspring will be male.
#' @param marR Mating rate. A numeric value ranging from 0 to 1 which determines
#' the proportion of mated (fertilized) couples in the pedigree within each
#' generation. For instance, marR  = 0.5 suggests 50 percent of the offspring in
#'  a specific generation will be  mated and have their offspring.
#' @param rd_kpc logical. If TRUE, the number of kids per mate will be randomly
#' generated from a poisson distribution with mean kpc. If FALSE, the number of
#' kids per mate will be fixed at kpc.
#' @param balancedSex Not fully developed yet. Always \code{TRUE} in the
#' current version.
#' @param balancedMar Not fully developed yet. Always \code{TRUE} in the
#' current version.
#' @param verbose logical  If TRUE, message progress through stages of algorithm
#' @param code_male The value to use for males. Default is "M"
#' @param code_female The value to use for females. Default is "F"
#' @param fam_shift An integer to shift the person ID. Default is 1L.
#' This is useful when simulating multiple pedigrees to avoid ID conflicts.
#' @param beta logical or character. Controls which algorithm version to use:
#'   \itemize{
#'     \item{\code{FALSE}, \code{"base"}, or \code{"original"} (default): Use the original algorithm.
#'           Slower but ensures exact reproducibility with set.seed().}
#'     \item{\code{TRUE} or \code{"optimized"}: Use the optimized algorithm with 4-5x speedup.
#'           Produces statistically equivalent results but not identical to base version
#'           due to different random number consumption. Recommended for large simulations
#'           where speed matters more than exact reproducibility.}
#'   }
#'   Note: Both versions are mathematically correct and produce valid pedigrees with the
#'   same statistical properties (sex ratios, mating rates, etc.). The optimized version
#'   uses vectorized operations instead of loops, making it much faster for large pedigrees.
#' @param ... Additional arguments to be passed to other functions.
#' @inheritParams ped2fam
#' @param spouseID The name of the column that will contain the spouse ID in the output data frame. Default is "spID".
#' @param remap_ids logical. If TRUE, remap all ID columns to sequential integers (1, 2, 3, ...) in row order.
#' @return A \code{data.frame} with each row representing a simulated individual. The columns are as follows:
#' \itemize{
#'   \item{fam: The family id of each simulated individual. It is 'fam1' in a single simulated pedigree.}
#'   \item{ID: The unique personal ID of each simulated individual. The first digit is the fam id; the fourth digit is the generation the individual is in; the following digits represent the order of the individual within their  pedigree. For example, 100411 suggests this individual has a family id of 1, is in the 4th generation, and is the 11th individual in the 4th generation.}
#'   \item{gen: The generation the simulated individual is in.}
#'   \item{dadID: Personal ID of the individual's father.}
#'   \item{momID: Personal ID of the individual's mother.}
#'   \item{spID: Personal ID of the individual's mate.}
#'   \item{sex: Biological sex of the individual. F - female; M - male.}
#' }
#' @export
#' @examples
#' set.seed(5)
#' df_ped <- simulatePedigree(
#'   kpc = 4,
#'   Ngen = 4,
#'   sexR = .5,
#'   marR = .7
#' )
#' summary(df_ped)
simulatePedigree <- function(kpc = 3,
                             Ngen = 4,
                             sexR = .5,
                             marR = 2 / 3,
                             rd_kpc = FALSE,
                             balancedSex = TRUE,
                             balancedMar = TRUE,
                             verbose = FALSE,
                             personID = "ID",
                             momID = "momID",
                             dadID = "dadID",
                             spouseID = "spouseID",
                             code_male = "M",
                             code_female = "F",
                             fam_shift = 1L,
                             remap_ids = FALSE,
                             beta = FALSE) {
  # SexRatio: ratio of male over female in the offspring setting; used in the between generation combinations
  # SexRatio <- sexR / (1 - sexR)

  # Calculate the expected family size in each generations
  sizeGens <- allGens(kpc = kpc, Ngen = Ngen, marR = marR)
  #  famSizeIndex <- 1:sum(sizeGens)
  if (verbose == TRUE) {
    message(
      "Step 1: Let's build the connection within each generation first"
    )
  }
  df_Fam <- buildWithinGenerations(
    sizeGens = sizeGens,
    Ngen = Ngen,
    sexR = sexR,
    marR = marR,
    verbose = verbose,
    personID = personID,
    momID = momID,
    dadID = dadID,
    code_male = code_male,
    code_female = code_female,
    fam_shift = fam_shift,
    beta = beta
  )
  if (verbose == TRUE) {
    message(
      "Step 2: Let's try to build connection between each two generations"
    )
  }

  df_Fam <- buildBtwnGenerations(
    df_Fam = df_Fam,
    Ngen = Ngen,
    sizeGens = sizeGens,
    verbose = verbose,
    marR = marR,
    sexR = sexR,
    kpc = kpc,
    rd_kpc = rd_kpc,
    personID = personID,
    momID = momID,
    dadID = dadID,
    code_male = code_male,
    code_female = code_female,
    beta = beta
  )

  df_Fam <- df_Fam[, 1:7]
  df_Fam <- df_Fam[!(is.na(df_Fam$pat) & is.na(df_Fam$mat) & is.na(df_Fam$spID)), ]

  names(df_Fam) <-  c("fam", personID, "gen", dadID, momID, spouseID, "sex")

  # connect the detached members
  df_Fam[is.na(df_Fam[[momID]]) & is.na(df_Fam[[dadID]]) & df_Fam$gen > 1, ]


  if(remap_ids) {
    # Remap all ID columns to sequential integers (1, 2, 3, ...) in row order,
    # so the final data frame has tidy consecutive IDs regardless of fam_shift offsets.
    old_ids <- rbind( df_Fam[[personID]],  df_Fam[[momID]],  df_Fam[[dadID]],  df_Fam[[spouseID]])
    old_ids <- unique(old_ids[!is.na(old_ids)])
    id_map <- setNames(seq_along(old_ids), as.character(old_ids))

    df_Fam[[personID]] <- as.integer(id_map[as.character( df_Fam[[personID]])])
    df_Fam[[momID]] <- as.integer(id_map[as.character( df_Fam[[momID]])])
    df_Fam[[dadID]] <- as.integer(id_map[as.character( df_Fam[[dadID]])])
    df_Fam[[spouseID]] <- as.integer(id_map[as.character( df_Fam[[spouseID]])])
  }
  df_Fam
}

#' Simulate Multiple Pedigrees
#'
#' This function simulates multiple "balanced" pedigrees and returns them
#' combined into a single data frame. It is a convenience wrapper around
#' \code{\link{simulatePedigree}} that makes it easy to simulate many families
#' at once, with unique IDs across all families.
#'
#' @param n_fam Integer. Number of families to simulate. Default is 2.
#' @param remap_ids Logical. If TRUE (default), all ID columns (personID, momID, dadID, spouseID) will be remapped to sequential integers starting at 1 across the combined data frame. This ensures tidy consecutive IDs regardless of fam_shift offsets. If FALSE, IDs will retain their original values from each pedigree simulation, which may include gaps or non-sequential values due to fam_shift.
#' @inheritParams simulatePedigree
#' @return A \code{data.frame} containing all simulated individuals from all
#'   families combined, with the same columns as \code{\link{simulatePedigree}}.
#'   The \code{fam} column uniquely identifies each family (e.g., "fam1",
#'   "fam2", ...). Individual IDs are sequential integers starting at 1
#'   (i.e., \code{1:nrow(result)}), and all parent/spouse ID references are
#'   remapped to match.
#' @export
#' @examples
#' set.seed(5)
#' df_peds <- simulatePedigrees(
#'   n_fam = 3,
#'   kpc = 4,
#'   Ngen = 4,
#'   sexR = .5,
#'   marR = .7
#' )
#' summary(df_peds)
simulatePedigrees <- function(n_fam = 2,
                              kpc = 3,
                              Ngen = 4,
                              sexR = .5,
                              marR = 2 / 3,
                              rd_kpc = FALSE,
                              balancedSex = TRUE,
                              balancedMar = TRUE,
                              verbose = FALSE,
                              personID = "ID",
                              momID = "momID",
                              dadID = "dadID",
                              spouseID = "spouseID",
                              code_male = "M",
                              code_female = "F",
                              remap_ids = TRUE,
                              beta = FALSE
                              ) {
  n_fam <- as.integer(n_fam)
  if (is.na(n_fam) || n_fam < 1L) {
    stop("'n_fam' must be a positive integer.")
  }
  ped_list <- vector("list", n_fam)
  for (i in seq_len(n_fam)) {
    ped_i <- simulatePedigree(
      kpc = kpc,
      Ngen = Ngen,
      sexR = sexR,
      marR = marR,
      rd_kpc = rd_kpc,
      balancedSex = balancedSex,
      balancedMar = balancedMar,
      verbose = verbose,
      personID = personID,
      momID = momID,
      dadID = dadID,
      spouseID = spouseID,
      code_male = code_male,
      code_female = code_female,
      fam_shift = i,
      remap_ids = FALSE, # Keep original IDs for now; we'll remap after combining.
      beta = beta
    )
    ped_i$fam <- paste0("fam", i)
    ped_list[[i]] <- ped_i
  }
  combined <- as.data.frame(data.table::rbindlist(ped_list))
  names(combined) <-  c("fam", personID, "gen", dadID, momID, spouseID, "sex")
if(remap_ids) {
  # Remap all ID columns to sequential integers (1, 2, 3, ...) in row order,
  # so the final data frame has tidy consecutive IDs regardless of fam_shift offsets.
  old_ids <- rbind(combined[[personID]], combined[[momID]], combined[[dadID]], combined[[spouseID]])
  old_ids <- unique(old_ids[!is.na(old_ids)])
  id_map <- setNames(seq_along(old_ids), as.character(old_ids))

  combined[[personID]] <- as.integer(id_map[as.character(combined[[personID]])])
  combined[[momID]] <- as.integer(id_map[as.character(combined[[momID]])])
  combined[[dadID]] <- as.integer(id_map[as.character(combined[[dadID]])])
  combined[[spouseID]] <- as.integer(id_map[as.character(combined[[spouseID]])])
}
  combined
}
