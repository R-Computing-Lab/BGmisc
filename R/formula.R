#' Calculate Relatedness Coefficient
#'
#' This function calculates the relatedness coefficient between two individuals based on their shared ancestry, as described by Wright (1922).
#'
#' @description
#' The relatedness coefficient between two people (b & c) is defined in relation to their common ancestors:
#' \eqn{r_{bc} = \sum \left(\frac{1}{2}\right)^{n+n'+1} (1+f_a)}
#'
#' @param generations Number of generations back of common ancestors the pair share.
#' @param path Traditional method to count common ancestry, which is twice the number of generations removed from common ancestors. If not provided, it is calculated as 2*generations.
#' @param full Logical. Indicates if the kin share both parents at the common ancestor's generation. Default is TRUE.
#' @param maternal Logical. Indicates if the maternal lineage should be considered in the calculation.
#' @param empirical Logical. Adjusts the coefficient based on empirical data, using the total number of nucleotides and other parameters.
#' @param segregating Logical. Adjusts for segregating genes.
#' @param total_a Numeric. Represents the total size of the autosomal genome in terms of nucleotides, used in empirical adjustment. Default is 6800*1000000.
#' @param total_m Numeric. Represents the total size of the mitochondrial genome in terms of nucleotides, used in empirical adjustment. Default is 16500.
#' @param weight_a Numeric. Represents the weight of phenotypic influence from additive genetic variance, used in empirical adjustment.
#' @param weight_m Numeric. Represents the weight of phenotypic influence from mitochondrial effects, used in empirical adjustment.
#' @param denom_m Logical. Indicates if `total_m` and `weight_m` should be included in the denominator of the empirical adjustment calculation.
#' @param ... Further named arguments that may be passed to another function.
#'
#' @return
#' Relatedness Coefficient (`coef`): A measure of the genetic relationship between two individuals.
#'
#' @examples
#' \dontrun{
#' # For full siblings, the relatedness coefficient is expected to be 0.5:
#' calculateRelatedness(generations = 1, full = TRUE)
#' # For half siblings, the relatedness coefficient is expected to be 0.25:
#' calculateRelatedness(generations = 1, full = FALSE)
#' }
#' @export
#'
# lint-disable
calculateRelatedness <- function(
    generations = 2, path = NULL, full = TRUE, maternal = FALSE,
    empirical = FALSE, segregating = TRUE, total_a = 6800 * 1000000, total_m = 16500,
    weight_a = 1, weight_m = 1, denom_m = FALSE, ...) {
  # If path is not provided, it is calculated as twice the number of generations
  if (is.null(path)) {
    path <- generations * 2
  }

  # Calculate the coefficient based on the path
  coef <- .5^path

  # If full siblings, the coefficient is doubled
  if (full) {
    coef <- coef * 2
  }
  # If not considering segregating genes, adjust the coefficient
  if (!segregating) {
    coef <- coef * .01 + .99
  }

  # If empirical adjustment is needed
  if (empirical) {
    coef <- (coef * total_a * weight_a + maternal * total_m * weight_m) / (denom_m * total_m * weight_m + total_a * weight_a)
  }
  return(coef)
}

#' Infer Relatedness Coefficient
#'
#' Infers the relatedness coefficient between two groups based on the observed correlation between their additive genetic variance and shared environmental variance.
#'
#' @description
#' The function uses the ACE framework to infer the relatedness between two individuals.
#'
#' @param cor_obs Numeric. Observed correlation between the two groups. Must be between -1 and 1.
#' @param ace_A Numeric. Proportion of variance attributable to additive genetic variance. Must be between 0 and 1. Default is 0.9.
#' @param ace_C Numeric. Proportion of variance attributable to shared environmental variance. Must be between 0 and 1. Default is 0.
#' @param shared_C Numeric. Proportion of shared environment shared between the two individuals. Must be between 0 and 1. Default is 0.
#'
#' @return
#' Calculated relatedness coefficient (`est_r`).
#'
#' @examples
#' \dontrun{
#' # Infer the relatedness coefficient:
#' inferRelatedness(cor_obs = 0.5, ace_A = 0.9, ace_C = 0, shared_C = 0)
#' }
#' @export
inferRelatedness <- function(cor_obs, ace_A = .9, ace_C = 0, shared_C = 0) {
  if (ace_A > 1 || ace_A < 0 || ace_C > 1 || ace_C < 0) {
    stop("ace_A and ace_C must be proportions between 0 and 1")
  }
  calc_r <- (cor_obs - shared_C * ace_C) / ace_A
  return(calc_r)
}
