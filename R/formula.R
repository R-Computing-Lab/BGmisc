#' Relatedness Coefficient Calculation
#'
#' Relatedness Coefficient Calculation based on Wright (1922)
#'
#' \eqn{r_{bc} = \sum \left(\frac{1}{2}\right)^{n+n'+1} (1+f_a)}
#' where the relatedness coefficient between two people (b & c) is defined in relation to their common ancestors.
#'
#' @param generations Specifies the number of generations back of common ancestors the pair share
#' @param path A Traditional method to count common ancestry, which is 2 times the number of generations removed from common ancestors
#' @param full Full or half kin. Do the kin share both parents at the common ancestor's generation?
#' @return Relatedness Coefficient \code{coef}

related_coef=function(
  generations=2,
# Specifies the number of generations back of common ancestors the pair share
  path=NULL,
# Traditional method to count common ancestry, which is 2 times the number of generations removed from common ancestors
  full=TRUE){
  if(is.null(path)){
    path=generations*2
  }
  coef=.5^path
  if(full){
    coef=coef*2
  }
    return(coef)
}

#' Estimate Relatedness based on Observed Correlation
#'
#'
#'
#' @param cor_obs Observed Correlation
#' @param ace_A proportion of variance attributable to additive genetic variance
#' @param ace_C proportion of variance attributable to shared environmental variance
#' @param shared_c proportion of shared environment shared. Typically takes zero or 1.
#' @return estimated relatedness Coefficient  \code{est_r}

relatedness <- function(cor_obs, ace_A=.9,ace_C=0, shared_c=0 ){
if(ace_A>1|ace_A<0|ace_C>1|ace_C<0){
	stop("ace_A and ace_C must be proportions")}
  est_r=(cor_obs-shared_c*ace_C)/ace_A
  return(est_r)
}
