#' Relatedness Coefficient Calculation
#'
#' Relatedness Coefficient Calculation based on Wright (1922)
#'
#' \eqn{r_{bc} = \sum \left(\frac{1}{2}\right)^{n+n'+1} (1+f_a)}
#' where the relatedness coefficient between two people (b & c) is defined in relation to their common ancestors.
#'
#' @param generations A number
#' @param path A number
#' @param both A number
#' @return Relatedness Coefficient \code{coef}
#' @examples
#'
#'
related_coef=function(
  generations=2,
#' Specifies the number of generations back of common ancestors the pair share
  path=NULL,
#' Traditional method to count common ancestry, which is 2 times the number of generations removed from common ancestors
  both=TRUE,
  ...){
  if(is.null(path)){
    path=generations*2
  }
  coef=.5^path
  if(both){
    coef=coef*2
  }
    return(coef)
}
