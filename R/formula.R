#' Calculate Relatedness Coefficient
#'
#' @param generations A number
#' @param path A number
#' @param both A number
#' @return The sum of \code{x} and \code{y}
#' @examples
#' add(1, 1)
#' add(10, 1)
related_coef=function(
  generations=2,
  path=NULL,
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
