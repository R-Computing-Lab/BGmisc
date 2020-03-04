#' Calculate Relatedness Coefficient
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
