# ' Function to calculate relatedness coefficient
#'
# ' This function calculates the relatedness coefficient between two individuals based on their shared ancestry. It is based on Wright (1922)
#'
#' \eqn{r_{bc} = \sum \left(\frac{1}{2}\right)^{n+n'+1} (1+f_a)}
#' where the relatedness coefficient between two people (b & c) is defined in relation to their common ancestors.
#'
# ' @param generations The number of generations back of common ancestors the pair share
# ' @param path The traditional method to count common ancestry, which is twice the number of generations removed from common ancestors.
# '   If path is not provided, it is calculated as 2*generations.
# ' @param full Logical parameter indicating whether the kin share both parents at the common ancestor's generation.
# '   Default is TRUE.
# ' @return Relatedness Coefficient, a measure of the genetic relationship between two individuals.= \code{coef}
#' @export
#'
#' @examples
#'
#' # For two full siblings, we would expect a relatedness of 0.5. Using the
#' # default method to count common ancestry, and looking back one generation
#' # (i.e. towards the full siblings' parents), we get a relatedness coefficient
#' # of 0.5:
#' related_coef(generations = 1, path = NULL, full = TRUE)
#' # Similarly, for half siblings, we would expect a relatedness coefficient of 0.25:
#' related_coef(generations = 1, path = NULL, full = FALSE)

related_coef=function(
  generations=2,
# Specifies the number of generations back of common ancestors the pair share
  path=NULL,
# Traditional method to count common ancestry, which is 2 times the number of generations removed from common ancestors
  full=TRUE,
  maternal=FALSE,
  empirical=FALSE, # adjust proportion
  segregating=TRUE, # adjust for segregating genes
  total_a=6800*1000000,
  total_m=16500,
  weight_a=1,   # weight phenotypic influence
  weight_m=1,   # weight phenotypic influence
  denom_m=FALSE,
  ...)){
	
  # If path is not provided, it is calculated as twice the number of generations
  if(is.null(path)){
    path=generations*2
  }
	
  # Calculate the coefficient based on the path
  coef <- .5^path
	
  # If full siblings, the coefficient is doubled
  if(full){
    coef <- coef*2
  }
 # If not considering segregating genes, adjust the coefficient
if(!segregating){  
  coef <- coef*.01+.99
}

  # If empirical adjustment is needed
if(empirical){
    return(
      (coef*total_a*weight_a+maternal*total_m*weight_m)/(denom_m*total_m*weight_m+total_a*weight_a)
    )
}else{
  return(coef)
  }
}

# ' Function to estimate relatedness based on the observed correlation
#'
# ' This function estimates the relatedness between two individuals based on the observed correlation between their additive genetic variance
#'   and shared environmental variance.
#'   
#' @param cor_obs Observed Correlation
#' @param ace_A proportion of variance attributable to additive genetic variance
#' @param ace_C proportion of variance attributable to shared environmental
#'   variance
#' @param shared_c proportion of shared environment shared. Typically takes zero
#'   or 1.
#' @return estimated relatedness Coefficient  \code{est_r}
#' @export
#'
#' @examples
#' # Using the ACE framework, we can estimate the relatedness between two
#' # individuals based on the observed correlation between their additive genetic
#' # variance, shared environmental variance, and proportion of shared environment.
#'
#' relatedness(cor_obs = 0.5, ace_A = 0.9, ace_C = 0, shared_c = 0)

relatedness <- function(cor_obs, ace_A=.9,ace_C=0, shared_c=0 ){
if(ace_A>1|
   ace_A<0|
   ace_C>1|
   ace_C<0){
	stop("ace_A and ace_C must be proportions")}
  est_r=(cor_obs-shared_c*ace_C)/ace_A
  return(est_r)
}
