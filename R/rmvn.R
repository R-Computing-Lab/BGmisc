#' rmvn
#' @keywords internal
#' @param n Sample Size
#' @param sigma Covariance matrix
#' @return Generates multivariate normal data from a covariance matrix (\code{sigma}) of length \code{n}


rmvn <- function(n,sigma) {
  Sh <- with(svd(sigma), v%*%diag(sqrt(d))%*%t(u))
  matrix(stats::rnorm(ncol(sigma)*n),ncol=ncol(sigma))%*%Sh
}
