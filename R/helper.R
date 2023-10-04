#' Error Function
#'
#' @param error error output
#' @keywords internal
#' @return Replaces error message (\code{error}) with NA
#'
efunc <- function(error) {
  return(NA)
}

#' rmvn
#' @keywords internal
#' @param n Sample Size
#' @param sigma Covariance matrix
#' @return Generates multivariate normal data from a covariance matrix (\code{sigma}) of length \code{n}
#'
rmvn <- function(n, sigma) {
  Sh <- with(svd(sigma), v %*% diag(sqrt(d)) %*% t(u))
  matrix(stats::rnorm(ncol(sigma) * n), ncol = ncol(sigma)) %*% Sh
}

#' nullToNA
#' @keywords internal
#' @param x vector of any length
#' @return replaces null values in a vector to NA
#'
nullToNA <- function(x) {
  if (length(x) == 0) {
    x <- NA
  } else {
    x[sapply(x, is.null)] <- NA
  }
  return(x)
}

#' modified tryCatch function
#'
#' @param x vector of any length
#' @keywords internal
#' @return Fuses the nullToNA function with efunc
#'
try_na <- function(x) {
  nullToNA(tryCatch(x, error = efunc))
}

#' Compute the null space of a matrix
#'
#' @param M a matrix of which the null space is desired
#' @keywords internal
#'
#' @details
#' The method uses the QR factorization to determine a basis for the null
#' space of a matrix.  This is sometimes also called the orthogonal
#' complement of a matrix.  As implemented, this function is identical
#' to the function of the same name in the MASS package.
#'
Null <- function(M) {
  tmp <- qr(M)
  set <- if (tmp$rank == 0L) {
    seq_len(ncol(M))
  } else {
    -seq_len(tmp$rank)
  }
  return(qr.Q(tmp, complete = TRUE)[, set, drop = FALSE])
}

#' SimPed (Deprecated)
#'
#' This function is a wrapper around the new `simulatePedigree` function.
#' `SimPed` has been deprecated, and it's advised to use `simulatePedigree` directly.
#'
#' @param ... Arguments to be passed to `simulatePedigree`.
#' @return The same result as calling `simulatePedigree`.
#' @seealso \code{\link{simulatePedigree}} for the updated function.
#' @description When calling this function, a warning will be issued about its deprecation.
#' @keywords deprecated
#' @examples
#' \dontrun{
#' # This is an example of the deprecated function:
#' SimPed(...)
#' # It is recommended to use:
#' simulatePedigree(...)
#' }
#' @export
SimPed <- function(...) {
  warning("The 'SimPed' function is deprecated. Please use 'simulatePedigree' instead.")
  simulatePedigree(...)
}

#' related_coef (Deprecated)
#'
#' This function is a wrapper around the new `calculateRelatedness` function.
#' `related_coef` has been deprecated, and it's advised to use `calculateRelatedness` directly.
#'
#' @param ... Arguments to be passed to `calculateRelatedness`.
#' @return The same result as calling `calculateRelatedness`.
#' @seealso \code{\link{calculateRelatedness}} for the updated function.
#' @description When calling this function, a warning will be issued about its deprecation.
#' @keywords deprecated
#' @examples
#' \dontrun{
#' # This is an example of the deprecated function:
#' related_coef(...)
#' # It is recommended to use:
#' calculateRelatedness(...)
#' }
#' @export
related_coef <- function(...) {
  warning("The 'related_coef' function is deprecated. Please use 'calculateRelatedness' instead.")
  calculateRelatedness(...)
}

#' relatedness (Deprecated)
#'
#' This function is a wrapper around the new `inferRelatedness` function.
#' `relatedness` has been deprecated, and it's advised to use `inferRelatedness` directly.
#'
#' @param ... Arguments to be passed to `inferRelatedness`.
#' @return The same result as calling `inferRelatedness`.
#' @seealso \code{\link{inferRelatedness}} for the updated function.
#' @description When calling this function, a warning will be issued about its deprecation.
#' @keywords deprecated
#' @examples
#' \dontrun{
#' # This is an example of the deprecated function:
#' relatedness(...)
#' # It is recommended to use:
#' inferRelatedness(...)
#' }
#' @export
relatedness <- function(...) {
  warning("The 'relatedness' function is deprecated. Please use 'inferRelatedness' instead.")
  inferRelatedness(...)
}
