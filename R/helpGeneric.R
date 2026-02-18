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
  sH <- with(svd(sigma), v %*% diag(sqrt(d)) %*% t(u))
  matrix(stats::rnorm(ncol(sigma) * n), ncol = ncol(sigma)) %*% sH
}

#' nullToNA
#' @keywords internal
#' @param x vector of any length
#' @return replaces null values in a vector to NA
#'
null2NA <- function(x) {
  if (length(x) == 0) {
    x <- NA
    # Handle case when x is a list
  } else if (is.list(x)) {
    for (i in seq_along(x)) {
      if (is.null(x[[i]])) {
        x[[i]] <- NA
      }
    }
  }
  return(x)
}

#' @rdname null2NA
#'
nullToNA <- null2NA

#' modified tryCatch function
#'
#' @param x vector of any length
#' @keywords internal
#' @return Fuses the nullToNA function with efunc
#'
tryNA <- function(x) {
  null2NA(tryCatch(x, error = efunc))
}
#' @rdname tryNA
#' @keywords internal

#'
try_na <- tryNA
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


#' Resample Elements of a Vector
#'
#' This function performs resampling of the elements in a vector `x`. It randomly
#' shuffles the elements of `x` and returns a vector of the resampled elements. If `x`
#' is empty, it returns `NA_integer_`.
#'
#' @param x A vector containing the elements to be resampled. If `x` is empty, the
#' function will return `NA_integer_`.
#' @param ... Additional arguments passed to `sample.int`, such as `size` for the
#' number of items to sample and `replace` indicating whether sampling should be with
#' replacement.
#'
#' @return A vector of resampled elements from `x`. If `x` is empty, returns
#' `NA_integer_`. The length and type of the returned vector depend on the input
#' vector `x` and the additional arguments provided via `...`.
#'
#'
#' @keywords internal
resample <- function(x, ...) {
  # message(length(x))
  if (length(x) == 0) {
    return(NA_integer_)
  }
  x[sample.int(length(x), ...)]
}
