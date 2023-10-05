#' evenInsert
#' A function to insert m elements evenly into a length n vector.
#'
#' @param m A numeric vector of length less than or equal to n. The elements to be inserted.
#' @param n A numeric vector. The vector into which the elements of m will be inserted.
#' @param verbose logical  If TRUE, prints additional information. Default is FALSE.
#' @return Returns a numeric vector with the elements of m evenly inserted into n.
#' @details
#' The function takes two vectors, m and n, and inserts the elements of m evenly into n.
#' If the length of m is greater than the length of n, the vectors are swapped, and the insertion proceeds.
#' The resulting vector is a combination of m and n, with the elements of m evenly distributed within n.
#' @export
#' @seealso \code{\link{SimPed}} for the main function that uses this supporting function.

evenInsert <- function(m, n, verbose = FALSE) {
  if (length(m) > length(n)) {
    temp <- m
    m <- n
    n <- temp
  }

  # idx <- numeric()
  for (i in 1:length(m)) {
    names(m)[i] <- ceiling(i * length(n) / length(m))
  }
  if (verbose) {
    print(m)
  }
  names(n) <- 1:length(n)
  if (verbose) {
    print(n)
  }
  vec <- c(m, n)
  vec <- vec[order(as.numeric(names(vec)))]
  vec <- unname(vec)

  return(vec)
}
