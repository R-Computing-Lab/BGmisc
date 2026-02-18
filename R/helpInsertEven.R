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
#' @seealso \code{\link{SimPed}} for the main function that uses this supporting function.

insertEven <- function(m, n, verbose = FALSE) {
  lm <- length(m)
  ln <- length(n)
  if (lm > ln) {
    temp <- m
    m <- n
    n <- temp

    temp <- lm
    lm <- ln
    ln <- temp
    temp <- NULL

    if (isTRUE(verbose)) {
      message("Swapped m and n because length(m) > length(n)")
    }
  }


  if (lm == 0L) {
    return(unname(n))
  }
  if (ln == 0L) {
    return(unname(m))
  }

  pos_m <- ceiling(seq_len(lm) * ln / lm)

  if (isTRUE(verbose)) {
    message("m insertion targets: ", paste(pos_m, collapse = ", "))
    message("n indices: ", paste(seq_len(ln), collapse = ", "))
  }


  vec <- c(m, n)
  primary <- c(pos_m, seq_len(ln))
  secondary <- c(rep.int(0L, lm), rep.int(1L, ln))
  vec <- vec[order(primary, secondary)]

  unname(vec)
}

#' @rdname insertEven
evenInsert <- insertEven
