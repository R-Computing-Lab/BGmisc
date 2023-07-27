#' evenInsert
#' A function to insert m elements in a length n vector evenly. This is a supporting function for function \code{SimPed}
#' @param m a vector
#' @param n a vector
#' @return Returns the inserted vector.
#' @export
evenInsert <- function(m,n){
      if (length(m) > length(n)){
            temp <- m
            m <- n
            n <- temp
      }

      #idx <- numeric()
      for (i in 1:length(m)){
            names(m)[i] <- ceiling(i*length(n)/length(m))
      }
      #print(m)

      names(n) <- 1:length(n)
      #print(n)

      vec <- c(m,n)
      vec <- vec[order(as.numeric(names(vec)))]
      vec <- unname(vec)

      return(vec)
}

