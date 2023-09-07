#' identifyComponentModel
#' Determine if a variance components model is identified
#'
#' @param ... Comma-separated relatedness component matrices representing the variance components of the model.
#' @param silent logical. If TRUE, suppresses messages about identification; FALSE by default.
#' @return A list of length 2 containing:
#'   \itemize{
#'     \item \code{identified}: TRUE if the model is identified, FALSE otherwise.
#'     \item \code{nidp}: A vector of non-identified parameters, specifying the names of components that are not simultaneously identified.
#'   }
#'
#' @export
#'
#' @details
#' This function checks the identification status of a given variance components model
#' by examining the rank of the concatenated matrices of the components.
#' If any components are not identified, their names are returned in the output.
#'
#' @examples
#'
#' identifyComponentModel(A = list(matrix(1, 2, 2)), C = list(matrix(1, 2, 2)), E = diag(1, 2))
#'
identifyComponentModel <- function(..., silent = FALSE) {
  # Collect the relatedness components
  dots <- list(...)
  nam <- names(dots)
  if (is.null(nam)) {
    nam <- paste0("Comp", 1:length(dots))
  }
  # Convert components to vectorized form
  compl <- lapply(dots, comp2vech, include.zeros = TRUE)
  compm <- do.call(cbind, compl)
  rank <- qr(compm)$rank
  if (rank != length(dots)) {
    if (!silent) cat("Component model is not identified.\n")
    jacOC <- Null(t(compm))
    nidp <- nam[apply(jacOC, 1, function(x) {
      sum(x^2)
    }) > 1e-17]
    if (!silent) {
      cat(
        "Non-identified parameters are ",
        paste(nidp, collapse = ", "), "\n"
      )
    }
    return(list(identified = FALSE, nidp = nidp))
  } else {
    if (!silent) cat("Component model is identified.\n")
    return(list(identified = TRUE, nidp = character(0)))
  }
}

#' fitComponentModel
#' Fit the estimated variance components of a model to covariance data
#'
#' @param covmat The covariance matrix of the raw data, which may be blockwise.
#' @param ... Comma-separated relatedness component matrices representing the variance components of the model.
#' @return A regression (linear model fitted with \code{lm}). The coefficients of the regression represent the estimated variance components.
#' @export
#'
#' @details
#' This function fits the estimated variance components of a model to given covariance data.
#' The rank of the component matrices is checked to ensure that the variance components are all identified.
#' Warnings are issued if there are inconsistencies.
#'
#' @examples
#' \dontrun{
#' # install.packages("OpenMX")
#' data(twinData, package = "OpenMx")
#' sellVars <- c("ht1", "ht2")
#' mzData <- subset(twinData, zyg %in% c(1), c(selVars, "zyg"))
#' dzData <- subset(twinData, zyg %in% c(3), c(selVars, "zyg"))
#'
#' fitComponentModel(
#'   covmat = list(cov(mzData[, selVars], use = "pair"), cov(dzData[, selVars], use = "pair")),
#'   A = list(matrix(1, nrow = 2, ncol = 2), matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)),
#'   C = list(matrix(1, nrow = 2, ncol = 2), matrix(1, nrow = 2, ncol = 2)),
#'   E = list(diag(1, nrow = 2), diag(1, nrow = 2))
#' )
#' }
#'
fitComponentModel <- function(covmat, ...) {
  dots <- list(...)
  compl <- lapply(dots, comp2vech, include.zeros = TRUE)
  compm <- do.call(cbind, compl)
  rank <- qr(compm)$rank
  y <- comp2vech(covmat, include.zeros = TRUE)
  if (rank != length(dots)) {
    msg <- paste(
      "Variance components are not all identified.",
      "Try identifyComponentModel()."
    )
    stop(msg)
  }
  if (rank > length(y)) {
    msg <- paste0(
      "Trying to estimate ",
      rank, " variance components when at most ", length(y),
      " are possible with the data given.\n"
    )
    warning(msg)
  }
  stats::lm(y ~ 0 + compm)
}

#' vech
#' Create the half-vectorization of a matrix
#'
#' @param x a matrix, the half-vectorization of which is desired
#' @return A vector containing the lower triangle of the matrix, including the diagonal.
#' @export
#'
#' @details
#' This function returns the vectorized form of the lower triangle of a matrix, including the diagonal.
#' The upper triangle is ignored with no checking that the provided matrix is symmetric.
#'
#' @examples
#'
#' vech(matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2))
#'
vech <- function(x) {
  x[lower.tri(x, diag = TRUE)]
}

#' comp2vech
#' Turn a variance component relatedness matrix into its half-vectorization
#'
#' @param x Relatedness component matrix (can be a matrix, list, or object that inherits from 'Matrix').
#' @param include.zeros logical. Whether to include all-zero rows. Default is FALSE.
#' @export
#' @return The half-vectorization of the relatedness component matrix.
#' @details
#' This function is a wrapper around the \code{vech} function, extending it to allow for blockwise matrices and specific classes.
#' It facilitates the conversion of a variance component relatedness matrix into a half-vectorized form.
#'
#' @examples comp2vech(list(matrix(c(1, .5, .5, 1), 2, 2), matrix(1, 2, 2)))
#'
comp2vech <- function(x, include.zeros = FALSE) {
  if (is.matrix(x)) {
    return(vech(x))
  } else if (is.list(x)) {
    if (include.zeros) {
      return(vech(as.matrix(Matrix::bdiag(x))))
    } else {
      return(do.call(c, lapply(x, vech)))
    }
  } else if (inherits(x, "Matrix")) {
    return(vech(as.matrix(x)))
  } else {
    msg <- paste(
      "Can't make component into a half vectorization:",
      "x is neither a list nor a matrix."
    )
    stop(msg)
  }
}
