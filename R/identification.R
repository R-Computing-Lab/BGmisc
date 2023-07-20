require(Matrix)

#' Determine if a variance components model is identified
#'
#' @param ... Comma-separated relatedness component matrices.
#' @param silent logical. Whether to print messages about identification.
#' @export
#'
#' @details
#' Returns of list of length 2. The first element is a single logical value:
#' TRUE if the model is identified, FALSE otherwise. The second list element
#' is the vector of non-identified parameters.  For instance, a model might
#' have 5 components with 3 of them identified and 2 of them not.  The second
#' list element will give the names of the components that are not
#' simultaneously identified.
#'
#' @examples
#'
#' identifyComponentModel(A=list(matrix(1, 2, 2)), C=list(matrix(1, 2, 2)), E= diag(1, 2))
#'
identifyComponentModel <- function(..., silent=FALSE){
	dots <- list(...)
	nam <- names(dots)
	if(is.null(nam)){
		nam <- paste0('Comp', 1:length(dots))
	}
	compl <- lapply(dots, comp2vech, include.zeros=TRUE)
	compm <- do.call(cbind, compl)
	rank <- qr(compm)$rank
	if(rank != length(dots)){
		if(!silent) cat("Component model is not identified.\n")
		jacOC <- Null(t(compm))
		nidp <- nam[apply(jacOC, 1, function(x){sum(x^2)}) > 1e-17]
		if(!silent) {
			cat("Non-identified parameters are ",
				paste(nidp, collapse=", "), "\n")
		}
		return(list(identified=FALSE, nidp=nidp))
	} else{
		if(!silent) cat("Component model is identified.\n")
		return(list(identified=TRUE, nidp=character(0)))
	}
}

#' Fit the estimated variance components of a model to covariance data
#'
#' @param covmat the covariance matrix of the raw data, possibly blockwise.
#' @param ... Comma-separated relatedness component matrices.
#' @export
#'
#' @details
#' Returns a regression (linear model fitted with \code{lm}).
#' The coefficients of the regression are the estimated variance components.
#'
#' @examples
#'
#' \dontrun{
#' # install.packages("OpenMX")
#' data(twinData, package = "OpenMx")
#' sellVars <- c("ht1", "ht2")
#' mzData <- subset(twinData, zyg %in% c(1), c(selVars, 'zyg'))
#' dzData <- subset(twinData, zyg %in% c(3), c(selVars, 'zyg'))
#'
#' fitComponentModel(
#' covmat = list(cov(mzData[,selVars], use = "pair"), cov(dzData[,selVars], use = "pair")),
#' A = list(matrix(1, nrow = 2, ncol = 2), matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)),
#' C = list(matrix(1, nrow = 2, ncol = 2), matrix(1, nrow = 2, ncol = 2)),
#' E = list(diag(1, nrow = 2), diag(1, nrow = 2))
#' )
#' }
#'
fitComponentModel <- function(covmat, ...){
	dots <- list(...)
	compl <- lapply(dots, comp2vech, include.zeros=TRUE)
	compm <- do.call(cbind, compl)
	rank <- qr(compm)$rank
	y <- comp2vech(covmat, include.zeros=TRUE)
	if(rank != length(dots)){
		msg <- paste("Variance components are not all identified.",
			"Try identifyComponentModel().")
		stop(msg)
	}
	if(rank > length(y)){
		msg <- paste0("Trying to estimate ",
			rank, " variance components when at most ", length(y),
			" are possible with the data given.\n")
		warning(msg)
	}
	stats::lm(y ~ 0 + compm)
}

#' Create the half-vectorization of a matrix
#'
#' @param x a matrix, the half-vectorization of which is desired
#' @export
#'
#' @details
#' Returns the vector of the lower triangle of a matrix, including the diagonal.
#' The upper triangle is ignored with no checking that the provided matrix
#' is symmetric.
#'
#' @examples
#'
#' vech(matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2))
#'
vech <- function(x){
	x[lower.tri(x, diag=TRUE)]
}

#' Turn a variance component relatedness matrix into its half-vectorization
#'
#' @param x relatedness component matrix
#' @param include.zeros logical. Whether to include all-zero rows.
#' @export
#'
#' @details
#' This is a wrapper around the \code{vech} function for producing the
#' half-vectorization of a matrix.  The extension here is to allow for
#' blockwise matrices.
#'
#' @examples comp2vech(list(matrix(c(1, .5, .5, 1), 2, 2), matrix(1, 2, 2)))
#'
comp2vech <- function(x, include.zeros=FALSE){
	if(is.matrix(x)){
		return(vech(x))
	}else if(is.list(x)){
		if(include.zeros){
			return(vech(as.matrix(Matrix::bdiag(x))))
		} else {
			return(do.call(c, lapply(x, vech)))
		}
	} else if(inherits(x, 'Matrix')) {
		return(vech(as.matrix(x)))
	} else {
		msg <- paste("Can't make component into a half vectorization:",
			"x is neither a list nor a matrix.")
		stop(msg)
	}
}

