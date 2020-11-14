require(Matrix)

##' Determine if a variance components model is identified
##'
##' @param ... Comma-separated relatedness component matrices.
##' @param silent logical. Whether to print messages about identification.
##'
##' @details
##' Returns of list of length 2. The first element is a single logical value:
##' TRUE if the model is identified, FALSE otherwise. The second list element
##' is the vector of non-identified parameters.  For instance, a model might
##' have 5 components with 3 of them identified and 2 of them not.  The second
##' list element will give the names of the components that are not
##' simultaneously identified.
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

##' Fit the estimated variance components of a model to covariance data
##'
##' @param covmat the covariance matrix of the raw data, possibly blockwise.
##' @param ... Comma-separated relatedness component matrices.
##'
##' @details
##' Returns a regression (linear model fitted with \code{lm}).
##' The coefficients of the regression are the estimated variance components.
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

##' Create the half-vectorization of a matrix
##'
##' @param x a matrix, the half-vectorization of which is desired
##'
##' @details
##' Returns the vector of the lower triangle of a matrix, including the diagonal.
##' The upper triangle is ignored with no checking that the provided matrix
##' is symmetric.
vech <- function(x){
	x[lower.tri(x, diag=TRUE)]
}

##' Turn a variance component relatedness matrix into its half-vectorization
##'
##' @param x relatedness component matrix
##' @param include.zeros logical. Whether to include all-zero rows.
##'
##' @details
##' This is a wrapper around the \code{vech} function for producing the
##' half-vectorization of a matrix.  The extension here is to allow for
##' blockwise matrices.
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

##' Compute the null space of a matrix
##'
##' @param M a matrix of which the null space is desired
##'
##' @details
##' The method uses the QR factorization to determine a basis for the null
##' space of a matrix.  This is sometimes also called the orthogonal
##' complement of a matrix.  As implemented, this function is identical
##' to the function of the same name in the MASS package.
Null <- function (M) {
	tmp <- qr(M)
	set <- if (tmp$rank == 0L) {
		seq_len(ncol(M))
	} else {
		-seq_len(tmp$rank)
	}
	return(qr.Q(tmp, complete = TRUE)[, set, drop = FALSE])
}





