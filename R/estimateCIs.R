#' Calculate Confidence Intervals for Correlation Coefficients
#'
#' This function calculates confidence intervals for correlation coefficients using different methods.
#'
#' @param tbl A data frame or tibble containing the correlation coefficient and standard error variables.
#' @param rho_var The name of the column in \code{tbl} that contains the correlation coefficients.
#' @param se_var The name of the column in \code{tbl} that contains the standard errors.
#' @param doubleentered Logical. If \code{TRUE}, the function assumes that the correlation coefficients are double-entered, which adjusts the standard errors accordingly. Default is \code{FALSE}.
#' @param method The method to use for calculating the confidence intervals. Options are "raykov", "fisherz", "doubleenteredconserv", or "doubleentered".
#' @param conf_level The confidence level for the intervals. Default is 0.95.
#' @param adjust_base A numeric value to adjust the standard errors. Default is 1.
#' @param design_effect_m A numeric value for the design effect related to the mean. Default is \code{NULL}.
#' @param design_effect_rho A numeric value for the design effect related to the correlation. Default is \code{NULL}.
#' @param design_effect_m_col A character string specifying the column name for the design effect related to the mean. Default is \code{NULL}.
#' @param design_effect_rho_col A character string specifying the column name for the design effect related to the correlation. Default is \code{NULL}.
#' @return A modified version of \code{tbl} with additional columns for the confidence intervals and related statistics.
#' Everything uses adjusted standard errors, including confidence intervals, z-tests, and p-values.
#' @examples
#' tbl <- data.frame(rho = c(0.5, 0.7, 0.3), se = c(0.1, 0.2, 0.05))
#' calculateCIs(tbl, rho_var = "rho", se_var = "se", method = "raykov")
#'
#' @export
#' @importFrom stats qnorm pnorm

calculateCIs <- function(tbl,
                         rho_var,
                         se_var,
                         doubleentered = FALSE,
                         method = "raykov",
                         adjust_base = 1,
                         design_effect_m = NULL,
                         design_effect_rho = NULL,
                         design_effect_m_col = NULL,
                         design_effect_rho_col = NULL,
                         conf_level = 0.95) {
  # Load necessary packages

  # Get the name of the rho column regardless of input type
  if (mode(rho_var) != "character") {
    stop("rho_var must be a character string")
  }
  # Convert the rho_var and se_var into column names
  rho_col_name <- if (is.character(rho_var) && length(rho_var) == 1) {
    rho_var
  } else {
    deparse(substitute(rho_var))
  }

  se_col_name <- if (is.character(se_var) && length(se_var) == 1) {
    se_var
  } else {
    deparse(substitute(se_var))
  }


  # Construct new column names using paste0
  plusse_col_name <- paste0(rho_col_name, "_plusse")
  minusse_col_name <- paste0(rho_col_name, "_minusse")
  z_col_name <- paste0(rho_col_name, "_z")
  wald_col_name <- paste0(rho_col_name, "_wald")
  ztest_col_name <- paste0(rho_col_name, "_ztest")
  zp1tail_col_name <- paste0(rho_col_name, "_zp1tail")
  zp2tail_col_name <- paste0(rho_col_name, "_zp2tail")
  sez_col_name <- paste0(se_col_name, "_sez")
  se_adjusted_col_name <- paste0(se_col_name, "_se_adjusted")
  sez_adjusted_col_name <- paste0(se_col_name, "_sez_adjusted")
  waldp1tail_col_name <- paste0(rho_col_name, "_waldp1tail")
  waldp2tail_col_name <- paste0(rho_col_name, "_waldp2tail")

  # Create a copy of tbl to avoid modifying the original data frame
  tbl_out <- tbl
  n_rows <- nrow(tbl_out)


  # Resolve design effect vector
  if (!is.null(design_effect_m_col) && !is.null(design_effect_rho_col)) {
    m_vals <- tbl_out[[design_effect_m_col]]
    rho_vals <- tbl_out[[design_effect_rho_col]]
    design_effect <- sqrt(1 + (m_vals - 1) * rho_vals)
  } else if (doubleentered == TRUE && is.null(design_effect_m) && is.null(design_effect_rho)) {
    design_effect <- rep(sqrt(1 + (2 - 1) * 1), n_rows)
  } else if (!is.null(design_effect_m) && !is.null(design_effect_rho)) {
    design_effect <- rep(sqrt(1 + (design_effect_m - 1) * design_effect_rho), n_rows)
  } else {
    design_effect <- rep(adjust_base, n_rows)
  }

  z_crit <- stats::qnorm((1 + conf_level) / 2)

  if (method == "raykov") {
    # Apply Fisher's r to z transform
    z_vals <- .fisherz(tbl_out[[rho_col_name]])
    sez_vals <- tbl_out[[se_col_name]] / (1 - tbl_out[[rho_col_name]]^2)

    tbl_out[[z_col_name]] <- z_vals
    tbl_out[[sez_col_name]] <- sez_vals
    tbl_out[[se_adjusted_col_name]] <- tbl_out[[se_col_name]] * design_effect
    tbl_out[[sez_adjusted_col_name]] <- tbl_out[[se_adjusted_col_name]] / (1 - tbl_out[[rho_col_name]]^2)
    tbl_out[[plusse_col_name]] <- .fisherz2r(z_vals + z_crit * tbl_out[[sez_adjusted_col_name]])
    tbl_out[[minusse_col_name]] <- .fisherz2r(z_vals - z_crit * tbl_out[[sez_adjusted_col_name]])

    # H0: r = 0. Compute z-test and p-values
    ztest_vals <- tbl_out[[z_col_name]] / tbl_out[[sez_adjusted_col_name]]

    tbl_out[[ztest_col_name]] <- ztest_vals
    tbl_out[[zp1tail_col_name]] <- stats::pnorm(q = abs(ztest_vals), lower.tail = FALSE)
    tbl_out[[zp2tail_col_name]] <- 2 * tbl_out[[zp1tail_col_name]]
  } else {
    tbl_out[[se_adjusted_col_name]] <- tbl_out[[se_col_name]] * design_effect

    # Compute confidence intervals
    tbl_out[[plusse_col_name]] <- tbl_out[[rho_col_name]] + z_crit * tbl_out[[se_adjusted_col_name]]
    tbl_out[[minusse_col_name]] <- tbl_out[[rho_col_name]] - z_crit * tbl_out[[se_adjusted_col_name]]
  }
  # Compute Wald statistics
  tbl_out[[wald_col_name]] <- tbl_out[[rho_col_name]] / tbl_out[[se_adjusted_col_name]]
  tbl_out[[waldp1tail_col_name]] <- stats::pnorm(q = abs(tbl_out[[wald_col_name]]), lower.tail = FALSE)
  tbl_out[[waldp2tail_col_name]] <- 2 * tbl_out[[waldp1tail_col_name]]


  return(tbl_out)
}
#' Fisher's r to z transformation and back
#'
#' These functions convert correlation coefficients (r) to Fisher's z-scores and back.
#' @param rho A numeric vector of correlation coefficients.
#' @return A numeric vector of transformed values.
#' @keywords internal
#' @details
#' Credit to the psych package for the Fisher's r to z transformation.

.fisherz <- function(rho) {
  0.5 * log((1 + rho) / (1 - rho))
} # converts r to z


#' @inherit .fisherz title
#' @inherit .fisherz return
#' @param z A numeric vector of Fisher's z-scores.
#' @keywords internal
#'
.fisherz2r <- function(z) {
  (exp(2 * z) - 1) / (1 + exp(2 * z))
} # converts back again
