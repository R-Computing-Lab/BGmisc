#' Calculate Confidence Intervals for Correlation Coefficients
#'
#' This function calculates confidence intervals for correlation coefficients using different methods.
#'
#' @param tbl A data frame or tibble containing the correlation coefficient and standard error variables.
#' @param rho_var The name of the column in \code{tbl} that contains the correlation coefficients.
#' @param se_var The name of the column in \code{tbl} that contains the standard errors.
#' @param method The method to use for calculating the confidence intervals. Options are "raykov", "fisherz", "doubleenteredconserv", or "doubleentered".
#' @param conf_level The confidence level for the intervals. Default is 0.95.
#'
#' @return A modified version of \code{tbl} with additional columns for the confidence intervals and related statistics.
#'
#' @examples
#' tbl <- data.frame(rho = c(0.5, 0.7, 0.3), se = c(0.1, 0.2, 0.05))
#' calculateCIs(tbl, rho_var = "rho", se_var = "se", method = "raykov")
#'
#' @export
calculateCIs <- function(tbl, rho_var, se_var, method = "raykov", conf_level = 0.95) {
  # Load necessary packages
  require(rlang)


  # Get the name of the rho column regardless of input type
 if(mode(rho_var) != "character") {
    stop("method must be a character string")
  }
  # Convert the rho_var and se_var into column names
  rho_col_name <- if (is_string(rho_var)) rho_var else deparse(substitute(rho_var))
  se_col_name <- if (is_string(se_var)) se_var else deparse(substitute(se_var))

  # Construct new column names using paste0
  plusse_col_name <- paste0(rho_col_name, "_plusse")
  minusse_col_name <- paste0(rho_col_name, "_minusse")
  z_col_name <- paste0(rho_col_name, "_z")
  ztest_col_name <- paste0(rho_col_name, "_ztest")
  p1tail_col_name <- paste0(rho_col_name, "_p1tail")
  p2tail_col_name <- paste0(rho_col_name, "_p2tail")
  sez_col_name <- paste0(se_col_name, "_sez")

  # Create a copy of tbl to avoid modifying the original data frame
  tbl_out <- tbl

  if (method == "raykov") {
    # Apply Fisher's r to z transform
    z_vals <- psych::fisherz(tbl_out[[rho_col_name]])
    se_vals <- tbl_out[[se_col_name]] / (1 - tbl_out[[rho_col_name]]^2)

    tbl_out[[z_col_name]] <- z_vals
    tbl_out[[sez_col_name]] <- se_vals
    tbl_out[[plusse_col_name]] <- psych::fisherz2r(z_vals + qnorm((1 + conf_level) / 2) * se_vals)
    tbl_out[[minusse_col_name]] <- psych::fisherz2r(z_vals - qnorm((1 + conf_level) / 2) * se_vals)

    # H0: r = 0. Compute z-test and p-values
    ztest_vals <- tbl_out[[rho_col_name]] / tbl_out[[se_col_name]]
    tbl_out[[ztest_col_name]] <- ztest_vals
    tbl_out[[p1tail_col_name]] <- pnorm(q = abs(ztest_vals), lower.tail = FALSE)
    tbl_out[[p2tail_col_name]] <- 2 * tbl_out[[p1tail_col_name]]

  } else {
    # Set the multiplier for CI calculations based on the method
    ci_multi <- switch(method,
                       "fisherz" = 1.96,  # 95% CI, can generalize to other levels
                       "doubleenteredconserv" = sqrt(2) * 1.96,
                       "doubleentered" = sqrt(2),
                       stop("method must be one of 'fisherz', 'doubleenteredconserv', or 'doubleentered'"))

    # Compute confidence intervals
    tbl_out[[plusse_col_name]] <- tbl_out[[rho_col_name]] + ci_multi * tbl_out[[se_col_name]]
    tbl_out[[minusse_col_name]] <- tbl_out[[rho_col_name]] - ci_multi * tbl_out[[se_col_name]]
  }

  return(tbl_out)
}

